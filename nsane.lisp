(defpackage #:nsane
  (:use :cl)
  (:shadow cl:write-string)
  (:export #:+status-codes+
	   ;; device
	   #:device
	   #:device-name
	   #:device-vendor
	   #:device-model
	   #:device-type
	   ;; option
	   #:option-name
	   #:option-title
	   #:option-description
	   #:option-type
	   #:option-unit
	   #:option-size
	   #:option-cap
	   #:option-constraint-type
	   #:option-constraint
	   ;;scan format
	   #:scan-last-frame-p
	   #:scan-bytes-per-line
	   #:scan-pixels-per-line
	   #:scan-lines
	   #:scan-depth
	   ;; protocol
	   #:init
	   #:get-devices
	   #:openc
	   #:closec
	   #:get-option-descriptors
	   #:control-option
	   #:get-parameters
	   #:start
	   #:cancel
	   #:authorize
	   #:exit))
(in-package #:nsane)

(defparameter +rpc-codes+
  #(:init
    :get-devices
    :openc
    :closec
    :get-option-descriptors
    :control-option
    :get-parameters
    :start
    :cancel
    :authorize
    :exit))

;;remove prepended status? don't copy api and use description?
(defparameter +status-codes+
  #(:status-good
    :status-unsuported
    :status-cancelled
    :status-device-busy
    :status-inval
    :status-eof
    :status-jammed
    :status-no-docs
    :status-cover-open
    :status-io-error
    :status-no-mem
    :status-access-denied))

(defparameter +value-type+
  #(:boolean
    :integer
    :fixed
    :string
    :button
    :group))

(defparameter +value-unit+
  #(:none
    :pixel
    :bit
    :mm
    :dpi
    :percent
    :microsecond))

(defparameter +capabilities+
  #(:soft-select
    :hard-select
    :soft-detect
    :emulated
    :automatic
    :inactive
    :advanced))

(defparameter +format+
  #(:gray
    :rgb
    :red
    :green
    :blue))

(defparameter +info+
  #(:inexact
    :reload-options
    :reload-parameters))



(declaim (optimize debug))
(defstruct device
  name
  vendor
  model
  type)

(defstruct (option (:constructor option-descriptor (name title description type unit size cap constraint-type constraint)))
  name
  title
  description
  type
  unit
  size
  cap
  constraint-type
  constraint)

(defstruct (scan (:constructor parameters (format last-frame-p bytes-per-line pixels-per-line lines depth)))
  format
  last-frame-p
  bytes-per-line
  pixels-per-line
  lines
  depth)

(defvar *socket* )
(defvar *stream* )

;;;; Accessors

(defun read-word (stream)
  (+ (ash (read-byte stream) 24)
     (ash (read-byte stream) 16)
     (ash (read-byte stream) 8)
     (read-byte stream)))
(defun write-word (word stream)
  (write-byte (ldb (byte 8 24) word) stream)
  (write-byte (ldb (byte 8 16) word) stream)
  (write-byte (ldb (byte 8 8) word) stream)
  (write-byte (ldb (byte 8 0) word) stream))

(defun read-fixed (stream)
  (let ((fixed (read-word stream)))
    (+ (ldb (byte 16 0) fixed) (/ (ldb (byte 16 16) fixed) #xFFFF))))
(defun write-fixed (real stream)
  (multiple-value-bind (int decimal) (truncate real)
    (write-word (logior int (ash (round (* decimal #xFFFF )) 16)) stream)))

(defun read-string (stream)
  (let* ((length (u:fp (read-word stream)))
	 (array (u:fp(make-array length :element-type '(unsigned-byte 8)))))
    (read-sequence array stream)
    (map 'string #'code-char (remove 0 array))))
(defun write-string (string stream)
  (write-sequence (sane-string string) stream))

(defun read-boolean (stream)
  (if (zerop (read-word stream)) nil t))
(defun write-boolean (bool stream)
  (write-word (if bool 1 0) stream))

(defun read-byte-order (stream)
  (case (read-word stream)
    (#x1234 :little-endian)
    (#x4321 :big-endian)
    (t :unknown-format)))

(defun decode-mask (mask mask-vector)
  (loop :for i :from 0 :below (integer-length mask)
	:when (= 1 (ldb (byte 1 i) mask))
	  :collect (aref mask-vector i)))

(defun read-device (stream)
  (make-device
   :name (read-string stream)
   :vendor (read-string stream)
   :model (read-string stream)
   :type (read-string stream)))

(defun read-option-descriptor (stream)
  (let* ((name (u:fp (read-string stream)))
	 (title  (u:fp (read-string stream)))
	 (description  (u:fp (read-string stream)))
	 (type  (u:fp(aref +value-type+ (read-word stream))))
	 (unit  (u:fp(aref +value-unit+ (read-word stream))))
	 (size  (u:fp (read-word stream)))
	 (cap   (decode-mask (read-word stream) +capabilities+))
	 (constraint-type  (u:fp (read-word stream)))
	 (constraint  (case constraint-type
			(0 nil)
			(1 (list (read-word stream)(read-word stream)(read-word stream) (read-word stream)))
			(2 (let  ((length (read-word stream)))
			     (loop :repeat length :collect (u:fp (read-word stream)))))
			(3 (let  ((length (read-word stream)))
			     (loop :repeat length
				   :for string := (u:fp(read-string stream)) :collect string))))))
    (option-descriptor name title description type unit size cap constraint-type constraint)))

(defun read-parameters (stream)
  (let ((null (read-word stream))
	(format (aref +format+ (read-word stream)))
	(last (read-boolean stream))
	(bytes (read-word stream))
	(pixels (read-word stream))
	(lines (read-word stream))
	(depth (read-word stream)))
    (declare (ignore null))
    (parameters format last bytes pixels lines depth)))

(defun retrieve-scan (port &optional (host #(127 0 0 1)))
  (usocket:with-client-socket (socket stream host port :element-type '(unsigned-byte 8))
    (apply #'concatenate '(vector (unsigned-byte 8))
	   (loop :for length := (read-word stream)
		 :until (= length #xFFFFFFFF)
		 :for temp := (make-array length :element-type '(unsigned-byte))
		 :for read-length := (read-sequence temp stream)
		 :collect temp))))

;;;; Protocol 

(defun init (&optional (host #(127 0 0 1)) (port 6566) (name "cl-sane"))
  (let* ((opcode 0)
	 (version #(1 0 0 3)))
    (setf *socket* (usocket:socket-connect host port :element-type '(unsigned-byte 8))
	  *stream* (usocket:socket-stream *socket*))
    (write-word opcode *stream*)
    (write-sequence version *stream*)
    (write-sequence (sane-string name) *stream*)
    (force-output *stream*)
    (values (aref +status-codes+ (read-word *stream*))
	    (reverse (list (nibbles:read-ub16/be *stream*)
			   (read-byte *stream*)
			   (read-byte *stream*))))))

(defun get-devices ()
  (write-word 1 *stream*)
  (force-output *stream*)
  (let* ((status (read-word *stream*))
	 (is-null (read-word *stream*))
	 (pointer (read-word *stream*)))
    (if (= 1 is-null)
	:no-devices
	(loop :for device := (u:fp (read-device *stream*))
	      :for next := (u:fp (read-word *stream*))
	      :collect device
	      :until (= next 1)))))

(defun openc (device &optional (socket *socket*))
  (let ((opcode 2)
	(stream (usocket:socket-stream socket)))
    (write-word opcode stream)
    (write-sequence (sane-string (device-name device)) stream)
    (force-output stream)
    (let ((status (u:fp (read-word stream)))
	  (handle (u:fp (read-word stream)))
	  (resource (u:fp (read-string stream))))
      (values handle
	      (aref +status-codes+ status)
	      resource))))

(defun closec (handle  &optional (socket *socket*))
  (let ((opcode 3)
	(stream (usocket:socket-stream socket)))
    (write-word opcode stream)
    (write-word handle stream)
    (force-output stream)
    (read-word stream)))

(defun get-option-descriptors (handle)
  (let ((opcode 4))
    (write-word opcode *stream*)
    (write-word handle *stream*)
    (force-output *stream*)
    (let ((length (read-word *stream*))
	  (spacer (read-word *stream*)))
      (apply #'vector (loop :repeat length :for x :from 1 :collect (u:fp (read-option-descriptor *stream*))
			    :do (unless (= length x) (read-word *stream*)))))))

;; size is in bytes not words
(defun control-option (handle option action type size value)
  (let ((opcode 5)
	(stream (usocket:socket-stream *socket*)))
    (declare (type (member :get :set :auto) action))
    (write-word opcode stream)
    (write-word handle stream)
    (write-word option stream)
    (write-word (case action (:get 0) (:set 1) (:auto 2)) stream)
    (write-word (position type +value-type+) stream)
    (write-word size stream)
    (unless (eql type :string)
      (write-word 1 stream))
    (case type 
      (:boolean (write-boolean value stream))
      (:integer (write-word value stream))
      (:fixed (write-fixed value stream))
      (:string (write-string value stream))
      (:button (error "raise an issue")))
    (force-output stream)
    (let* ((status (aref +status-codes+ (read-word stream)))
	   (info (decode-mask (read-word stream) +info+))
	   (type (aref +value-type+ (read-word stream)))
	   (size (read-word stream))
	   (pointer (unless (eql type :string)
		       (read-word stream)))
	   (value (case type
		    (:boolean (read-boolean stream))
		    (:integer (read-word stream))
		    (:fixed (read-fixed stream))
		    (:string (read-string stream))
		    (:button (error "raise an issue")))) ; ? don't have a scanner with a button to see what happens
	   (resource (read-string stream)))
      (declare (ignore pointer))
      (list status info type size value resource))))

(defun get-parameters (handle)
  (let ((opcode 6)
	(stream (usocket:socket-stream *socket*)))
    (write-word opcode stream)
    (write-word handle stream)
    (force-output stream)
    (read-parameters stream)))


(defun start (handle)
  (let ((opcode 7)
	(stream (usocket:socket-stream *socket*)))
    (write-word opcode stream)
    (write-word handle stream)
    (force-output stream)
    (let ((status (aref +status-codes+ (read-word stream)))
	  (port (read-word stream))
	  (byte-order (read-byte-order stream))
	  (resource (read-string stream)))
      (list status port byte-order resource))))

(defun cancel (handle)
  (let ((opcode 8)
	(stream (usocket:socket-stream *socket*)))
    (write-word opcode stream)
    (write-word handle stream)
    (force-output stream)
   ; (read-word stream) ;api says dummy word is sent, but doesn't seem to be the case
    ))

;;untested, unkownn if need to read dummy word or same issue as CANCEL
(defun authorize (resource user password)
  (let ((opcode 9)
	(stream (usocket:socket-stream *socket*)))
    (write-word opcode stream)
    (write-string resource stream)
    (write-string user stream)
    (write-string password stream)))

(defun exit ()
  (let ((opcode 10)
	(stream (usocket:socket-stream *stream*)))
    (write-word opcode stream)))

;;;; junk?

(defun sane-word (int)
  (when (> int #xFFFFFFFF)
    (error "too large to fit in a word."))
  (rotatef (ldb (byte 8 0) int)(ldb (byte 8 24) int))
  (rotatef (ldb (byte 8 8) int)(ldb (byte 8 16) int))
  int)

(defun sane-string (string)
  (let* ((length (1+ (length string)))
	(array (make-array (+ 4 length) :initial-contents `(0 0 0 ,length ,@(map 'list #'char-code string) 0) :element-type '(unsigned-byte 8))))
    array))
