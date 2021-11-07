(in-package :nsane)

(defun document (type doc-strings)
  (dolist (doc-string doc-strings)
    (destructuring-bind (thing . doc) doc-string
      (case type
	(:functions  (setf (documentation (find-symbol (symbol-name thing)) 'function) (format nil doc)))
	(:package (setf (documentation (find-package (symbol-name thing)) t) (format nil doc)))))))

(document
 :package
 '((nsane .    
      "Common Lisp implementation of the SANE network protocol~%~

       This package is NOT meant to be :USE'd This is not a frontend. It is an implementation ~
       of the protocol in common lisp so anybody can write a frontend with it. There is one ~
       additional function besides the protocol, RETRIEVE-SCAN, that will retrieve and return a ~
       scan as a vector of (unsigned-byte 8).~%~

       NSANE:*SOCKET* needs to be bound to a socket returned by nsane:init before any other ~
       protocol functions are used.~%~

       Functions follow the naming convention of the protocol with SANE_NET_ dropped from the ~
       function and then kebobbed, e.g. SANE_NET_GET_OPTION_DESCRIPTORS becomes ~
       nsane:get-option-descriptors.~%~

       The protocol description is available at https://sane-project.gitlab.io/standard/net.html")))

(document
 :functions
 '((init .
      "Establishes a connection to a sane network daemon. Host, port, and name can be specified ~
       by keywords, Defaults to localhost:6566. name should be a unique name restricted to ascii ~
       characters, defaults to \"nsane\". Returns multiple values, a socket to be bound to ~
       NSANE:*SOCKET*, a status code, and the version of network daemon. ")
   
   (get-devices .
      "Returns a list of detected devices. Due to poor specification, may barf on more than one ~
       device. Raise an issue if this happens.")

   (open .
      "Takes a device as returned by GET-DEVICES and returns a handle to refer to the device.")

   (close .
      "Takes a handle and releases the connection to the device. Does not close the socket ~
       connection to the sane daemon.")

   (get-option-parameters .
      "Returns a vector of NSANE:OPTION structs. An options position in the vector is used to ~
       identify the option in NSANE:CONTROL-OPTION.")

   (control-option .
      "Takes a HANDLE returned by NSANE:OPEN, an OPTION represented by the position of the ~
       option in the vector returned by NSANE:GET-OPTION-PARAMETERS, an ACTION of either ~
       :GET :SET :AUTO, TYPE and SIZE as returned by GET-OPTION-PARAMETERS, and optionally ~
       new VALUE for the OPTION. ")

   (get-parameters .
      "Takes a HANDLE and returns a SCAN struct with the information necessary to interpret ~
       the flat vector returned by RETRIEVE-SCAN.")
   
   (start .
      "Starts scanning on the device referred to by the supplied HANDLE and returns a list ~
       of STATUS, PORT, BYTE-ORDER, RESOURCE. PORT is where your scan resides(on same host ~
       provided to NSANE:INIT) and what is passed to RETRIEVE-SCAN. BYTE-ORDER is either ~
       :LITTLE-ENDIAN or :BIG-ENDIAN. If RESOURCE is not an empty string it will point to a ~
       resource that needs to be authorized using NSANE:AUTHORIZE.")

   (cancel .
      "Takes a HANDLE and attempts to cancel any in progress scans.")

   (authorize .
      "Takes a RESOURCE provided by any function that requires authorization, as well as USER ~
       and PASSWORD strings to authorize access to the scanner.")
   
   (exit .
      "Closes the connection to the sane network daemon and closes the socket.")
   
   (retrieve-scan .
      "Takes a PORT returned by NSANE:START and optional HOST (defaults to 127.0.0.1 and should ~
       be same host as passed to NSANE:INIT) and returns a vector of ub8s to be interpreted as ~
       specified by NSANE:GET-PARAMETERS.")))
