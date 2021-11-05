(in-package :asdf-user)
(defsystem "nsane"
  :description "sane network protocl"
  :version "0.0.1"
  :licence "LGPL"
  :author "Johannes Martinez Calzada"
  :depends-on ("usocket")
  :components ((:file "nsane")))
