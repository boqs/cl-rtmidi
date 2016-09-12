;;;; cl-rtmidi.asd

(asdf:defsystem #:cl-rtmidi
  :description "MIDI-slinging on linux with lisp"
  :author "Rick Venn <sasquatch@rickvenn.com>"
  :license "GPL v2"
  :serial t
  :defsystem-depends-on ("cffi-grovel")
  :components ((:file "package")
	       (:file "cl-rtmidi")
	       (:cffi-grovel-file "serial-grovel")
	       (:file "packetiser")
	       (:file "midi-serial")
	       (:file "midi-oss"))
  :depends-on (:optima :cffi :iterate :external-program :iolib :cl-ppcre))

