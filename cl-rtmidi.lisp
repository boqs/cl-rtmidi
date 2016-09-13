;;;; cl-rtmidi.lisp

(in-package #:cl-rtmidi)

(defclass midi-stream ()
  ((last-header
    :accessor last-header
    :initform nil)))

(defvar *default-midi-in-stream*)

(defgeneric read-midi-byte (midi-stream))

(defgeneric write-midi-byte (byte midi-stream))

(defgeneric midi-force-output (midi-stream))

(defun get-oss-midi-dev-named (name)
  "dirty little unixey hack to figure out what OSS device different
USB dongles are on"
  (let ((dev-idx (get-oss-midi-index-named name)))
    (when dev-idx
      (format nil
	      "/dev/midi~A"
	      dev-idx))))

(defun get-oss-midi-index-named (name)
  "this one does the heavy lifting"
  (with-open-file (cards "/proc/asound/cards")
    (loop as line = (read-line cards nil)
       while line
       when (cl-ppcre:scan name line)
       return (aref (nth-value 1 (cl-ppcre:scan-to-strings
				  " (\\d*)" line))
		    0))))

(defun get-virmidi (idx)
  "virtual midi needs to get read in a funny place"
  (format nil "/dev/snd/midiC~aD~a"
	  (get-oss-midi-index-named "VirMIDI")
	  idx))

#+sbcl
(defun get-internal-utime ()
  (multiple-value-bind (s us) (sb-ext:get-time-of-day)
    (+ (* 1000000 s)
       us)))
#+ccl
(defun get-internal-utime ()
  (round (/ (ccl:current-time-in-nanoseconds)
	    1000)))
