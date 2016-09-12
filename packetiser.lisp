(in-package :cl-rtmidi)

(defparameter *midi-decoder-state* :waiting)

(defparameter +max-packet-length+ 3)

(defclass midi-message ()
  ((raw-midi :initarg :raw-midi)
   (timestamp :initarg :timestamp
	      :initform (get-internal-utime))))

(defclass midi-performance-gesture
    (midi-message)
  ())

(defclass note-on-midi-message (midi-performance-gesture)
  ())
(defclass note-off-midi-message (midi-performance-gesture)
  ())
(defclass key-pressure-midi-message (midi-performance-gesture)
  ())
(defclass control-change-midi-message (midi-performance-gesture)
  ())
(defclass channel-mode-midi-message (midi-performance-gesture)
  ())
(defclass program-change-midi-message (midi-performance-gesture)
  ())
(defclass channel-pressure-midi-message (midi-performance-gesture)
  ())
(defclass pitch-bend-midi-message (midi-performance-gesture)
  ())

(defclass sysex-message (midi-message)
  ())
(defclass sysex-dump (sysex-message)
  ())
(defclass boomerang-sysex-message (sysex-message)
  ())

(defclass midi-timing-message (sysex-message)
  ())

(defclass clock-tick-midi-message (midi-timing-message)
  ())
(defclass start-midi-message (midi-timing-message)
  ())
(defclass continue-midi-message (midi-timing-message)
  ())
(defclass stop-midi-message (midi-timing-message)
  ())
(defclass song-position-pointer-midi-message (midi-timing-message)
  ())

(defun hi-nibble (byte)
  (ash byte -4))

(defun lo-nibble (byte)
  (logand byte #b1111))

(defun hi-bit (byte)
  (ash byte -7))

(defun pack-nibbles (hi-nibble lo-nibble)
  (+ (ash hi-nibble 4)
     lo-nibble))

(defun parse-packet (packet)
  (match (list (hi-bit (car packet))
	       (hi-nibble (car packet))
	       (lo-nibble (car packet))
	       (cdr packet))
    ((list _
	   #b1111
	   #b1000
	   _)
     (make-instance 'clock-tick-midi-message
		    :raw-midi packet))
    ((list _
	   #b1111
	   #b1010
	   _)
     (make-instance 'start-midi-message
		    :raw-midi packet))
    ((list _
	   #b1111
	   #b1011
	   _)
     (make-instance 'continue-midi-message
		    :raw-midi packet))
    ((list _
	   #b1111
	   #b1100
	   _)
     (make-instance 'stop-midi-message
		    :raw-midi packet))
    ((list 1 hi lo (guard rest
			  (every (lambda (x)
				   (= 0 (hi-bit x)))
				 rest)))
     (match (list hi lo rest)
       ((list #b1000 _ (guard rest (= 2 (length rest))))
	(make-instance 'note-off-midi-message
		       :raw-midi packet))
       ((list #b1001 _ (guard rest (= 2 (length rest))))
	(make-instance 'note-on-midi-message
		       :raw-midi packet))
       ((list #b1010 _ (guard rest (= 2 (length rest))))
	(make-instance 'key-pressure-midi-message
		       :raw-midi packet))
       ((list #b1011 _ (guard rest (= 2 (length rest))))
	(make-instance 'control-change-midi-message
		       :raw-midi packet))
       ((list #b1100 _ (guard rest (= 1 (length rest))))
	(make-instance 'program-change-midi-message
		       :raw-midi packet))
       ((list #b1101 _ (guard rest (= 1 (length rest))))
	(make-instance 'channel-pressure-midi-message
		       :raw-midi packet))
       ((list #b1011 _ (guard rest (= 2 (length rest))))
	(make-instance 'pitch-bend-midi-message
		       :raw-midi packet))))))

(defun read-midi-message (&optional (midi-stream *default-midi-in-stream*))
  (declare (optimize (debug 3)))
  "reads midi-stream until a well-formed midi-message is received"
  (let ((packet (list (last-header midi-stream))))
    (loop as next-byte = (read-midi-byte midi-stream)
       do
	 (if (= 1 (hi-bit next-byte))
	     (progn (setf packet (list next-byte))
		    (setf (last-header midi-stream) next-byte))
	     (push next-byte packet))
	 (when packet
	   (let ((message (parse-packet (reverse packet))))
	     (when message
	       (return-from read-midi-message
		 message)))))))

(defvar *default-midi-out-stream*)

(defun write-midi-message (message &optional (midi-stream *default-midi-out-stream*))
  (loop for byte in (slot-value message 'raw-midi)
     do (write-midi-byte byte midi-stream))
  (midi-force-output midi-stream))

#+nil
(with-open-file (stream "/dev/snd/midiC3D0"
			:direction :io
			:if-exists :overwrite
			:element-type '(unsigned-byte 8))
  (loop (read-midi-message stream)))


(defun midi-note= (x y)
  (and (= (car (slot-value x 'raw-midi))
	  (car (slot-value y 'raw-midi)))
       (= (cadr (slot-value x 'raw-midi))
	  (cadr (slot-value y 'raw-midi)))))

(defun make-midi-note-off (chan key &optional (velocity 0))
  (make-instance 'note-off-midi-message
		 :raw-midi (list (pack-nibbles #b1000 chan)
				 key velocity)))

(defun make-midi-note-on (chan key &optional (velocity 127))
  (make-instance 'note-on-midi-message
		 :raw-midi (list (pack-nibbles #b1001 chan)
				 key velocity)))
