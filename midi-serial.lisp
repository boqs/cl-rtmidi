(in-package :cl-rtmidi)

(defconstant +midi-baud-speed+ 31250)

;; XXX HACK! this sucks pretty bad, oh well...
(defun setup-midi-uart-dev (dev-file)
  (external-program:run "stty" (list "-F" dev-file "raw"))
  (external-program:run "stty" (list "-F" dev-file "sane" "-brkint" "-icrnl" "-opost" "-onlcr" "-isig" "-icanon" "-iexten" "-echo" "-echoe")))

(defmacro with-midi-uart-fd ((fd dev-filename file-mode) &body body)
  (let ((tio (gensym)))
    `(progn
       (setup-midi-uart-dev ,dev-filename)
       (let ((,fd (iolib.syscalls:open ,dev-filename (logior ,file-mode
							     o-noctty))))
	 (unwind-protect (cffi:with-foreign-pointer (,tio (cffi:foreign-type-size '(:struct termios2)))
			   (assert (>= (iolib.syscalls:ioctl ,fd tcgets2 ,tio)
				       0))
			   (cffi:with-foreign-slots ((c-cflag c-ispeed c-ospeed)
						     ,tio
						     (:struct termios2))
			     (setf (cffi:foreign-slot-value ,tio '(:struct termios2)
							    'c-cflag)
				   (logior bother
					   (logand (lognot cbaud)
						   c-cflag)))
			     (setf (cffi:foreign-slot-value ,tio '(:struct termios2)
							    'c-ispeed)
				   +midi-baud-speed+)
			     (setf (cffi:foreign-slot-value ,tio '(:struct termios2)
							    'c-ospeed)
				   +midi-baud-speed+)
			     (assert (>= (iolib.syscalls:ioctl ,fd tcsets2 ,tio)
				       0))
			     (progn ,@body)))
	   (iolib.syscalls:close ,fd))))))

(defclass midi-uart-stream (midi-stream)
  ((fd
    :accessor fd
    :initarg :fd)
   (char-buffer
    :accessor rd-buffer
    :initarg :rd-buffer)
   (wr-buffer
    :accessor wr-buffer
    :initarg :wr-buffer)))

(defmacro with-midi-uart-stream ((midi-stream dev-filename file-mode) &body body)
  (let ((fd (gensym))
	(rd-buf (gensym))
	(wr-buf (gensym)))
    `(with-midi-uart-fd (,fd ,dev-filename ,file-mode)
       (cffi:with-foreign-pointer (,rd-buf 1)
	 (cffi:with-foreign-pointer (,wr-buf 1)
	   (let ((,midi-stream (make-instance 'midi-uart-stream
					      :fd ,fd
					      :rd-buffer ,rd-buf
					      :wr-buffer ,wr-buf)))
	     (progn ,@body)))))))

(cffi:defcfun tcflush :int (fd :int) (queue-selector :int))

(defmacro with-midi-uart-out ((midi-stream dev-filename) &body body)
  `(with-midi-uart-stream (,midi-stream ,dev-filename o-wronly)
     ;; (tcflush (fd ,midi-stream) tcoflush)
     (progn ,@body)))

(defmethod write-midi-byte (byte (midi-stream midi-uart-stream))
  (setf (cffi:mem-ref (wr-buffer midi-stream) :uchar)
	byte)
  (iolib.syscalls:write (fd midi-stream) (wr-buffer midi-stream) 1))

(defmacro with-midi-uart-in ((midi-stream dev-filename) &body body)
  `(with-midi-uart-stream (,midi-stream ,dev-filename o-rdonly)
     (tcflush (fd ,midi-stream) tciflush)
     (progn ,@body)))

(defmethod read-midi-byte ((midi-stream midi-uart-stream))
  (iolib.syscalls:read (fd midi-stream) (rd-buffer midi-stream) 1)
  (cffi:mem-ref (rd-buffer midi-stream) :uchar))

(defmethod midi-force-output ((midi-stream midi-uart-stream))
  ;; for some reason this borks. Hmmm....
  ;; (iolib.syscalls:fsync (print (fd midi-stream)))
  )

(defmacro with-midi-uart-io ((midi-stream dev-filename) &body body)
  `(with-midi-uart-stream (,midi-stream ,dev-filename o-rdrw)
     (progn ,@body)))

(defun write-list-to-fd (list fd)
  (cffi:with-foreign-object (foo :uchar (length list))
      (dotimes (i (length list))
	(setf (cffi:mem-aref foo :uchar i)
	      (nth i list)))
      (iolib.syscalls:write fd foo 3)))
