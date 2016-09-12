(in-package #:cl-rtmidi)

(defclass midi-oss-stream (midi-stream)
  ((byte-stream
    :accessor byte-stream
    :initarg :byte-stream)))

(defmethod read-midi-byte ((midi-stream midi-oss-stream))
  (read-byte (byte-stream midi-stream)))

(defmacro with-midi-oss-in ((midi-stream device-filename) &body body)
  (let ((byte-stream (gensym "byte-stream")))
    `(with-open-file (,byte-stream ,device-filename
				   :direction :io
				   :if-exists :overwrite
				   :element-type  '(unsigned-byte 8))
       (let ((,midi-stream (make-instance 'midi-oss-stream
					  :byte-stream ,byte-stream)))
	 ,@body))))

(defmacro with-midi-oss-out ((midi-stream device-filename) &body body)
  (let ((byte-stream (gensym "byte-stream")))
    `(with-open-file (,byte-stream ,device-filename
				   :direction :output
				   :if-exists :overwrite
				   :element-type '(unsigned-byte 8))
       (let ((,midi-stream (make-instance 'midi-oss-stream
					  :byte-stream ,byte-stream)))
	 ,@body))))

(defmethod write-midi-byte (byte (midi-stream midi-oss-stream))
  (write-byte byte (byte-stream midi-stream)))

(defmethod midi-force-output ((midi-stream midi-oss-stream))
  (force-output (byte-stream midi-stream)))
