;;;; package.lisp

(defpackage #:cl-rtmidi
  (:use #:cl #:optima)
  (:export #:midi-message
	   ;;Below are symbols related to performance gestures
	   #:midi-performance-gesture #:note-on-midi-message
	   #:note-off-midi-message #:key-pressure-midi-message
	   #:control-change-midi-message #:channel-mode-midi-message
	   #:program-change-midi-message #:channel-pressure-midi-message
	   #:pitch-bend-midi-message

	   #:sysex-message
	   #:sysex-dump #:boomerang-sysex-message
	   ;; Below are symbols related to midi clock
	   #:midi-timing-message #:clock-tick-midi-message
	   #:start-midi-message #:continue-midi-message
	   #:stop-midi-message #:song-position-pointer-midi-message
	   
	   ;; #:hi-nibble
	   ;; #:lo-nibble #:hi-bit
	   ;; #:pack-nibbles #:parse-packet
	   #:write-midi-message #:with-midi-out #:*default-midi-out-stream*
	   #:read-midi-message #:with-midi-in #:*default-midi-in-stream*
	   #:midi-note= #:make-midi-note-off #:make-midi-note-on
	   #:get-oss-midi-index-named #:get-oss-midi-dev-named))
