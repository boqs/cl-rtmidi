cl-rtmidi is a realtime midi driver for common lisp on linux platform.
it currently supports oss-emulated usb alsa devices, e.g:

(with-midi-oss-in (*default-midi-in-stream* "/dev/midi1")
  (loop (print (read-midi-message *default-midi-in-stream*))))

and also midi transport over UART on embedded linux devices, such as
beaglebone black:

(with-midi-uart-out (*default-midi-out-stream* "/dev/ttyS1")
  (loop
    (sleep 1)
    (write-midi-message (make-instance 'note-on-midi-message
                                       :raw-midi '(144 35 111)))))

This uart method has only been tested on beaglebone black.  In order
to set up the uart on arch linux, and give lisp the right permissions
this recipe seems to work:

append this line to uEnv.txt:
capemgr.enable_partno=BB-UART2

then reboot beaglebone black, then type the following unix
incantations:

echo BB-UART2 > /sys/devices/platform/bone_capemgr/slots
stty -F /dev/ttyS2 115200 sane -brkint -icrnl -opost -onlcr -isig -icanon -iexten -echo -echoe;
chmod a+rw /dev/ttyS2 (or add user to the uucp group and logout/login)

obviously everything could be done a bit neater if I was better at
UNIX, but hey if you're reading this I guess you are probably a
paren-slinging lisp badass and won't lose sleep over it...

