(defpackage #:catch-apple-config (:export #:*base-directory*))
(defparameter catch-apple-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defsystem #:catch-apple
    :version "0.1"
    :name "catch-apple"
    :author "Hsu,Po-Chun"
    :license "MIT License"
    :depends-on (:sdl2 :sdl2-image :sdl2-ttf)
    :serial t
    :components ((:file "package")
                 (:file "texture_class")
                 (:file "timer_class")
                 (:file "main" :depends-on ("texture_class" "timer_class"))))
