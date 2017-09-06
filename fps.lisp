(ql:quickload '("sdl2" "sdl2-image" "sdl2-ttf"))

(defpackage #:sdl-timer
  (:use :cl :sdl2)
  (:export :main))

(in-package :sdl-timer)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defclass texture ()
  ((renderer
    :initarg :renderer
    :accessor renderer
    :initform (error "Musut supply a renderer"))
   (mTexture
    :accessor mTexture
    :initform nil)
   (mWidth
    :accessor mWidth
    :initform 0)
   (mHeight
    :accessor mHeight
    :initform 0)))

(defgeneric texture-load-from-file (obj filepath)
  (:documentation "Load texture from flie."))

(defgeneric texture-load-from-string (obj font str)
  (:documentation "Load texture from string."))

(defgeneric texture-set-viewport (obj x y width height)
  (:documentation "Set viewport."))

(defgeneric texture-set-color (obj r g b)
  (:documentation "Set color modulation."))

(defgeneric texture-render (obj x y &key clip)
  (:documentation "Render this texture."))

(defclass timer ()
  ((StartTicks
    :accessor StartTicks
    :initform 0)
   (PausedTicks
    :accessor PausedTicks
    :initform 0)
   (Paused
    :type boolean
    :accessor Paused
    :initform nil)
   (Started
    :type boolean
    :accessor Started
    :initform nil)))

(defgeneric timer-start (obj)
  (:documentation "Start a timer."))

(defgeneric timer-stop (obj)
  (:documentation "Stop a timer."))

(defgeneric timer-pause (obj)
  (:documentation "Pause a timer."))

(defgeneric timer-resume (obj)
  (:documentation "Unpause a timer."))

(defgeneric timer-get-ticks (obj)
  (:documentation "Gets a timer's time."))

(defmethod texture-load-from-file ((obj texture) filepath)
  (with-slots (renderer mTexture mWidth mHeight) obj
    (let ((surface (sdl2-image:load-image filepath)))
      (setf mWidth (sdl2:surface-width surface))
      (setf mHeight (sdl2:surface-height surface))
      ;; color-key 
      (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface)
                                                      0 #xFF #xFF))
      (setf mTexture (sdl2:create-texture-from-surface renderer surface)))))

(defmethod texture-load-from-string ((obj texture) font str)
   (with-slots (renderer mTexture mWidth mHeight) obj
    (let ((surface (sdl2-ttf:render-text-solid font str 0 0 0 0)))
      (setf mWidth (sdl2:surface-width surface))
      (setf mHeight (sdl2:surface-height surface))
      (setf mTexture (sdl2:create-texture-from-surface renderer surface)))))

(defmethod texture-set-viewport ((obj texture) x y width height)
  (with-slots (renderer mTexture) obj
    (sdl2:render-set-viewport renderer
                              (sdl2:make-rect x y width height))
    (sdl2:render-copy renderer mTexture)))

(defmethod texture-set-color ((obj texture) r g b)
  (sdl2:set-texture-color-mod (mTexture obj) r g b))

(defmethod texture-render ((obj texture) x y &key clip)
  (with-slots (renderer mTexture mWidth mHeight) obj
    (cond (clip
           (progn
             (when (listp clip)
               (setf clip (apply #'sdl2::make-rect clip)))
             (sdl2:render-copy renderer mTexture
                               :source-rect clip
                               :dest-rect
                               (sdl2:make-rect x y
                                               (sdl2:rect-width clip)
                                               (sdl2:rect-height clip)))))
          (t
           (sdl2:render-copy renderer mTexture :dest-rect
                             (sdl2:make-rect x y mWidth mHeight))))))

(defmethod timer-start ((obj timer))
  (with-slots (StartTicks PausedTicks Paused Started) obj
    (setf StartTicks (sdl2:get-ticks))
    (setf PausedTicks 0)
    (setf Started t)
    (setf Paused nil)))

(defmethod timer-stop ((obj timer))
  (with-slots (StartTicks PausedTicks Paused Started) obj
    (setf StartTicks 0)
    (setf PausedTicks 0)
    (setf Started nil)
    (setf Paused nil)))

(defmethod timer-pause ((obj timer))
  (with-slots (StartTicks PausedTicks Paused Started) obj
    (when (and Started (not Paused))
      (setf StartTicks 0)
      (setf PausedTicks (- (sdl2:get-ticks) StartTicks))
      (setf Paused t))))

(defmethod timer-resume ((obj timer))
  (with-slots (StartTicks PausedTicks Paused Started) obj
    (when (and Started Paused)
      (setf StartTicks (- (sdl2:get-ticks) PausedTicks))
      (setf PausedTicks 0)
      (setf Paused nil))))

(defmethod timer-get-ticks ((obj timer))
  (with-slots (StartTicks PausedTicks Paused Started) obj
    (if Started
      (if Paused
          PausedTicks
          (- (sdl2:get-ticks) StartTicks))
      0)))

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated :presentvsync))
         (sdl2-image:init '(:png))
         (sdl2-ttf:init)
         ,@body
         (sdl2-image:quit)
         (sdl2-ttf:quit)))))

(defmacro frame-incf (frame)
  `(if (= ,frame most-positive-fixnum)
       (setf ,frame 1)
       (incf ,frame)))

(defun main()
  (with-window-renderer (window renderer)
    (let* ((font (sdl2-ttf:open-font "monaco.ttf" 20))
          (text-texture (make-instance 'texture :renderer renderer))
          (fps-timer (make-instance 'timer))
          (cap-timer (make-instance 'timer))
          (fixed-fps 30)
          (tick-per-frame (floor 1000 fixed-fps))
          (frames 1))

      (timer-start fps-timer)
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               (timer-start cap-timer)
               
               (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
               (sdl2:render-clear renderer)

               (texture-load-from-string
                text-texture font (format nil "FPS: ~A"
                                          (floor frames
                                                 (1+ (floor (timer-get-ticks fps-timer) 1000)))))

               (texture-render text-texture
                               (- (floor *screen-width* 2)
                                  (floor (mWidth text-texture) 2))
                               (- (floor *screen-height* 2)
                                  (floor (mHeight text-texture) 2)))

               (let ((time (timer-get-ticks cap-timer)))
                     (when (< time tick-per-frame)
                       (sdl2:delay (floor (- tick-per-frame time)))))

               (frame-incf frames)
               (sdl2:render-present renderer))))))

(main)
