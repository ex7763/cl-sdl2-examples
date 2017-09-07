;; UNDONE

(ql:quickload '("sdl2" "sdl2-image" "sdl2-ttf"))

(defpackage #:sdl-motion
  (:use :cl :sdl2)
  (:export :main))

(in-package :sdl-motion)

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

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated :presentvsync))
         (sdl2-image:init '(:png))
         ,@body
         (sdl2-image:quit)))))

(defun check-collision (rect1 rect2)
  (if (or (<= (+ (sdl2:rect-y rect1) (sdl2:rect-height rect1))
              (sdl2:rect-y rect2))
          (>= (sdl2:rect-y rect1)
              (+ (sdl2:rect-y rect2) (sdl2:rect-height rect2)))
          (<= (+ (sdl2:rect-x rect1) (sdl2:rect-width rect1))
              (sdl2:rect-x rect2))
          (>= (sdl2:rect-x rect1)
              (+ (sdl2:rect-x rect2) (sdl2:rect-width rect2))))
      nil
      t))

(defun main()
  (with-window-renderer (window renderer)
    (let ((dot-texture (make-instance 'texture :renderer renderer))
          (dot-speed 3)
          (dot-x 0)
          (dot-y 0)
          (dot-vel-x 0)
          (dot-vel-y 0)
          (window-rect (sdl2::make-rect 0 0 *screen-width* *screen-height*))
          (wall-rect (sdl2:make-rect (/ *screen-width* 4) 30
                                     (/ *screen-width* 8) (/ *screen-height* 2))))
      (texture-load-from-file dot-texture "media/dot.bmp")
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown
         (:keysym keysym)
         (let ((scancode (scancode-value keysym)))
           (cond ((scancode= scancode :scancode-up)
                  (setf dot-vel-y (- dot-speed)))
                 ((scancode= scancode :scancode-down)
                  (setf dot-vel-y dot-speed))
                 ((scancode= scancode :scancode-left)
                  (setf dot-vel-x (- dot-speed)))
                 ((scancode= scancode :scancode-right)
                  (setf dot-vel-x dot-speed)))))
        (:keyup
         (:keysym keysym)
         (let ((scancode (scancode-value keysym)))
           (cond ((scancode= scancode :scancode-up)
                  (setf dot-vel-y 0))
                 ((scancode= scancode :scancode-down)
                  (setf dot-vel-y 0))
                 ((scancode= scancode :scancode-left)
                  (setf dot-vel-x 0))
                 ((scancode= scancode :scancode-right)
                  (setf dot-vel-x 0)))))
        (:idle ()
               (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
               (sdl2:render-clear renderer)

               ;; Render a wall
               (sdl2:set-render-draw-color renderer #xCC #xFF #xCC #xFF)
               (sdl2:render-fill-rect renderer wall-rect)


               (incf dot-x dot-vel-x)
               (incf dot-y dot-vel-y)
               
               (when (or
                      (not (check-collision (sdl2:make-rect dot-x dot-y
                                                            (mWidth dot-texture) (mHeight dot-texture))
                                            window-rect))
                      (check-collision (sdl2:make-rect dot-x dot-y
                                                       (mWidth dot-texture) (mHeight dot-texture))
                                       wall-rect))
                 (progn
                   (decf dot-x dot-vel-x)
                   (decf dot-y dot-vel-y)))

               (texture-render dot-texture dot-x dot-y)
               (sdl2:render-present renderer))))))

(main)
