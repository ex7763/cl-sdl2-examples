(ql:quickload '("sdl2" "sdl2-image"))

(defpackage #:sdl-animated
  (:use :cl :sdl2)
  (:export :main))

(in-package :sdl-animated)

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

(defgeneric texture-set-viewport (obj x y width height)
  (:documentation "Set viewport."))

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
      
      (setf mTexture
            (sdl2:create-texture-from-surface renderer surface)))))

(defmethod texture-set-viewport ((obj texture) x y width height)
  (with-slots (renderer mTexture) obj
    (sdl2:render-set-viewport renderer
                              (sdl2:make-rect x y width height))
    (sdl2:render-copy renderer mTexture)))

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
                           ,@body))))

(defmacro frame-incf (frame)
  `(if (> ,frame 10000)
       (setf ,frame 0)
       (incf ,frame)))

(defun main()
  (with-window-renderer (window renderer)
    (sdl2-image:init '(:png))
    (let ((foos-texture (make-instance 'texture :renderer renderer))
          (foos-clips '((0 0 64 205)
                        (64 0 64 205)
                        (128 0 64 205)
                        (196 0 64 205)))
          (frame 0)
          (frame-pre-sprite 10))
     
          (texture-load-from-file foos-texture "media/foos.png")
           
          (sdl2:with-event-loop (:method :poll)
            (:quit () t)
            (:idle ()
                   (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
                   (sdl2:render-clear renderer)

                   (texture-render foos-texture 0 0
                                   :clip (nth (mod (floor frame frame-pre-sprite) 4)
                                              foos-clips))
                   
                   (frame-incf frame)
                   
                   (sdl2:render-present renderer))))))

(main)
