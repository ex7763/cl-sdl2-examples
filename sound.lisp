(ql:quickload '("sdl2" "sdl2-image" "sdl2-ttf" "sdl2-mixer"))

(defpackage #:sdl-sound
  (:use :cl :sdl2)
  (:export :main))

(in-package :sdl-sound)

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
         (sdl2-ttf:init)
         (sdl2-mixer:init :mp3)
         ,@body
         (sdl2-image:quit)
         (sdl2-ttf:quit)
         (sdl2-mixer:quit)))))

(defun main()
  (with-window-renderer (window renderer)
    (sdl2-mixer:open-audio 22050 :s16sys 2 2048)
    (let ((font (sdl2-ttf:open-font "monaco.ttf" 20))
          (text-texture (make-instance 'texture :renderer renderer))
          (music (sdl2-mixer:load-music "temple.mp3"))
          (volume 128))
      (texture-load-from-string text-texture font
                                "space to start, 0 to stop, 1 to resume and 2 to pause")
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown
         (:keysym keysym)
         (let ((scancode (scancode-value keysym)))
           (cond ((scancode= scancode :scancode-space)
                  (progn (format t "Playing Song~%")
                         (sdl2-mixer:play-music music -1)))
                 ((scancode= scancode :scancode-0)
                  (progn (format t "Stop Song~%")
                         (sdl2-mixer:halt-music)))
                 ((scancode= scancode :scancode-up)
                  (when (< (+ volume 20) 128)
                    (incf volume 20)
                    (format t "Current Volume: ~a~%" volume)
                    (sdl2-mixer:volume-music volume)))
                 ((scancode= scancode :scancode-down)
                  (when (> (- volume 20) 0)
                    (decf volume 20)
                    (format t "Current Volume: ~a~%" volume)
                    (sdl2-mixer:volume-music volume))))))
        (:idle ()
               (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
               (sdl2:render-clear renderer)

               (texture-render text-texture
                               (- (floor *screen-width* 2)
                                  (floor (mWidth text-texture) 2))
                               (- (floor *screen-height* 2)
                                  (floor (mHeight text-texture) 2)))

               (sdl2:render-present renderer)))
      (sdl2-mixer:halt-music)
      (sdl2-mixer:close-audio)
      (sdl2-mixer:free-music music))))

(main)
