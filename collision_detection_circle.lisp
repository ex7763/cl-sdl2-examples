(ql:quickload '("sdl2" "sdl2-image" "sdl2-ttf"))

(defpackage #:sdl-collision-circle
  (:use :cl :sdl2)
  (:export :main))

(in-package :sdl-collision-circle)

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

(defclass dot ()
  ((pos-x
    :accessor pos-x
    :initform 0)
   (pos-y
    :accessor pos-y
    :initform 0)
   (dot-width
    :accessor dot-width
    :initform 20)
   (dot-height
    :accessor dot-height
    :initform 20)
   (r
    :initarg :r
    :accessor r
    :initform 0)
   (vel-x
    :accessor vel-x
    :initform 0)
   (vel-y
    :accessor vel-y
    :initform 0)
   (obj-speed
    :accessor obj-speed
    :initform 0)))

(defgeneric dot-random-vel (obj num1 num2)
  (:documentation "Set random velocity num1 ~ (num1 + num2)."))

(defgeneric dot-random-pos (obj)
  (:documentation "Set random position."))

(defgeneric dot-move (obj)
  (:documentation "Move dot."))

(defgeneric dot-stop (obj)
  (:documentation "Stop dot."))

(defgeneric dot-bounce (obj)
  (:documentation "If hit the wall, change velocity of dot."))

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

(defmethod dot-random-vel ((obj dot) num1 num2)
  (setf (vel-x obj) (+ num1 (random num2)))
  (setf (vel-y obj) (+ num1 (random num2))))

(defmethod dot-random-pos ((obj dot))
  (setf (pos-x obj) (- (random *screen-width*) 20))
  (setf (pos-y obj) (- (random *screen-height*) 20)))

(defmethod dot-move ((obj dot))
  (incf (pos-x obj) (vel-x obj))
  (incf (pos-y obj) (vel-y obj)))

(defmethod dot-stop ((obj dot))
  (decf (pos-x obj) (vel-x obj))
  (decf (pos-y obj) (vel-y obj)))

(defmethod dot-bounce ((obj dot))
  (when (or (> (+ (pos-x obj) 20) *screen-width*)
            (< (pos-x obj) 0))
    (setf (vel-x obj) (- (vel-x obj))))
  (when (or (> (+ (pos-y obj) 20) *screen-height*)
            (< (pos-y obj) 0))
    (setf (vel-y obj) (- (vel-y obj)))))

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

(defun distance-square (ax ay bx by)
  (+ (expt (- bx ax) 2) (expt (- by ay) 2)))

(defun check-collision (obj1 obj2)
  (cond ((and (eql (type-of obj1) 'dot)
              (eql (type-of obj2) 'dot))
         (let ((total-raduis-square (expt (+ (r obj1) (r obj2)) 2)))
           (if (> total-raduis-square (distance-square (pos-x obj1) (pos-y obj1)
                                                       (pos-x obj2) (pos-y obj2)))
               t
               nil)))
        ((and (eql (type-of obj1) 'dot)
              (listp obj2))
         (let ((cx 0) (cy 0))
           (if (< (pos-x obj1) (first obj2))
               (setf cx (first obj2))
               (if (> (pos-x obj1) (+ (first obj2) (third obj2)))
                   (setf cx (+ (first obj2) (third obj2)))
                   (setf cx (pos-x obj1))))
           (if (< (pos-y obj1) (second obj2))
               (setf cy (second obj2))
               (if (> (pos-y obj1) (+ (second obj2) (fourth obj2)))
                   (setf cy (+ (second obj2) (fourth obj2)))
                   (setf cy (pos-y obj1))))
           (if (< (distance-square (pos-x obj1) (pos-y obj1) cx cy)
                  (expt (r obj1) 2))
               t
               nil)))
        (t (error "Wrong type"))))

(defun make-rect-from-list (lst)
  (sdl2:make-rect (first lst) (second lst) (third lst) (fourth lst)))

(defun main()
  (with-window-renderer (window renderer)
    (let ((dot-texture (make-instance 'texture :renderer renderer))
          (dot-speed 3)
          (dot1 (make-instance 'dot :r 10))
          (dot2 (make-instance 'dot :r 10))
          (wall-rect (list (/ *screen-width* 4) 30
                           (/ *screen-width* 8) (/ *screen-height* 2))))
      (texture-load-from-file dot-texture "media/dot.bmp")
      (dot-random-pos dot2)
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown
         (:keysym keysym)
         (let ((scancode (scancode-value keysym)))
           (cond ((scancode= scancode :scancode-up)
                  (setf (vel-y dot1) (- dot-speed)))
                 ((scancode= scancode :scancode-down)
                  (setf (vel-y dot1) dot-speed))
                 ((scancode= scancode :scancode-left)
                  (setf (vel-x dot1) (- dot-speed)))
                 ((scancode= scancode :scancode-right)
                  (setf (vel-x dot1) dot-speed)))))
        (:keyup
         (:keysym keysym)
         (let ((scancode (scancode-value keysym)))
           (cond ((scancode= scancode :scancode-up)
                  (setf (vel-y dot1) 0))
                 ((scancode= scancode :scancode-down)
                  (setf (vel-y dot1) 0))
                 ((scancode= scancode :scancode-left)
                  (setf (vel-x dot1) 0))
                 ((scancode= scancode :scancode-right)
                  (setf (vel-x dot1) 0)))))
        (:idle ()
               (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
               (sdl2:render-clear renderer)

               ;; Render a wall
               (sdl2:set-render-draw-color renderer #xCC #xFF #xCC #xFF)
               (sdl2:render-fill-rect renderer (make-rect-from-list wall-rect))

               (dot-move dot1)
               
               (when (or (check-collision dot1 dot2)
                         (check-collision dot1 wall-rect))
                   (dot-stop dot1))

               (texture-render dot-texture (- (pos-x dot1) 10)
                               (- (pos-y dot1) 10))
               (texture-render dot-texture (- (pos-x dot2) 10)
                               (- (pos-y dot2) 10))
               (sdl2:render-present renderer))))))

(main)
