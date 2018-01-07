(ql:quickload '("sdl2" "sdl2-image" "sdl2-ttf"))

(in-package :catch-apple)

(defparameter *screen-width* 600)
(defparameter *screen-height* 800)

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "Catch Apple"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated :presentvsync))
         (sdl2-image:init '(:png))
         (sdl2-ttf:init)
         ,@body
         (sdl2-image:quit)
         (sdl2-ttf:quit)))))

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

(defun apple-random ()
  (+ (random (- *screen-width* 50)) 25))

(defmacro reset-apple (apple-x apple-vel-y apple-y total)
  `(progn
     (setf ,apple-x (apple-random))
     (setf ,apple-vel-y (random 3))
     (setf ,apple-y 30)
     (incf ,total)))

(defmacro local-path (path)
  `(merge-pathnames ,path catch-apple-config:*base-directory*))

(defun main()
  (with-window-renderer (window renderer)
    (let* ((font (sdl2-ttf:open-font (local-path #P"media/monaco.ttf") 20))
           (gamestate :menu)
           (apple-texture (make-instance 'texture :renderer renderer))
           (apple-acc 1)
           (apple-speed-max 10)
           (apple-x (apple-random))
           (apple-y 30)
           (apple-vel-x 0)
           (apple-vel-y 0)
           (box-texture (make-instance 'texture :renderer renderer))
           (box-x 0)
           (box-y (- *screen-height* 150))
           (score-texture (make-instance 'texture :renderer renderer))
           (catched 0)
           (total 0)
           (text-texture (make-instance 'texture :renderer renderer))
           (window-rect (sdl2::make-rect 0 0 *screen-width* *screen-height*))
           (box-rect (sdl2:make-rect box-x box-y
                                     85 85)))
      (texture-load-from-file apple-texture (local-path #p"media/apple.png"))
      (texture-load-from-file box-texture (local-path #p"media/box.png"))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown
         (:keysym keysym)
         (let ((scancode (sdl2:scancode-value keysym)))
           (cond ((scancode= scancode :scancode-escape)
                  (sdl2:push-event :quit)))))
        (:mousebuttondown ()
                          (when (eq gamestate :menu)
                            (setf gamestate :play)))
        (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
                      (setf box-x x))
        (:idle ()
               (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
               (sdl2:render-clear renderer)

               (case gamestate
                 (:menu (progn
                          (sdl2:set-render-draw-color renderer #xCC #xFF #xCC #xFF)
                          (sdl2:render-clear renderer)

                          (texture-load-from-string text-texture font
                                                    "Click to start")
                          (texture-render text-texture
                                          (- (floor *screen-width* 2)
                                             (floor (mWidth text-texture) 2))
                                          (- (floor *screen-height* 2)
                                             (floor (mHeight text-texture) 2)))))
                 (:play (progn
                          (sdl2:hide-cursor)
                          (when (= (mod (sdl2:get-ticks) 1) 0)
                            (incf apple-vel-y apple-acc)
                            (incf apple-y apple-vel-y))

                          (if (> apple-vel-y 0)
                              (incf apple-y (min apple-vel-y apple-speed-max))
                              (incf apple-y (max apple-vel-y (- apple-speed-max))))
                          
                          (when (> apple-y *screen-height*)
                            (reset-apple apple-x apple-vel-y apple-y total))

                          (when (check-collision (sdl2:make-rect apple-x apple-y
                                                                 40 40)
                                                 (sdl2:make-rect (- box-x 43) box-y
                                                                 85 85))
                            (reset-apple apple-x apple-vel-y apple-y total)
                            (incf catched))

                          (when (= total 100)
                            (setf gamestate :finish))
                          
                          (texture-load-from-string score-texture font
                                                    (format nil "Catched apple: ~A / ~A!" catched total))

                          (texture-render score-texture 0 0)
                          (texture-render apple-texture apple-x apple-y)
                          (texture-render box-texture (- box-x 43) box-y)))
                 (:finish (progn
                            (sdl2:show-cursor)
                            (sdl2:set-render-draw-color renderer #xCC #x99 #xFF #xFF)
                            (sdl2:render-clear renderer)
                            (texture-load-from-string text-texture font
                                                      (format nil "Your score is ~A / ~A!" catched total))
                            (texture-render text-texture
                                            (- (floor *screen-width* 2)
                                               (floor (mWidth text-texture) 2))
                                            (- (floor *screen-height* 2)
                                               (floor (mHeight text-texture) 2)))
                            (sdl2:render-present renderer)
                            (sdl2:delay 5000)
                            (setf total 0)
                            (setf catched 0)
                            (reset-apple apple-x apple-vel-y apple-y total)
                            (setf gamestate :menu))))
               
               (sdl2:render-present renderer))))))
