(ql:quickload '("sdl2" "sdl2-image"))

(defpackage #:sdl-render
  (:use :cl :sdl2)
  (:export :main))

(in-package :sdl-render)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
                           ,@body))))

(defun main()
  (with-window-renderer (window renderer)
    (sdl2:with-event-loop (:method :poll)
      (:idle ()
             (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
             (sdl2:render-clear renderer)
             (sdl2:set-render-draw-color renderer #xFF #x00 #x00 #xFF)
             (sdl2:render-fill-rect
              renderer (sdl2:make-rect (/ *screen-width* 4)
                                       0
                                       (/ *screen-width* 2)
                                       *screen-height*))
             (sdl2:set-render-draw-color renderer #x00 #xFF #x00 #xFF)
             (sdl2:render-draw-rect
              renderer (sdl2:make-rect (round (/ *screen-width* 6))
                                       (round (/ *screen-height* 8))
                                       (round (* 2/3 *screen-width*))
                                       (round (* 2/3 *screen-height*))))
             (sdl2:set-render-draw-color renderer #x00 #x00 #xFF #xFF)
             (sdl2:render-draw-line renderer
                                    0
                                    (/ *screen-height* 2)
                                    *screen-width*
                                    (/ *screen-height* 2))
             (sdl2:set-render-draw-color renderer #xFF #xFF #x00 #xFF)
             (loop for i from 0 below *screen-height* by 4
                do (sdl2::render-draw-point renderer (/ *screen-width* 2) i))
             (sdl2:render-present renderer))
      (:quit () t))))

(main)
