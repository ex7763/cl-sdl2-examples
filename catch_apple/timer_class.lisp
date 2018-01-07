(in-package :catch-apple)

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
