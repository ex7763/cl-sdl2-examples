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
