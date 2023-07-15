(defpackage #:sketch-utils
  (:use #:cl #:sketch #:sketch-fit)
  (:export #:fit #:with-fit #:fit-point)
  (:export #:with-translate
           #:with-rotate
           #:with-scale)
  (:export #:filter-alpha)
  (:export #:enable-scissor
           #:disable-scissor
           #:with-scissor))

(in-package #:sketch-utils)

;;; Basic with- macros for translate, rotate and scale sketch functions
(defmacro with-translate ((dx dy) &body body)
  `(with-current-matrix
     (translate ,dx ,dy)
     ,@body))

(defmacro with-rotate ((angle &optional (cx 0) (cy 0)) &body body)
  `(with-current-matrix
     (rotate ,angle ,cx ,cy)
     ,@body))

(defmacro with-scale ((sx &optional sy (cx 0) (cy 0)) &body body)
  `(with-current-matrix
     (scale ,sx ,sy ,cx ,cy)
     ,@body))

;;; colors
(defun filter-alpha (color alpha)
  (rgb (color-red color) (color-green color) (color-blue color)
       alpha))

;;; scissors
(defun enable-scissor (x y w h)
  (gl:enable :scissor-test)
  (destructuring-bind ((x1 y1) (x2 y2))
      (list
       (sketch::transform-vertex (list x (+ y h)) (sketch::env-model-matrix sketch::*env*))
       (sketch::transform-vertex (list (+ x w) y) (sketch::env-model-matrix sketch::*env*)))
    (let* ((height (sketch::sketch-height sketch::*sketch*))
           (y1 (- height y1))
           (y2 (- height y2)))
      (gl:scissor x1 y1 (- x2 x1) (- y2 y1)))))

(defun disable-scissor ()
  (gl:disable :scissor-test))

(defmacro with-scissor ((x y w h) &body body)
  `(progn
     (enable-scissor ,x ,y ,w ,h)
     ,@body
     (disable-scissor)))
