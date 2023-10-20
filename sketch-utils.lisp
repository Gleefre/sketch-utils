(defpackage #:sketch-utils
  (:use #:cl #:sketch #:sketch-buttons)
  (:export #:fit #:with-fit #:fit-point)
  (:export #:with-split)
  (:export #:with-translate
           #:with-rotate
           #:with-scale)
  (:export #:filter-alpha
           #:with-color)
  (:export #:enable-scissor
           #:disable-scissor
           #:with-scissor)
  ;; sketch-buttons
  (:export #:button
           #:on-press
           #:on-release
           #:on-hover
           #:on-unhover)
  (:export #:button-contains
           #:button-event
           #:button-motion-event
           #:button-press
           #:button-release
           #:button-hover
           #:button-unhover)
  (:export #:bind
           #:binds)
  (:export #:with-buttons)
  (:export #:rectangle-button
           #:brect)
  (:export #:ellipse-button
           #:bellipse
           #:bcircle))

(in-package #:sketch-utils)

;;; sketch-fit

(defun fit (width height from-width from-height &optional (to-x 0) (to-y 0) (from-x 0) (from-y 0) max-scale)
  (translate from-x from-y)
  (let* ((scale (min (/ from-width width)
                     (/ from-height height)
                     (if max-scale max-scale 10000)))
         (x-shift (/ (- from-width (* width scale)) 2))
         (y-shift (/ (- from-height (* height scale)) 2)))
    (translate x-shift y-shift)
    (scale scale))
  (translate (- to-x) (- to-y)))

(defun fit-point (x y width height from-width from-height
                  &optional (to-x 0) (to-y 0) (from-x 0) (from-y 0) max-scale)
  (setf x (- x from-x)
        y (- y from-y))
  (let* ((scale (min (/ from-width width)
                     (/ from-height height)
                     (if max-scale max-scale 10000)))
         (x-shift (/ (- from-width (* width scale)) 2))
         (y-shift (/ (- from-height (* height scale)) 2)))
    (setf x (- x x-shift)
          y (- y y-shift))
    (setf x (/ x scale)
          y (/ y scale)))
  (setf x (+ x to-x)
        y (+ y to-y))
  (list x y))

(defmacro with-fit ((width height from-width from-height
                     &optional  (to-x 0) (to-y 0) (from-x 0) (from-y 0) max-scale)
                    &body body)
  `(progn
     (push-matrix)
     (fit ,width ,height ,from-width ,from-height ,to-x ,to-y ,from-x ,from-y ,max-scale)
     ,@body
     (pop-matrix)))

;;; with-split macro
(defmacro with-split ((width-var height-var &optional (orientation :horizontal))
                      &body size-body)
  (declare (type (member :horizontal :vertical) orientation))
  (let (($sizes (loop for clause in size-body collect (gensym "size")))
        ($sum-of-sizes (gensym "sum-of-sizes")))
    `(let (,@(loop for (size . body) in size-body
                   for $size in $sizes
                   collect `(,$size ,size)))
       (let ((,$sum-of-sizes (+ ,@$sizes)))
         (with-current-matrix
           ,@(loop for (size . body) in size-body
                   for $size in $sizes
                   collect `(let (,@(if (eq orientation :horizontal)
                                        `((,height-var ,height-var)
                                          (,width-var (* (/ ,$size ,$sum-of-sizes) ,width-var)))
                                        `((,width-var ,width-var)
                                          (,height-var (* (/ ,$size ,$sum-of-sizes) ,height-var)))))
                              (declare (ignorable ,height-var ,width-var))
                              ,@body)
                   collect (if (eq orientation :horizontal)
                               `(translate (* (/ ,$size ,$sum-of-sizes) ,width-var) 0)
                               `(translate 0 (* (/ ,$size ,$sum-of-sizes) ,height-var))))))
       nil)))

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

(defmacro with-color ((color &optional (type :fill)) &body body)
  `(with-pen (make-pen ,type ,color)
     ,@body))

;;; scissors
(defun enable-scissor (x y w h)
  (gl:enable :scissor-test)
  (destructuring-bind ((x1 y1) (x2 y2))
      (list
       (sketch::transform-vertex (list x (+ y h)) (sketch::env-model-matrix sketch::*env*))
       (sketch::transform-vertex (list (+ x w) y) (sketch::env-model-matrix sketch::*env*)))
    (let* ((height (sketch::sketch-height sketch::*sketch*))
           (y1 (- height y1))
           (y2 (- height y2))
           (x2 (max x1 x2))
           (y2 (max y1 y2)))
      (gl:scissor x1 y1 (- x2 x1) (- y2 y1)))))

(defun disable-scissor ()
  (gl:disable :scissor-test))

(defmacro with-scissor ((x y w h) &body body)
  `(progn
     (enable-scissor ,x ,y ,w ,h)
     ,@body
     (disable-scissor)))
