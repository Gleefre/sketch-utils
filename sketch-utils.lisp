(defpackage #:sketch-utils
  (:use #:cl #:sketch-buttons)
  (:local-nicknames (#:s #:sketch))
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
  (:export #:define-start-function)
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
  (s:translate from-x from-y)
  (let* ((scale (min (/ from-width width)
                     (/ from-height height)
                     (if max-scale max-scale 10000)))
         (x-shift (/ (- from-width (* width scale)) 2))
         (y-shift (/ (- from-height (* height scale)) 2)))
    (s:translate x-shift y-shift)
    (s:scale scale))
  (s:translate (- to-x) (- to-y)))

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
     (s:push-matrix)
     (fit ,width ,height ,from-width ,from-height ,to-x ,to-y ,from-x ,from-y ,max-scale)
     ,@body
     (s:pop-matrix)))

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
         (s:with-current-matrix
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
                               `(s:translate (* (/ ,$size ,$sum-of-sizes) ,width-var) 0)
                               `(s:translate 0 (* (/ ,$size ,$sum-of-sizes) ,height-var))))))
       nil)))

;;; Basic with- macros for translate, rotate and scale sketch functions
(defmacro with-translate ((dx dy) &body body)
  `(s:with-current-matrix
     (s:translate ,dx ,dy)
     ,@body))

(defmacro with-rotate ((angle &optional (cx 0) (cy 0)) &body body)
  `(s:with-current-matrix
     (s:rotate ,angle ,cx ,cy)
     ,@body))

(defmacro with-scale ((sx &optional sy (cx 0) (cy 0)) &body body)
  `(s:with-current-matrix
     (s:scale ,sx ,sy ,cx ,cy)
     ,@body))

;;; colors
(defun filter-alpha (color alpha)
  (s:rgb (s:color-red color) (s:color-green color) (s:color-blue color)
         alpha))

(defmacro with-color ((color &optional (type :fill)) &body body)
  `(s:with-pen (s:make-pen ,type ,color)
     ,@body))

;;; scissors
(defun enable-scissor (x y w h)
  (gl:enable :scissor-test)
  (destructuring-bind ((x1 y1) (x2 y2))
      (list
       (s::transform-vertex (list x (+ y h)) (s::env-model-matrix s::*env*))
       (s::transform-vertex (list (+ x w) y) (s::env-model-matrix s::*env*)))
    (let* ((height (s::sketch-height s::*sketch*))
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

;;; main function

(defmacro define-start-function ((name &optional toplevel-name)
                                 sketch-name initargs
                                 &body options)
  "If toplevel-name is not specified uses `<name>-toplevel'.
Possible options:
  :setup - defines `sketch:setup' `:before' method
      (:setup (<arg-name>)
        <body>)
  :on-close - defines `kit.sdl2:on-close' `:before' method;
      (:on-close (<arg-name>)
        <body>)
  :start - executed before creating an instance of sketch (on every function call)
      (:start <body>)
  :quit - executed after the instance is closed (only for toplevel function)
      (:quit <body>)"
  (let ((initargs-name (gensym "INITARGS"))
        (toplevel-name (or toplevel-name
                           (intern (concatenate 'string
                                                (symbol-name name)
                                                "-TOPLEVEL")
                                   (symbol-package name)))))
    (flet ((define-method (name allow-other-keys arg &rest body)
             `(defmethod ,name :before ((,@arg ,sketch-name)
                                        ,@(if allow-other-keys
                                              '(&key &allow-other-keys)))
                (declare (ignorable ,@arg))
                ,@body)))
      `(progn
         ,(alexandria:when-let (arg-and-body (cdr (assoc :setup options)))
            (apply #'define-method 'sketch:setup t arg-and-body))
         ,(alexandria:when-let (arg-and-body (cdr (assoc :on-close options)))
            (apply #'define-method 'kit.sdl2:close-window nil arg-and-body))
         (defun ,name (&rest ,initargs-name &key &allow-other-keys)
           (sketch::initialize-sketch)
           ,@(cdr (assoc :start options))
           (apply #'make-instance ',sketch-name (append ,initargs-name (list ,@initargs))))
         (defun ,toplevel-name ()
           (sdl2:make-this-thread-main
            (lambda ()
              (let ((sketch::*build* t))
                (sketch::initialize-sketch)
                ,@(cdr (assoc :start options))
                (make-instance ',sketch-name ,@initargs))))
           ,@(cdr (assoc :quit options))
           (values))
         (values ',name ',toplevel-name)))))
