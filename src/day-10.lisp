(defpackage :aoc2024-10
  (:use :cl :aoc2024-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2024-10)

(defvar *lines* (input-file-lines 10))

(defvar *map* (parse-map *lines*))

(defun parse-map (lines)
  (let* ((height (length lines))
         (width (length (first lines)))
         (map (make-array (list height width) :element-type 'integer)))
    (do ((lines lines (cdr lines))
         (y 0 (1+ y)))
        ((null lines)
         map)
      (dotimes (x width)
        (setf (aref map y x)
              (- (char-code (aref (car lines) x)) (char-code #\0)))))))

(defun solve-1 ()
  (let ((sum 0))
    (dolist (trailhead (find-trailheads) sum)
      (incf sum (trailhead-score trailhead)))))

(defun solve-2 ()
  (let ((sum 0))
    (dolist (trailhead (find-trailheads) sum)
      (incf sum (trailhead-rating trailhead)))))

(defun find-trailheads ()
  (let ((trailheads nil))
    (dotimes (y (array-dimension *map* 1))
      (dotimes (x (array-dimension *map* 0))
        (let ((point (point x y)))
          (when (trailheadp point)
            (push point trailheads)))))
    trailheads))

(defstruct (point (:predicate pointp))
  (x 0 :type integer)
  (y 0 :type integer))

(defmethod print-object ((point point) stream)
  (print-unreadable-object (point stream :type t)
    (princ (point-x point) stream)
    (write-char #\Space stream)
    (princ (point-y point) stream)))

(defun point (x y)
  (make-point :x x :y y))

(defun point= (p1 p2)
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))

(defun point-height (point)
  (aref *map* (point-y point) (point-x point)))

(defun trailheadp (point)
  (= (point-height point) 0))

(defun trail-end-p (point)
  (= (point-height point) 9))

(defun point-neighbors (point)
  (let ((neighbors nil)
        (x (point-x point))
        (y (point-y point)))
    (when (> y 0)
      (push (point x (1- y)) neighbors))
    (when (< x (1- (array-dimension *map* 1)))
      (push (point (1+ x) y) neighbors))
    (when (< y (1- (array-dimension *map* 0)))
      (push (point x (1+ y)) neighbors))
    (when (> x 0)
      (push (point (1- x) y) neighbors))
    neighbors))

(defun next-trail-points (point)
  (let ((height (point-height point)))
    (delete-if-not (lambda (neighbor)
                     (= (point-height neighbor) (1+ height)))
                   (point-neighbors point))))

(defun trailhead-score (point)
  (assert (trailheadp point))
  (labels ((walk (points trail-ends)
             (cond
               ((null points)
                (length (remove-duplicates trail-ends :test 'point=)))
               ((trail-end-p (car points))
                (walk (cdr points)
                      (cons (car points) trail-ends)))
               (t
                (walk (append (next-trail-points (car points))
                              (cdr points))
                      trail-ends)))))
    (walk (list point) nil)))

(defun trailhead-rating (point)
  (assert (trailheadp point))
  (labels ((walk (points count)
             (cond
               ((null points)
                count)
               ((trail-end-p (car points))
                (walk (cdr points) (1+ count)))
               (t
                (walk (append (next-trail-points (car points))
                              (cdr points))
                      count)))))
    (walk (list point) 0)))
