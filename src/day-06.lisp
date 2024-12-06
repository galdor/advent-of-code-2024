(defpackage :aoc2024-06
  (:use :cl :aoc2024-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2024-06)

(defvar *lines* (input-file-lines 6))

(define-condition guard-loop ()
  ((steps
    :initarg :steps
    :accessor guard-loop-steps)))

(defun solve-1 ()
  (let ((map (parse-map *lines*)))
    (move-guard map)
    (count-visited-tiles map)))

(defun solve-2 ()
  (let* ((map (parse-map *lines*))
         (ref-map (copy-map map))
         (initial-position (initial-guard-position map))
         (loops nil))
    (move-guard ref-map)
    (dotiles (position ref-map)
      (when (and (char= (tile position ref-map) #\^)
                 (not (equal position initial-position)))
        (let ((map2 (copy-map map)))
          (setf (tile position map2) #\O)
          (handler-case
              (move-guard map2)
            (guard-loop (condition)
              (push (guard-loop-steps condition) loops))))))
    (length (remove-duplicates loops :test #'equal))))

(defun parse-map (lines)
  (make-array (list (length (car lines))
                    (length lines))
              :element-type 'character
              :initial-contents lines))

(defun copy-map (array)
  (let ((dimensions (array-dimensions array)))
    (adjust-array (make-array dimensions
                              :element-type (array-element-type array)
                              :adjustable t :displaced-to array)
                  dimensions)))

(defun display-map (map &optional (stream *standard-output*))
  (dotimes (y (array-dimension map 0))
    (dotimes (x (array-dimension map 1))
      (write-char (aref map y x) stream))
    (terpri)))

(defun tile (position map)
  (aref map (cdr position) (car position)))

(defun (setf tile) (value position map)
  (setf (aref map (cdr position) (car position)) value))

(defmacro dotiles ((position map &optional result) &body body)
  (let ((map-var (gensym "MAP-"))
        (x (gensym "X-"))
        (y (gensym "Y-")))
    `(let ((,map-var ,map))
       (dotimes (,y (array-dimension ,map-var 0))
         (dotimes (,x (array-dimension ,map-var 1))
           (let ((,position (cons ,x ,y)))
             ,@body)))
       ,result)))

(defun count-visited-tiles (map)
  (let ((count 0))
    (dotiles (position map count)
      (when (char= (tile position map) #\^)
        (incf count)))))

(defun initial-guard-position (map)
  (dotiles (position map)
    (when (char= (tile position map) #\^)
      (return-from initial-guard-position position))))

(defun move-guard (map)
  (do ((position (initial-guard-position map))
       (direction :up)
       (steps nil)
       (step-table (make-hash-table :test #'equal)))
      (nil)
    (let ((next-position (next-position position direction)))
      (unless (valid-position next-position map)
        (return-from move-guard nil))
      (ecase (tile next-position map)
        ((#\. #\^)
         (setf (tile next-position map) #\^)
         (let ((step (cons position direction)))
           (when (gethash step step-table)
             (signal 'guard-loop :steps steps))
           (setf (gethash step step-table) t)
           (push step steps))
         (setf position next-position))
        ((#\# #\O)
         (setf direction (turn-right direction)))))))

(defun valid-position (position map)
  (and (<= 0 (car position) (1- (array-dimension map 1)))
       (<= 0 (cdr position) (1- (array-dimension map 0)))))

(defun next-position (position direction)
  (let ((x (car position))
        (y (cdr position)))
    (ecase direction
      (:up
       (cons x (1- y)))
      (:right
       (cons (1+ x) y))
      (:down
       (cons x (1+ y)))
      (:left
       (cons (1- x) y)))))

(defun turn-right (direction)
  (ecase direction
    (:up
     :right)
    (:right
     :down)
    (:down
     :left)
    (:left
     :up)))
