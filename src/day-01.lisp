(defpackage :aoc2024-01
  (:use :cl :aoc2024-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2024-01)

(defvar *lines* (input-file-lines 1))

(defun solve-1 ()
  (let ((input (mapcar 'parse-location-id-pair *lines*))
        (sum 0))
    (mapc (lambda (n1 n2)
            (incf sum (distance n1 n2)))
          (sort (mapcar #'car input) #'<)
          (sort (mapcar #'cdr input) #'<))
    sum))

(defun solve-2 ()
  (let* ((input (mapcar 'parse-location-id-pair *lines*))
         (list-2 (mapcar #'cdr input)))
    (let ((sum 0))
      (dolist (entry input sum)
        (let ((n1 (car entry)))
          (incf sum (* n1 (count n1 list-2))))))))

(defun distance (location-1 location-2)
  (abs (- location-2 location-1)))

(defun parse-location-id-pair (line)
  (let ((first-space (position #\Space line))
        (last-space (position #\Space line :from-end t)))
    (cons (parse-integer line :start 0 :end first-space)
          (parse-integer line :start (1+ last-space)))))
