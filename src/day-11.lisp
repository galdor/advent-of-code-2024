(defpackage :aoc2024-11
  (:use :cl :aoc2024-utils)
  (:export
   #:*data*
   #:solve-1
   #:solve-2))

(in-package :aoc2024-11)

(defvar *data* (string-trim (string #\Newline) (input-file-data 11)))

(defun parse-stones (data)
  (mapcar #'parse-integer (core:split-string data #\Space)))

(defun solve-1 ()
  (count-stones (parse-stones *data*) 25))

(defun solve-2 ()
  (count-stones (parse-stones *data*) 75))

(defun count-stones (stones n)
  (let ((stone-counts (make-hash-table :test #'equal)))
    (labels ((blink (stone)
               (if (zerop stone)
                   (list 1)
                   (let ((nb-digits (count-digits stone)))
                     (if (evenp nb-digits)
                         (split-stone stone nb-digits)
                         (list (* stone 2024))))))
             (count-stone (stone n)
               (if (zerop n)
                   1
                   (let ((key (cons stone n)))
                     (or (gethash key stone-counts)
                         (setf (gethash key stone-counts)
                               (count-stones (blink stone) (1- n)))))))
             (count-stones (stones n)
               (if (zerop n)
                   (length stones)
                   (let ((sum 0))
                     (dolist (stone stones sum)
                       (incf sum (count-stone stone n)))))))
      (count-stones stones n))))

(defun count-digits (n)
  (1+ (floor (log n 10))))

(defun split-stone (stone nb-digits)
  (assert (evenp nb-digits))
  (multiple-value-list
   (floor stone (expt 10 (/ nb-digits 2)))))
