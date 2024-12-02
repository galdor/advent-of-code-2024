(defpackage :aoc2024-02
  (:use :cl :aoc2024-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2024-02)

(defvar *lines* (input-file-lines 2))

(defparameter *dampener* nil)

(defun solve-1 ()
  (let ((reports (mapcar 'parse-report *lines*)))
    (count-if 'safe-report-p reports)))

(defun solve-2 ()
  (let ((reports (mapcar 'parse-report *lines*))
        (*dampener* t))
    (count-if 'safe-report-p reports)))

(defun parse-report (line)
  (mapcar 'parse-integer (core:split-string line #\Space)))

(defun safe-report-p (report)
  (labels ((safep (level report min max dampened)
             (or (null level)
                 (null report)
                 (and (<= min (- (car report) level) max)
                      (safep (car report) (cdr report) min max dampened))
                 (and *dampener*
                      (not dampened)
                      (cddr report)
                      (safep (car report) (cddr report) min max t)))))
    (or (safep (car report) (cdr report) 1 3 nil)
        (safep (car report) (cdr report) -3 -1 nil))))
