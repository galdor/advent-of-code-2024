(defpackage :aoc2024-05
  (:use :cl :aoc2024-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2024-05)

(defvar *lines* (input-file-lines 5))

(defparameter *ordering-rules* nil)

(defmacro with-input ((updates lines) &body body)
  `(multiple-value-bind (*ordering-rules* ,updates)
       (parse-input ,lines)
     ,@body))

(defun solve-1 ()
  (with-input (updates *lines*)
    (let ((sum 0))
      (dolist (update updates sum)
        (when (update-ordered update)
          (incf sum (middle-page update)))))))

(defun solve-2 ()
  (with-input (updates *lines*)
    (let ((sum 0))
      (dolist (update updates sum)
        (unless (update-ordered update)
          (incf sum (middle-page (sort update 'page-before))))))))

(defun parse-input (lines)
  (let ((ordering-rules (make-hash-table))
        (updates nil))
    (dolist (line lines)
      (cond
        ((find #\| line)
         (let* ((parts (core:split-string line "|"))
                (page1 (parse-integer (first parts)))
                (page2 (parse-integer (second parts))))
           (push page2 (gethash page1 ordering-rules))))
        ((find #\, line)
         (let ((pages (mapcar 'parse-integer (core:split-string line #\,))))
           (push (coerce pages 'vector) updates)))))
    (values ordering-rules (nreverse updates))))

(defun page-before (page1 page2)
  (member page2 (gethash page1 *ordering-rules*)))

(defun update-ordered (update)
  (dotimes (i (1- (length update)) t)
    (let ((page1 (aref update i))
          (page2 (aref update (1+ i))))
      (unless (page-before page1 page2)
        (return-from update-ordered nil)))))

(defun middle-page (update)
  (aref update (floor (length update) 2)))
