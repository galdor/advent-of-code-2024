(defpackage :aoc2024-07
  (:use :cl :aoc2024-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2024-07)

(defvar *lines* (input-file-lines 7))

(defmacro do-input ((expected-value values lines) &body body)
  (let ((line (gensym "LINE-")))
    `(dolist (,line ,lines)
       (multiple-value-bind (,expected-value ,values)
           (parse-input-line ,line)
         ,@body))))

(defun parse-input-line (line)
  (let ((colon (position #\: line)))
    (values (parse-integer line :end colon)
            (mapcar #'parse-integer
                    (core:split-string line #\Space :start (+ colon 2))))))

(defun solve-1 ()
  (let ((sum 0))
    (do-input (expected-value values *lines*)
      (when (plusp (count-valid-equations expected-value values '(+ *)))
        (incf sum expected-value)))
    sum))

(defun solve-2 ()
  (let ((sum 0))
    (do-input (expected-value values *lines*)
      (when (plusp (count-valid-equations expected-value values '(+ * ||)))
        (incf sum expected-value)))
    sum))

(defun count-valid-equations (expected-value values operators)
  (let ((nb-valid 0))
    (mapc-equation-permutations
     (lambda (equation)
       (when (= (eval-equation equation) expected-value)
         (incf nb-valid)))
     values operators)
    nb-valid))

(defun mapc-equation-permutations (function values operators)
  (mapc-permutations (lambda (operators)
                       (funcall function (equation values operators)))
                     (1- (length values))
                     operators))

(defun mapc-permutations (function n values)
  (labels ((call (n list)
             (if (zerop n)
                 (funcall function list)
                 (mapc (lambda (value)
                         (call (1- n) (cons value list)))
                       values))))
    (call n nil)))

(defun equation (values operators)
  (if (cdr values)
      (cons (car values)
            (cons (car operators)
                  (equation (cdr values) (cdr operators))))
      (list (car values))))

(defun eval-equation (equation)
  (if (cdr equation)
      (let ((arg1 (first equation))
            (op (second equation))
            (arg2 (third equation)))
        (let ((v (ecase op
                   ((+ *)
                    (funcall op arg1 arg2))
                   (||
                    (+ (* arg1 (expt 10 (1+ (floor (log arg2 10))))) arg2)))))
          (eval-equation (cons v (cdddr equation)))))
      (car equation)))
