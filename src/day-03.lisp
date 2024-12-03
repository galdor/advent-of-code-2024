(defpackage :aoc2024-03
  (:use :cl :aoc2024-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2024-03)

(defvar *data* (input-file-data 3))

(defun solve-1 ()
  (let ((program (parse-program *data*))
        (sum 0))
    (dolist (instruction program sum)
      (incf sum (eval-instruction instruction)))))

(defun solve-2 ()
  (let ((program (parse-program *data* :conditionals t))
        (sum 0))
    (dolist (instruction program sum)
      (incf sum (eval-instruction instruction)))))

(defun parse-program (string &key conditionals)
  (do ((instructions nil)
       (disabled nil)
       (i 0))
      ((>= i (length string))
       (nreverse instructions))
    (catch 'no-match
      (labels ((accept (value)
                 (etypecase value
                   (string
                    (when (core:string-starts-with string value :start1 i)
                      (incf i (length value))
                      value))
                   ((or symbol function)
                    (when (and (< i (length string))
                               (funcall value (char string i)))
                      (prog1 (char string i) (incf i))))))
               (expect (value)
                 (unless (accept value)
                   (incf i)
                   (throw 'no-match nil)))
               (expect+ (predicate)
                 (let ((match (make-array 0 :element-type 'character
                                            :adjustable t :fill-pointer 0)))
                   (loop
                     (let ((c (accept predicate)))
                       (cond
                         (c
                          (vector-push-extend c match))
                         (t
                          (unless (plusp (length match))
                            (throw 'no-match nil))
                          (return match))))))))
        (cond
          ((accept "do()")
           (when conditionals
             (setf disabled nil)))
          ((accept "don't()")
           (when conditionals
             (setf disabled t)))
          ((accept "mul(")
           (unless disabled
             (let (n1 n2)
               (setf n1 (parse-integer (expect+ 'digit-char-p)))
               (expect ",")
               (setf n2 (parse-integer (expect+ 'digit-char-p)))
               (expect ")")
               (push (list :mul n1 n2) instructions))))
          (t
           (incf i)))))))

(defun eval-instruction (instruction)
  (ecase (car instruction)
    (:mul
     (destructuring-bind (arg1 arg2) (cdr instruction)
       (* arg1 arg2)))))
