(defpackage :aoc2024-04
  (:use :cl :aoc2024-utils)
  (:export
   #:*characters*
   #:solve-1
   #:solve-2))

(in-package :aoc2024-04)

(defvar *characters*
  (let ((lines (input-file-lines 4)))
    (make-array (list (length lines)
                      (length (car lines)))
                :element-type 'character
                :initial-contents (input-file-lines 4))))

(defun solve-1 ()
  (length (find-word "XMAS" *characters*)))

(defun solve-2 ()
  (length (find-x-mas *characters*)))

(defun find-x-mas (characters)
  (let ((positions nil))
    (dotimes (i (array-dimension characters 0))
      (dotimes (j (array-dimension characters 1))
        (when (and (char= (aref characters i j) #\A)
                   (or (is-word-at "MAS" (1- i) (1- j) '( 1 .  1) characters)
                       (is-word-at "MAS" (1+ i) (1+ j) '(-1 . -1) characters))
                   (or (is-word-at "MAS" (1- i) (1+ j) '( 1 . -1) characters)
                       (is-word-at "MAS" (1+ i) (1- j) '(-1 .  1) characters)))
          (push (cons i j) positions))))
    positions))

(defun find-word (word characters)
  (let ((positions nil))
    (flet ((find-at (i j move)
             (when (is-word-at word i j move characters)
               (push (cons i j) positions))))
      (dotimes (i (array-dimension characters 0))
        (dotimes (j (array-dimension characters 1))
          (find-at i j '( 0 . -1))
          (find-at i j '( 1 . -1))
          (find-at i j '( 1 .  0))
          (find-at i j '( 1 .  1))
          (find-at i j '( 0 .  1))
          (find-at i j '(-1 .  1))
          (find-at i j '(-1 .  0))
          (find-at i j '(-1 . -1))))
      positions)))

(defun valid-position-p (i j characters)
  (and (<= 0 i (1- (array-dimension characters 0)))
       (<= 0 j (1- (array-dimension characters 1)))))

(defun is-word-at (word i j move characters)
  (dotimes (n (length word))
    (unless (and (valid-position-p i j characters)
                 (char= (aref characters i j) (char word n)))
      (return-from is-word-at))
    (incf i (car move))
    (incf j (cdr move)))
  t)
