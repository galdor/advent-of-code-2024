(defpackage :aoc2024-08
  (:use :cl :aoc2024-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2024-08)

(defvar *lines* (input-file-lines 8))

(defun parse-map (lines)
  (make-array (list (length lines) (length (car lines)))
              :element-type 'character
              :initial-contents lines))

(defun parse-antennas (map)
  (let ((antennas nil))
    (dotimes (y (array-dimension map 0))
      (dotimes (x (array-dimension map 1))
        (let ((tile (aref map y x)))
          (unless (char= tile #\.)
            (let ((pair (assoc tile antennas)))
              (if pair
                  (rplacd pair (cons (point x y) (cdr pair)))
                  (push (list tile (point x y)) antennas)))))))
    antennas))

(defun solve-1 ()
  (let* ((map (parse-map *lines*))
         (antennas (parse-antennas map))
         (all-antinodes nil))
    (dolist (antenna antennas)
      (let ((antinodes (antinodes (car antenna) antennas map)))
        (setf all-antinodes (append all-antinodes antinodes))))
    (length (remove-duplicates all-antinodes :test 'point=))))

(defun solve-2 ()
  (let* ((map (parse-map *lines*))
         (antennas (parse-antennas map))
         (all-antinodes nil))
    (dolist (antenna antennas)
      (let ((antinodes
              (antinodes-with-resonant-harmonics (car antenna) antennas map)))
        (setf all-antinodes (append all-antinodes antinodes))))
    (length (remove-duplicates all-antinodes :test 'point=))))

(defun antinodes (antenna-tile antennas map)
  (let ((points (cdr (assoc antenna-tile antennas)))
        (antinodes nil))
    (dolist (ref-point points antinodes)
      (dolist (point points)
        (unless (point= point ref-point)
          (let* ((vec2 (point- point ref-point))
                 (antinode (point+ ref-point (inverse-vec2 vec2))))
            (when (valid-point-p antinode map)
              (push antinode antinodes))))))
    antinodes))

(defun antinodes-with-resonant-harmonics (antenna-tile antennas map)
  (let ((points (cdr (assoc antenna-tile antennas)))
        (antinodes nil))
    (dolist (ref-point points antinodes)
      (dolist (point points)
        (unless (point= point ref-point)
          (do ((vec2 (point- point ref-point))
               (antinode ref-point (point+ antinode vec2)))
              ((not (valid-point-p antinode map))
               nil)
            (push antinode antinodes)))))
    antinodes))

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

(defun point- (p2 p1)
  (cons (- (point-x p2) (point-x p1))
        (- (point-y p2) (point-y p1))))

(defun point+ (p v)
  (point (+ (point-x p) (car v))
         (+ (point-y p) (cdr v))))

(defun inverse-vec2 (v)
  (cons (- (car v)) (- (cdr v))))

(defun valid-point-p (point map)
  (and (<= 0 (point-y point) (1- (array-dimension map 0)))
       (<= 0 (point-x point) (1- (array-dimension map 1)))))
