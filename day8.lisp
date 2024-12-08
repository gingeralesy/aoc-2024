(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/8

(defun day8-data ()
  (declare (optimize (speed 3)))
  (let ((antennas (make-hash-table))
        (map (make-queue))
        (width 0)
        (height 0))
    (declare (type (unsigned-byte 32) width height))
    (do-file (line "day8.txt" (values
                               antennas
                               (make-array `(,height ,width)
                                           :element-type 'character
                                           :initial-contents (queue-as-list map))
                               width height))
      (when (< width (length line))
        (setf width (length line)))
      (loop for i from 0 below width
            for ch = (char line i)
            for floor-p = (char= ch #\.)
            unless floor-p do (push (cons i height) (gethash ch antennas))
            collect ch into row
            finally (queue-push row map))
      (incf height))))

(defun day8-maybe-set (x y width height antinodes)
  (declare (type (signed-byte 32) x y))
  (declare (type (unsigned-byte 32) width height))
  (declare (type (simple-array boolean (* *)) antinodes))
  (declare (optimize (speed 3)))
  (when (and (and (<= 0 x) (<= 0 y) (< x width) (< y height))
             (not (aref antinodes y x))) ;; Already set?
    (setf (aref antinodes y x) T)
    T))

(defun day8-print-map (antinodes map width height)
  (dotimes (y height)
    (format T "~&")
    (dotimes (x width)
      (format T "~c" (if (and (aref antinodes y x) (char= #\. (aref map y x)))
                         #\# (aref map y x))))
    (format T "~%")))

(defun d8p1 ()
  (declare (optimize (speed 3)))
  (multiple-value-bind (antennas map width height) (day8-data)
    (declare (ignore map))
    (loop with antinodes = (make-array `(,height ,width) :element-type 'boolean
                                                         :initial-element NIL)
          with count of-type (unsigned-byte 32) = 0
          ;; for frequency being the hash-keys of antennas
          for locations being the hash-values of antennas
          do (loop for current = locations then (rest current)
                   while current
                   for (ax . ay) of-type ((unsigned-byte 32) . (unsigned-byte 32)) = (car current)
                   do (loop for (bx . by) of-type ((unsigned-byte 32) . (unsigned-byte 32))
                            in (rest current)
                            when (day8-maybe-set (+ bx (- bx ax)) (+ by (- by ay))
                                                 width height antinodes)
                            do (incf count)
                            when (day8-maybe-set (+ ax (- ax bx)) (+ ay (- ay by))
                                                 width height antinodes)
                            do (incf count)))
          finally (return count))))

;; 273

(defun day8-maybe-set-all (from-x from-y dx dy width height antinodes)
  (declare (type (unsigned-byte 32) from-x from-y width height))
  (declare (type (signed-byte 32) dx dy))
  (declare (type (simple-array boolean (* *)) antinodes))
  (declare (optimize (speed 3)))
  (loop with count of-type (unsigned-byte 32) = 0
        for x of-type (signed-byte 32) = from-x then (+ x dx)
        for y of-type (signed-byte 32) = from-y then (+ y dy)
        while (and (<= 0 x) (<= 0 y) (< x width) (< y height))
        do (unless (aref antinodes y x) ;; Already set?
             (setf (aref antinodes y x) T)
             (incf count))
        finally (return count))) ;; (day8-print-map antinodes map width height)

(defun d8p2 ()
  (declare (optimize (speed 3)))
  (multiple-value-bind (antennas map width height) (day8-data)
    (declare (ignore map))
    (loop with antinodes = (make-array `(,height ,width) :element-type 'boolean
                                                         :initial-element NIL)
          with count of-type (unsigned-byte 32) = 0
          ;; for frequency being the hash-keys of antennas
          for locations being the hash-values of antennas
          do (loop for current = locations then (rest current)
                   while current
                   for (ax . ay) of-type ((unsigned-byte 32) . (unsigned-byte 32)) = (car current)
                   do (loop for (bx . by) of-type ((unsigned-byte 32) . (unsigned-byte 32))
                            in (rest current)
                            do (incf count (day8-maybe-set-all ax ay (- ax bx) (- ay by)
                                                               width height antinodes))
                            do (incf count (day8-maybe-set-all bx by (- bx ax) (- by ay)
                                                               width height antinodes))))
          finally (return (prog1 count))))) ;; (day8-print-map antinodes map width height)

;; 1017
