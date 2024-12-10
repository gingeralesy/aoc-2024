(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/10

(defun day10-data ()
  (declare (optimize (speed 3)))
  (let ((map (make-queue))
        (width 0)
        (height 0))
    (declare (type (unsigned-byte 32) width height))
    (do-file (line "day10.txt" (values
                                (make-array `(,height ,width)
                                            :element-type '(integer 0 9)
                                            :initial-contents (queue-as-list map))
                                width height))
      (when (< width (length line)) (setf width (length line)))
      (loop for i from 0 below width
            collect (char->digit (char line i)) into row
            finally (queue-push row map))
      (incf height))))

(defun day10-count-routes-from (x y map width height visited)
  (declare (type (simple-array (integer 0 9) (* *)) map))
  (declare (type (unsigned-byte 32) x y width height))
  (declare (type (or boolean (simple-array boolean (* *))) visited))
  (declare (optimize (speed 3)))
  (let ((visited (if (eql visited T)
                     (make-array `(,height width) :element-type 'boolean
                                                  :initial-element NIL)
                     visited)))
    (when visited
      (setf (aref visited y x) T))
    (unless (< (aref map y x) 9) (return-from day10-count-routes-from 1))
    (loop with score of-type (unsigned-byte 32) = 0
          with next = (1+ (aref map y x))
          for next-x of-type (signed-byte 32) in (list (1- x) (1+ x) x x)
          for next-y of-type (signed-byte 32) in (list y y (1- y) (1+ y))
          when (and (<= 0 next-x) (< next-x width)
                    (<= 0 next-y) (< next-y height)
                    (= next (aref map next-y next-x))
                    (or (null visited) (null (aref visited next-y next-x))))
          do (incf score (day10-count-routes-from next-x next-y map width height visited))
          finally (return score))))

(defun day10-count-routes (map width height unique)
  (declare (type (simple-array (integer 0 9) (* *)) map))
  (declare (type (unsigned-byte 32) width height))
  (declare (type boolean unique))
  (declare (optimize (speed 3)))
  (let ((total 0))
    (declare (type (unsigned-byte 32) total))
    (dotimes (start-y height total)
      (dotimes (start-x width)
        (when (zerop (aref map start-y start-x))
          (incf total (day10-count-routes-from start-x start-y map width height (not unique))))))))

(defun d10p1 ()
  (declare (optimize (speed 3)))
  (multiple-value-bind (map width height) (day10-data)
    (day10-count-routes map width height NIL)))

;; 496

(defun d10p2 ()
  (declare (optimize (speed 3)))
  (multiple-value-bind (map width height) (day10-data)
    (day10-count-routes map width height T)))

;; 1120
