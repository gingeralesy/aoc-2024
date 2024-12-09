(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/9

(defun day9-data ()
  (declare (optimize (speed 3)))
  (let* ((line (with-local-file (stream "day9.txt")
                 (the (simple-array character (*)) (read-clean-line stream))))
         (count (ceiling (length line) 2))
         (block-sizes (make-array count :element-type '(integer 0 9) :initial-element 0))
         (free-space (make-array count :element-type '(integer 0 9) :initial-element 0)))
    (dotimes (i count)
      (let ((index (* i 2)))
        (setf (aref block-sizes i) (char->digit (char line index)))
        (unless (< 0 (aref block-sizes i)) (error "Zero block size: ~a" i))
        (when (< i (1- count))
          (setf (aref free-space i) (char->digit (char line (1+ index)))))))
    (values block-sizes free-space count)))

(defun d9p1 ()
  (multiple-value-bind (block-sizes free-space count) (day9-data)
    (declare (type (simple-array (integer 0 9) (*)) block-sizes free-space))
    (declare (type (unsigned-byte 32) count))
    (loop with sum = 0
          with current = 0
          with to-move = (1- count)
          with pos = 0
          while (and (< current count)
                     (or (< current to-move)
                         (and (= current to-move) (< 0 (aref block-sizes to-move)))))
          do (cond
               ((< 0 (aref block-sizes current))
                (incf sum (* pos current))
                (decf (aref block-sizes current))
                (incf pos))
               ((< 0 (aref free-space current))
                (incf sum (* pos to-move))
                (decf (aref block-sizes to-move))
                (unless (< 0 (aref block-sizes to-move))
                  (decf to-move))
                (decf (aref free-space current))
                (unless (< 0 (aref free-space current))
                  (incf current))
                (incf pos))
               (T (incf current)))
          finally (return sum))))

;; 6359213660505

(defun d9p2 ()
  (multiple-value-bind (block-sizes free-space count) (day9-data)
    (declare (type (simple-array (integer 0 9) (*)) block-sizes free-space))
    (declare (type (unsigned-byte 32) count))
    (let ((sum 0)
          (blocks-moved (make-array count :element-type 'boolean :initial-element NIL))
          (space-after (make-array count :element-type '(integer 0 9) :initial-element 0)))
      (dotimes (i count) (setf (aref space-after i) (aref free-space i)))
      (loop for to-move from (1- count) above 0
            for size = (aref block-sizes to-move)
            for move-to = (dotimes (i to-move count)
                            (unless (< (aref space-after i) size)
                              (return i)))
            when (< move-to count)
            do (let ((pos (+ (aref block-sizes move-to) ;; Block here
                             (- (aref free-space move-to) ;; Blocks moved here
                                (aref space-after move-to)))))
                 (dotimes (i move-to) ;; Blocks before here
                   (incf pos (+ (aref block-sizes i) (aref free-space i))))
                 (decf (aref space-after move-to) size)
                 (dotimes (i size)
                   (incf sum (* to-move (+ pos i))))
                 (setf (aref blocks-moved to-move) T)))
      (let ((pos 0))
        (dotimes (i count)
          (unless (aref blocks-moved i)
            (dotimes (j (aref block-sizes i))
              (incf sum (* i (+ j pos)))))
          (incf pos (+ (aref block-sizes i) (aref free-space i)))))
      sum)))

;; 6381624803796
