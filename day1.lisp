(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/1

(defun day1-values (&optional counts-p)
  "Gets the left and right lists of numbers in the input."
  (declare (optimize (speed 3)))
  (let ((parser (cl-ppcre:create-scanner "\\d+"))
        (left ())
        (right (if counts-p (make-hash-table :test #'= :hash-function #'identity) ())))
    (do-file (line "day1.txt" (values left right))
      (let ((nums (cl-ppcre:all-matches-as-strings parser line)))
        (push (parse-integer (car nums)) left)
        (if counts-p
            (let ((num (parse-integer (cadr nums))))
              (setf (gethash num right)
                    (1+ (the (unsigned-byte 32) (gethash num right 0)))))
            (push (parse-integer (cadr nums)) right))))))

(defun d1p1 ()
  (declare (optimize (speed 3)))
  (multiple-value-bind (left right) (day1-values)
    (declare (type list left right))
    (loop for a of-type (signed-byte 32) in (sort left #'<)
          for b of-type (signed-byte 32) in (sort right #'<)
          collecting (abs (- a b)) into diffs
          finally (return (apply #'+ diffs)))))

;; 1941353

(defun d1p2 ()
  (declare (optimize (speed 3)))
  (multiple-value-bind (left right) (day1-values T)
    (declare (type list left))
    (declare (type hash-table right))
    (loop with sum of-type (unsigned-byte 32) = 0
          for num of-type (unsigned-byte 32) in (sort left #'<)
          for count of-type (unsigned-byte 32) = (gethash num right 0)
          do (incf sum (* num count))
          finally (return sum))))

;; 22539317
