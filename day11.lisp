(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/11

(defun day11-data ()
  (declare (optimize (speed 3)))
  (mapcar #'parse-integer
          (cl-ppcre:all-matches-as-strings
           "\\d+"
           (with-local-file (stream "day11.txt")
             (the (simple-array character (*)) (read-clean-line stream))))))

(defun day11-digit-count (number)
  (declare (type (unsigned-byte 62) number))
  (declare (optimize (speed 3)))
  (loop for n = number then (floor n 10)
        count n into count of-type (unsigned-byte 32)
        until (< n 10)
        finally (return count)))

(defun d11p1 (&optional (blinks 25))
  (declare (type (unsigned-byte 8) blinks))
  (declare (optimize (speed 3)))
  ;; This is the slow naïve approach.
  (loop repeat blinks
        for prev = (day11-data) then (queue-as-list next)
        for next = (make-queue)
        do (loop for number of-type (unsigned-byte 62) in prev
                 for digits of-type (unsigned-byte 32) = (day11-digit-count number)
                 do (cond
                      ((zerop number) (queue-push 1 next))
                      ((zerop (mod digits 2))
                       (let ((mul (expt 10 (floor digits 2))))
                         (queue-push (floor number mul) next)
                         (queue-push (mod number mul) next)))
                      (T (queue-push (the (unsigned-byte 62) (* number 2024)) next))))
        finally (return (queue-length next))))

;; 183248

(defun d11p2 (&optional (blinks 75))
  (let ((numbers-a (make-hash-table))
        (numbers-b (make-hash-table)))
    (loop for number in (day11-data)
          do (incf (gethash number numbers-a 0)))
    (dotimes (blink blinks)
      (loop with prev = (if (zerop (mod blink 2)) numbers-a numbers-b)
            with next = (if (zerop (mod blink 2)) numbers-b numbers-a)
            for number being the hash-keys of prev
            for count being the hash-values of prev
            for digits = (day11-digit-count number)
            unless (zerop count)
            do (cond
                 ((zerop number) (incf (gethash 1 next 0) count))
                 ((zerop (mod digits 2))
                  (let ((mul (expt 10 (floor digits 2))))
                    (incf (gethash (floor number mul) next 0) count)
                    (incf (gethash (mod number mul) next 0) count)))
                 (T (incf (gethash (* number 2024) next 0) count)))
            do (setf (gethash number prev) 0)))
    (loop for count being the hash-values of (if (zerop (mod blinks 2)) numbers-a numbers-b)
          unless (zerop count) sum count)))

;; 218811774248729
