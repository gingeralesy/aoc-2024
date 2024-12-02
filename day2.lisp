(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/2

(defun day2-values ()
  "Gets the report lists of numbers in the input."
  (declare (optimize (speed 3)))
  (let ((parser (cl-ppcre:create-scanner "\\d+"))
        (reports (make-queue)))
    (do-file (line "day2.txt" (queue-as-list reports))
      (queue-push (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings parser line)) reports))))

(declaim (inline day2-safe-step-p))
(defun day2-safe-step-p (a b increasing-p)
  (declare (type (signed-byte 8) a b))
  (declare (type boolean increasing-p))
  (declare (optimize (speed 3)))
  (let ((change (- b a)))
    (not (or (and increasing-p (or (< change 1) (< 3 change)))
             (and (not increasing-p) (or (< -1 change) (< change -3)))))))

(defun day2-safe-report-p (report)
  (declare (type list report))
  (declare (optimize (speed 3)))
  (loop with increasing-p = (< (the (signed-byte 8) (car report))
                               (the (signed-byte 8) (cadr report)))
        for a of-type (signed-byte 8) in report
        for b of-type (signed-byte 8) in (rest report)
        for count of-type (signed-byte 8) from 0
        unless (day2-safe-step-p a b increasing-p)
        do (return (values NIL count increasing-p))
        finally (return (values T -1 increasing-p))))

(defun d2p1 ()
  (declare (optimize (speed 3)))
  (loop for report in (day2-values)
        when (day2-safe-report-p report) count report))

;; 220

(defun d2p2 ()
  (declare (optimize (speed 3)))
  (loop for report of-type list in (day2-values)
        when (multiple-value-bind (safe-p index) (day2-safe-report-p report)
               (declare (type (signed-byte 8) index))
               (or safe-p
                   ;; Safe without the beginning level?
                   (and (= index 1) (day2-safe-report-p (rest report)))
                   ;; Safe without the preceding or following level?
                   (loop for level in report
                         for i of-type (signed-byte 8) from 0
                         unless (= i index)
                         collect level into preceding
                         unless (= i (1+ index))
                         collect level into following
                         finally (return (or (day2-safe-report-p preceding)
                                             (day2-safe-report-p following))))))
        count report))

;; 296
