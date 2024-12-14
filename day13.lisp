(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/13

(defstruct (day13-button (:constructor day13-button (x y cost)))
  (x 0 :type (signed-byte 32))
  (y 0 :type (signed-byte 32))
  (cost 0 :type (unsigned-byte 8)))

(defstruct (day13-claw (:constructor day13-claw (button-a button-b prize-x prize-y)))
  (button-a NIL :type day13-button)
  (button-b NIL :type day13-button)
  (prize-x 0 :type (unsigned-byte 64))
  (prize-y 0 :type (unsigned-byte 64)))

(defun day13-data ()
  (declare (optimize (speed 3)))
  (flet ((test-line (line parser)
           (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings parser line)
             (unless match (error "Invalid line: ~a" line))
             groups)))
    (with-local-file (stream "day13.txt")
      (loop with button-parser = (cl-ppcre:create-scanner "^Button (A|B): X\\+(\\d+), Y\\+(\\d+)$")
            and prize-parser = (cl-ppcre:create-scanner "^Prize: X=(\\d+), Y=(\\d+)$")
            for button-a of-type (simple-vector 3)
            = (test-line (read-clean-line stream) button-parser)
            and button-b of-type (simple-vector 3)
            = (test-line (read-clean-line stream) button-parser)
            and prize of-type (simple-vector 2)
            = (test-line (read-clean-line stream) prize-parser)
            unless (string= "A" (the (simple-array character (1)) (svref button-a 0)))
            do (error "Invalid button: ~a" (svref button-a 0))
            unless (string= "B" (the (simple-array character (1)) (svref button-b 0)))
            do (error "Invalid button: ~a" (svref button-b 0))
            collect (day13-claw (day13-button (parse-integer (svref button-a 1))
                                              (parse-integer (svref button-a 2))
                                              3)
                                (day13-button (parse-integer (svref button-b 1))
                                              (parse-integer (svref button-b 2))
                                              1)
                                (parse-integer (svref prize 0))
                                (parse-integer (svref prize 1)))
            into claws
            count button-a into count
            until (eql (read-clean-line stream) :eof) ;; Empty line
            finally (return (values
                             (make-array count :element-type 'day13-claw :initial-contents claws)
                             count))))))

(defun day13-cheapest-path (claw &optional limit) ;; A*
  ;; Hmm.. This is slower than I expected. But it does give the result eventually.
  ;; It was actually faster to just loop down from the top with the expensive button until it's
  ;; possible to get there only pressing the cheaper button. Should've kept that solution.
  (let ((path (make-hash-table :test 'equal))
        (g-scores (make-hash-table :test 'equal))
        (f-scores (make-hash-table :test 'equal)))
    (labels ((past-p (x y)
               (or (< (day13-claw-prize-x claw) x)
                   (< (day13-claw-prize-y claw) y)))
             (heuristic-for (x y button)
               (if (past-p (+ (day13-claw-prize-x claw) x) (+ (day13-claw-prize-y claw) y))
                   #xffffffffffffffff
                   (* (max (ceiling (- (day13-claw-prize-x claw) x) (day13-button-x button))
                           (ceiling (- (day13-claw-prize-y claw) y) (day13-button-y button)))
                      (day13-button-cost button))))
             (heuristic (x y)
               (cond
                 ((and (= (day13-claw-prize-x claw) x)
                       (= (day13-claw-prize-y claw) y))
                  0)
                 ((past-p x y) #xffffffffffffffff)
                 (T (min (heuristic-for x y (day13-claw-button-a claw))
                         (heuristic-for x y (day13-claw-button-b claw))))))
             (reconstruct-path ()
               (loop for step = (gethash (cons (day13-claw-prize-x claw)
                                               (day13-claw-prize-y claw))
                                         path)
                     then (gethash (cons x y) path)
                     for ((x . y) . cost) = step
                     sum cost into total
                     when (= cost (day13-button-cost (day13-claw-button-a claw)))
                     count cost into presses-a
                     when (= cost (day13-button-cost (day13-claw-button-b claw)))
                     count cost into presses-b
                     until (and (zerop x) (zerop y))
                     finally (return (values total presses-a presses-b))))
             (g-score (x y) (if (past-p x y)
                                #xffffffffffffffff
                                (gethash (cons x y) g-scores #xffffffffffffffff)))
             (f-score (x y) (if (past-p x y)
                                #xffffffffffffffff
                                (gethash (cons x y) f-scores #xffffffffffffffff))))
      (setf (gethash '(0 . 0) g-scores) 0)
      (setf (gethash '(0 . 0) f-scores) (heuristic 0 0))
      (loop with buttons = `(,(day13-claw-button-a claw) ,(day13-claw-button-b claw))
            and queue = (make-queue '((0 . 0) . (0 . 0)))
            until (queue-empty-p queue)
            for ((x . y) . (presses-a . presses-b))
            = (pqueue-pop queue :test #'(lambda (a b)
                                          (< (f-score (caar a) (cdar a))
                                             (f-score (caar b) (cdar b)))))
            when (and (= x (day13-claw-prize-x claw))
                      (= y (day13-claw-prize-y claw)))
            do (return (reconstruct-path))
            do (loop for button in buttons
                     for presses in (list (cons (1+ presses-a) presses-b)
                                          (cons presses-a (1+ presses-b)))
                     for next-x = (+ x (day13-button-x button))
                     and next-y = (+ y (day13-button-y button))
                     and tentative-g = (+ (g-score x y) (day13-button-cost button))
                     do (when (and (or (not limit)
                                       (and (<= (car presses) limit) (<= (cdr presses) limit)))
                                   (< tentative-g (g-score next-x next-y)))
                          (let ((next (cons (cons next-x next-y) presses)))
                            (setf (gethash (car next) path)
                                  (cons (cons x y) (day13-button-cost button)))
                            (setf (gethash (car next) g-scores) tentative-g)
                            (setf (gethash (car next) f-scores)
                                  (u64 (+ tentative-g (heuristic next-x next-y))))
                            (when (and (< (gethash (car next) f-scores) #xffffffffffffffff)
                                       (not (find next (queue-as-list queue)
                                                  :test #'(lambda (a b)
                                                            (and (= (caar a) (caar b))
                                                                 (= (cdar a) (cdar b)))))))
                              (queue-push next queue)))))))))

(defun d13p1 ()
  (multiple-value-bind (claws count) (day13-data)
    (loop for i from 0 below count
          for claw = (aref claws i)
          ;; for (cost a b) = (multiple-value-list (day13-cheapest-path claw 100))
          ;; if cost
          ;; do (format T "~&~a. Cost: ~a; A: ~a times; B: ~a times~%" i cost a b)
          ;; else do (format T "~&~a. No prize~%" i)
          for cost = (day13-cheapest-path claw 100)
          when cost sum cost into total
          when cost count cost into prizes
          finally (return (values total prizes)))))

;; 33427

(defun d13p2 (&optional (prize-offset 10000000000000))
  ;; Euugh.. Fine. I'll solve it with math.
  ;; Now I have a headache but it works.
  (declare (type (unsigned-byte 62) prize-offset))
  ;; (declare (optimize (speed 3)))
  (multiple-value-bind (claws count) (day13-data)
    (declare (type (simple-array day13-claw (*)) claws))
    (declare (type (unsigned-byte 32) count))
    (loop for i from 0 below count
          for claw = (aref claws i)
          for button-a = (day13-claw-button-a claw)
          and button-b = (day13-claw-button-b claw)
          and x of-type (unsigned-byte 62) = (+ (day13-claw-prize-x claw) prize-offset)
          and y of-type (unsigned-byte 62) = (+ (day13-claw-prize-y claw) prize-offset)
          for ax of-type (unsigned-byte 32) = (day13-button-x button-a)
          and ay of-type (unsigned-byte 32) = (day13-button-y button-a)
          and bx of-type (unsigned-byte 32) = (day13-button-x button-b)
          and by of-type (unsigned-byte 32) = (day13-button-y button-b)
          for cost of-type (or null (unsigned-byte 62))
          = (multiple-value-bind (count-a remainder-a)
                (floor (the (signed-byte 64) (- (u62 (* bx y)) (u62 (* by x))))
                       (the (signed-byte 64) (- (* bx ay) (* by ax))))
              (when (and (<= 0 count-a) (zerop remainder-a))
                (multiple-value-bind (count-b remainder-b)
                    (floor (the (unsigned-byte 62) (- x (u62 (* ax count-a)))) bx)
                  (when (zerop remainder-b)
                    (+ (the (unsigned-byte 62)
                            (* (day13-button-cost button-a) count-a))
                       (the (unsigned-byte 62)
                            (* (day13-button-cost button-b) count-b)))))))
          when cost sum cost into total of-type (unsigned-byte 62)
          finally (return total))))

;; 91649162972270
