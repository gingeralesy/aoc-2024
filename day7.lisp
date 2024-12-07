(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/7

(defun day7-data ()
  (declare (optimize (speed 3)))
  (let ((line-parser (cl-ppcre:create-scanner "^(\\d+):(( \\d+)+)$"))
        (num-parser (cl-ppcre:create-scanner "\\d+"))
        (equations (make-queue))
        (count 0))
    (declare (type (unsigned-byte 32) count))
    (do-file (line "day7.txt" (values (queue-as-list equations) count))
      (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings line-parser line)
        (declare (type (simple-vector 3) groups))
        (unless match (error "Invalid line: ~a" line))
        (queue-push (cons (parse-integer (svref groups 0))
                          (mapcar #'parse-integer
                                  (cl-ppcre:all-matches-as-strings num-parser (svref groups 1))))
                    equations)
        (incf count)))))

(defun day7-solve (result current values operations)
  (declare (optimize (speed 3)))
  (declare (type (unsigned-byte 62) result))
  (declare (type (unsigned-byte 62) current))
  (declare (type list values operations))
  (let ((next (car values)))
    (declare (type (or null (unsigned-byte 32)) next))
    (or (and next (<= current result)
             (dolist (op operations)
               (declare (type function op))
               (when (day7-solve result (funcall op current next) (rest values) operations)
                 (return T))))
        (and (null next) (= result current)))))

(defun d7p1 ()
  (declare (optimize (speed 3)))
  (loop with total of-type (unsigned-byte 62) = 0
        for (result . values) in (day7-data)
        when (day7-solve result (car values) (rest values) (list #'* #'+))
        do (incf total (the (unsigned-byte 62) result))
        finally (return total)))

;; 1289579105366

(defun day7-concat (a b)
  (declare (type (unsigned-byte 62) a b))
  (loop for mul of-type (unsigned-byte 62) = 10 then (* 10 mul)
        until (< b mul)
        finally (return (u62 (+ (u62 (* mul a)) b)))))

(defun d7p2 ()
  (declare (optimize (speed 3)))
  (loop with total of-type (unsigned-byte 62) = 0
        for (result . values) in (day7-data)
        when (day7-solve result (car values) (rest values) (list #'* #'+ #'day7-concat))
        do (incf total (the (unsigned-byte 62) result))
        finally (return total)))

;; 92148721834692
