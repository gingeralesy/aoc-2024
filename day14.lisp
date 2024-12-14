(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/14

(defstruct (day14-robot (:constructor day14-robot (px py vx vy)))
  (px 0 :type (signed-byte 32))
  (py 0 :type (signed-byte 32))
  (vx 0 :type (signed-byte 32))
  (vy 0 :type (signed-byte 32)))

(defun day14-data ()
  (declare (optimize (speed 3)))
  (let ((parser (cl-ppcre:create-scanner "^p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)$"))
        (robots (make-queue))
        (count 0))
    (declare (type (unsigned-byte 32) count))
    (do-file (line "day14.txt" (values
                                (make-array count :element-type 'day14-robot
                                                  :initial-contents (queue-as-list robots))
                                count))
      (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings parser line)
        (declare (type (simple-vector *) groups))
        (unless match (error "Invalid line: ~a" line))
        (let ((px (parse-integer (svref groups 0)))
              (py (parse-integer (svref groups 1)))
              (vx (parse-integer (svref groups 2)))
              (vy (parse-integer (svref groups 3))))
          (queue-push (day14-robot px py vx vy) robots)
          (incf count))))))

(defun d14p1 (&optional (width 101) (height 103) (time 100))
  (multiple-value-bind (robots count) (day14-data)
    (loop with mid-x = (floor width 2)
          and mid-y = (floor height 2)
          for i from 0 below count
          for robot = (aref robots i)
          for x = (mod (+ (day14-robot-px robot) (* (day14-robot-vx robot) time)) width)
          and y = (mod (+ (day14-robot-py robot) (* (day14-robot-vy robot) time)) height)
          when (and (< x mid-x) (< y mid-y)) count robot into north-west
          when (and (< mid-x x) (< y mid-y)) count robot into north-east
          when (and (< x mid-x) (< mid-y y)) count robot into south-west
          when (and (< mid-x x) (< mid-y y)) count robot into south-east
          ;; do (format T "~&~a. ~a,~a~%" (1+ i) x y)
          finally (return (prog1 (* north-west north-east south-west south-east))))))

;; 229421808

(defun day14-print-map (stream map width height)
  (let ((found-p NIL))
    (dotimes (y height)
      (format stream "~&")
      (let ((in-a-row 0))
        (dotimes (x width)
          (if (gethash (cons x y) map)
              (incf in-a-row)
              (setf in-a-row 0))
          (when (and (not found-p) (< 10 in-a-row))
            (setf found-p T)
            (break))
          (format stream "~c " (if (gethash (cons x y) map) #\# #\.))))
      (format stream "~%"))))

(defun d14p2 (stream time &optional (width 101) (height 103))
  (multiple-value-bind (robots count) (day14-data)
    (loop with map = (make-hash-table :test 'equal)
          and mid-x = (floor width 2)
          and mid-y = (floor height 2)
          for i from 0 below count
          for robot = (aref robots i)
          for x = (mod (+ (day14-robot-px robot) (* (day14-robot-vx robot) time)) width)
          and y = (mod (+ (day14-robot-py robot) (* (day14-robot-vy robot) time)) height)
          do (setf (gethash (cons x y) map) T)
          finally (progn
                    (format stream "~&After ~a seconds~%" time)
                    (day14-print-map stream map width height)))))

;; 6577
