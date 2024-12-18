(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/18

(defun day18-data (limit &optional test-p (width (if test-p 7 71)) (height (if test-p 7 71)))
  (declare (type (unsigned-byte 32) limit width height))
  (declare (optimize (speed 3)))
  (let ((parser (cl-ppcre:create-scanner "\\d+"))
        (coordinates (make-queue))
        (count 0))
    (declare (type (unsigned-byte 32) count))
    (do-file (line (if test-p "test.txt" "day18.txt"))
      (let* ((nums (cl-ppcre:all-matches-as-strings parser line))
             (x (parse-integer (first nums)))
             (y (parse-integer (second nums))))
        (unless (and (< x width) (< y height))
          (error "Cannot fit coordinate [~d,~d] into ~d,~d space" x y width height))
        (queue-push (cons x y) coordinates)
        (incf count)))
    (loop with map = (make-array `(,height ,width) :element-type 'boolean :initial-element NIL)
          repeat limit
          for (x . y) = (queue-pop coordinates)
          do (setf (aref map y x) T)
          do (decf count)
          finally (return (values map width height
                                  (make-array count :element-type 'cons
                                                    :initial-contents (queue-as-list coordinates))
                                  count)))))

(defun day18-solve (start-x start-y end-x end-y map width height &optional out-path)
  (declare (type (unsigned-byte 32) start-x start-y end-x end-y width height))
  (declare (type (simple-array boolean (* *)) map))
  (declare (type (or null (simple-array boolean (* *))) map out-path))
  (declare (optimize (speed 3)))
  (let* ((f-score (make-array `(,height ,width) :element-type '(unsigned-byte 32)
                                                :initial-element #xffffffff))
         (g-score (make-array `(,height ,width) :element-type '(unsigned-byte 32)
                                                :initial-element #xffffffff))
         (queued (make-array `(,height ,width) :element-type 'boolean :initial-element NIL))
         (came-from (make-array `(,height ,width) :element-type '(or null cons)
                                                  :initial-element NIL))
         (open-set (make-heap #'(lambda (a b)
                                  (< (aref f-score (cdr a) (car a))
                                     (aref f-score (cdr b) (car b))))
                              :test #'equal :element-type 'cons)))
    (flet ((manhattan (from-x from-y &optional (to-x end-x) (to-y end-y))
             (+ (abs (- to-x from-x)) (abs (- to-y from-y))))
           (neighbours (x y)
             (declare (type (unsigned-byte 32) x y))
             (loop for dx in '(-1  1  0  0)
                   and dy in '( 0  0 -1  1)
                   for x1 of-type (signed-byte 32) = (+ x dx)
                   and y1 of-type (signed-byte 32) = (+ y dy)
                   when (and (<= 0 x1) (<= 0 y1) (< x1 width) (< y1 height) (not (aref map y1 x1)))
                   collect (cons x1 y1)))
           (reconstruct-path ()
             (loop for x = end-x then (car prev)
                   and y = end-y then (cdr prev)
                   for prev = (aref came-from y x)
                   collect (cons x y) into path
                   when out-path do (setf (aref out-path y x) T)
                   while prev count prev into count
                   finally (return (values count path)))))
      (setf (aref f-score start-y start-x) 0)
      (setf (aref g-score start-y start-x) (manhattan start-x start-y))
      (heap-push open-set (cons start-x start-y))
      (setf (aref queued start-y start-x) T)
      (loop while (< 0 (heap-size open-set))
            for current = (heap-pop open-set)
            for (x . y) = current
            do (setf (aref queued y x) NIL)
            when (and (= x end-x) (= y end-y)) do (return (reconstruct-path))
            do (loop for neighbour in (neighbours x y)
                     for (next-x . next-y) = neighbour
                     for tentative-g = (1+ (aref g-score y x))
                     do (when (< tentative-g (aref g-score next-y next-x))
                          (setf (aref came-from next-y next-x) current)
                          (setf (aref g-score next-y next-x) tentative-g)
                          (setf (aref f-score next-y next-x)
                                (+ tentative-g (manhattan next-x next-y)))
                          (if (aref queued next-y next-x)
                              (heap-update open-set neighbour)
                              (heap-push open-set neighbour))))))))

(defun d18p1 (&optional test-p)
  (multiple-value-bind (map width height) (day18-data (if test-p 12 1024) test-p)
    (nth-value 0 (day18-solve 0 0 (if test-p 6 70) (if test-p 6 70) map width height))))

;; 318

(defun d18p2 (&optional test-p)
  (let ((limit (if test-p 12 1024)))
    (multiple-value-bind (map width height remaining count) (day18-data limit test-p)
      (loop with path = (make-array `(,height ,width) :element-type 'boolean :initial-element NIL)
            and first-p = T
            and stuck-p = NIL
            and end-x = (if test-p 6 70)
            and end-y = (if test-p 6 70)
            for i from 0 below count
            for byte = (aref remaining i)
            do (setf (aref map (cdr byte) (car byte)) T)
            when (or first-p (aref path (cdr byte) (car byte)))
            do (setf stuck-p (null (day18-solve 0 0 end-x end-y map width height path)))
            do (setf first-p NIL)
            until stuck-p
            finally (return (values byte (+ limit (1+ i))))))))

;; 56,29
