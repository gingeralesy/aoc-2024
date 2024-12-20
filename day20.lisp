(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/20

(defparameter *day20-point-cache*
  (make-array '(3 141 141) :element-type '(or null day20-point) :initial-element NIL))

;; (progn
;;   (dotimes (z 3)
;;     (dotimes (y 141)
;;       (dotimes (x 141)
;;         (setf (aref *day20-point-cache* z y x) (%day20-point x y z))))))

(defstruct (day20-point (:constructor %day20-point (x y z)))
  (x 0 :type (unsigned-byte 32))
  (y 0 :type (unsigned-byte 32))
  (z 0 :type (unsigned-byte 32)))

(defun day20-point (x y z)
  (let ((point (aref *day20-point-cache* z y x)))
    (unless point
      (setf point (%day20-point x y z))
      (setf (aref *day20-point-cache* z y x) point))
    point))

(defmethod day20-point-dist ((point-a day20-point) (point-b day20-point))
  (declare (optimize (speed 3)))
  (+ (abs (- (day20-point-x point-b) (day20-point-x point-a)))
     (abs (- (day20-point-y point-b) (day20-point-y point-a)))))

(defmethod day20-point-neighbours ((point day20-point) map width height)
  (declare (type (simple-array boolean (3 * *)) map))
  (with-slots (x y z) point
    (loop for dx in '(-1  1  0  0)
          and dy in '( 0  0 -1  1)
          for x1 = (+ x dx)
          and y1 = (+ y dy)
          for z1 = (if (zerop z) (if (aref map 0 y1 x1) 1 0) 2)
          when (and (<= 0 x1) (<= 0 y1) (< x1 width) (< y1 height)
                    (not (aref map z1 y1 x1)))
          collect (day20-point x1 y1 z1) into neighbours
          finally (return neighbours))))

(defun day20-data ()
  (declare (optimize (speed 3)))
  (let ((walls (make-queue))
        (width 0)
        (height 0)
        (start NIL)
        (end NIL))
    (declare (type (unsigned-byte 32) width height))
    (flet ((construct-map (walls)
             (loop with map = (make-array `(3 ,height ,width) :element-type 'boolean
                                                              :initial-element NIL)
                   until (queue-empty-p walls)
                   do (with-slots (x y z) (queue-pop walls)
                        (setf (aref map 0 y x) T)
                        (setf (aref map 2 y x) T))
                   finally (return map))))
      (do-file (line "day20.txt" (values
                                  start end
                                  (construct-map walls)
                                  width height))
        (when (< width (length line)) (setf width (length line)))
        (loop for x from 0 below width
              for ch = (char line x)
              do (ecase ch
                   (#\.)
                   (#\# (queue-push (day20-point x height 0) walls))
                   (#\S (setf start (day20-point x height 0)))
                   (#\E (setf end (day20-point x height 2))))
              finally (incf height))))))

(defun day20-solve (start end map width height)
  (declare (type (simple-array boolean (3 * *)) map))
  (let ((f-score (make-array `(3 ,height ,width) :element-type '(unsigned-byte 32)
                                                 :initial-element #xffffffff))
        (g-score (make-array `(3 ,height ,width) :element-type '(unsigned-byte 32)
                                                 :initial-element #xffffffff))
        (came-from (make-array `(3 ,height ,width) :element-type '(or null day20-point)
                                                   :initial-element NIL))
        (queued (make-array `(3 ,height ,width) :element-type 'boolean
                                                :initial-element NIL)))
    (labels ((f-score (point)
               (with-slots (x y z) point (aref f-score z y x)))
             (g-score (point)
               (with-slots (x y z) point (aref g-score z y x)))
             (push-node (point queue)
               (with-slots (x y z) point
                 (cond
                   ((aref queued z y x)
                    (heap-update queue point))
                   (T
                    (heap-push queue point)
                    (setf (aref queued z y x) T)))))
             (pop-node (queue)
               (let ((point (heap-pop queue)))
                 (with-slots (x y z) point
                   (setf (aref queued z y x) NIL))
                 point))
             (calculate-path (end-point)
               (loop with cheat = NIL
                     for point = end-point then prev
                     for prev = (with-slots (x y z) point (aref came-from z y x))
                     when (= 1 (day20-point-z point)) do (setf cheat point)
                     collect point into path
                     while prev
                     count point into length
                     finally (return (values length cheat path)))))
      (let ((open-set (make-heap #'(lambda (point-a point-b)
                                     (< (f-score point-a) (f-score point-b)))
                                 :test #'equalp :element-type '(or null day20-point))))
        (with-slots (x y z) start
          (setf (aref g-score z y x) 0)
          (setf (aref f-score z y x) (day20-point-dist start end)))
        (push-node start open-set)
        (loop while (< 0 (heap-size open-set))
              for current = (pop-node open-set)
              when (with-slots (x y) current
                     (and (= x (day20-point-x end)) (= y (day20-point-y end))))
              do (return (calculate-path current))
              do (loop for neighbour in (day20-point-neighbours current map width height)
                       for tentative-g = (1+ (g-score current))
                       do (when (< tentative-g (g-score neighbour))
                            (with-slots (x y z) neighbour
                              (setf (aref came-from z y x) current)
                              (setf (aref g-score z y x) tentative-g)
                              (setf (aref f-score z y x)
                                    (+ tentative-g (day20-point-dist neighbour end)))
                              (push-node neighbour open-set)))))))))


(defun d20p1 (&optional (limit 100))
  ;; TODO: There's no point in re-calculating the route at the third floor.
  (multiple-value-bind (start end map width height) (day20-data)
    (let ((base-length (day20-solve (day20-point (day20-point-x start) (day20-point-y start) 2)
                                    end map width height)))
      (loop for neighbour in (day20-point-neighbours end map width height)
            do (with-slots (x y) neighbour
                 (setf (aref map 0 y x) T)
                 (setf (aref map 1 y x) T)))
      (loop with savings = (make-array base-length :element-type '(unsigned-byte 32)
                                                   :initial-element 0)
            for (length cheat-point) = (multiple-value-list
                                        (day20-solve start end map width height))
            while (and length (< length base-length))
            for save = (- base-length length)
            while (or (zerop limit) (<= limit save))
            do (incf (aref savings save))
            when cheat-point do (with-slots (x y z) cheat-point (setf (aref map z y x) T))
            count length into cheat-paths
            finally (return (values cheat-paths (loop for i from 0 below base-length
                                                      when (< 0 (aref savings i))
                                                      collect (cons i (aref savings i)))))))))

;; 1499

;; TODO: Part 2 requires major refactoring.
;; TODO: Honestly, might rewrite this whole thing.
;;       1. Calculate the route at the top floor.
;;       2. Stop approximating on the top floor, you always know the exact score.
;;       3. The n-tiles from the end during in-between floors are all known as well.
