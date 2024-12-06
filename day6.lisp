(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/6

(defun day6-data ()
  (declare (optimize (speed 3)))
  (let ((map (make-queue))
        (start-x 0)
        (start-y 0)
        (width 0)
        (height 0))
    (declare (type (unsigned-byte 32) width height))
    (do-file (line "day6.txt" (values
                               start-x start-y
                               (make-array `(,height ,width)
                                           :element-type 'boolean
                                           :initial-contents (queue-as-list map))
                               width height))
      (when (< 0 (length line))
        (when (< width (length line)) (setf width (length line)))
        (loop for x from 0 below width
              for ch = (char line x)
              collect (ecase ch
                        (#\. NIL)
                        (#\# T)
                        (#\^
                          (setf start-x x)
                          (setf start-y height)
                          NIL))
              into row
              finally (queue-push row map))
        (incf height)))))

(defun day6-rotate (x y dir-x dir-y map width height)
  (declare (type (simple-array boolean (* *)) map))
  (declare (type (unsigned-byte 32) x y width height))
  (declare (type (integer -1 1) dir-x dir-y))
  (declare (optimize (speed 3)))
  (unless (or (= dir-x 0) (= dir-y 0))
    (error "Diagonal direction: (~a, ~a)" dir-x dir-y))
  (loop with dx = dir-x
        with dy = dir-y
        for next-x = (+ x dx)
        for next-y = (+ y dy)
        while (and (<= 0 next-x) (<= 0 next-y)
                   (< next-x width) (< next-y height)
                   (aref map next-y next-x))
        do (cond
             ((= dx  1) (setf dx  0 dy  1))
             ((= dx -1) (setf dx  0 dy -1))
             ((= dy  1) (setf dx -1 dy  0))
             ((= dy -1) (setf dx  1 dy  0))
             (T (error "Cannot turn from (~a, ~a)" dx dy)))
        when (and (= dx dir-x) (= dy dir-y)) do (error "Stuck rotating in place.")
        finally (return (values dx dy))))

(defun day6-path ()
  (declare (optimize (speed 3)))
  (multiple-value-bind (start-x start-y map width height) (day6-data)
    (loop with visited = (make-array `(,height ,width)
                                     :element-type 'boolean
                                     :initial-element NIL)
          with positions = (make-queue)
          with dir-x of-type (integer -1 1) = 0
          with dir-y of-type (integer -1 1) = -1
          for x of-type (signed-byte 32) = start-x then (+ x dir-x)
          for y of-type (signed-byte 32) = start-y then (+ y dir-y)
          while (and (<= 0 x) (<= 0 y) (< x width) (< y height))
          do (queue-push (cons x y) positions)
          unless (aref visited y x) count x into steps
          do (setf (aref visited y x) T)
          do (multiple-value-bind (dx dy) (day6-rotate x y dir-x dir-y map width height)
               (setf dir-x dx)
               (setf dir-y dy))
          finally (return (values steps (queue-as-list positions) map width height)))))

(defun d6p1 () (nth-value 0 (day6-path)))

;; 5242

(defun day6-loop-p (dir-x dir-y start-x start-y visited map width height)
  (flet ((test-dir (a b) (and (= (car a) (car b)) (= (cdr a) (cdr b)))))
    (loop with visits = (make-array `(,height ,width) :element-type 'list :initial-element NIL)
          with dir-x = dir-x
          with dir-y = dir-y
          for x = start-x then (+ x dir-x)
          for y = start-y then (+ y dir-y)
          while (and (<= 0 x) (<= 0 y) (< x width) (< y height))
          for dir = (cons dir-x dir-y)
          when (or (find dir (aref visited y x) :test #'test-dir)
                   (find dir (aref visits y x) :test #'test-dir))
          do (return T)
          do (push dir (aref visits y x))
          do (multiple-value-bind (dx dy) (day6-rotate x y dir-x dir-y map width height)
               (setf dir-x dx)
               (setf dir-y dy)))))

(defun d6p2 ()
  (multiple-value-bind (steps positions map width height) (day6-path)
    (declare (ignore steps))
    (let ((checked (make-array `(,height ,width) :element-type 'boolean :initial-element NIL))
          (visits (make-array `(,height ,width) :element-type 'list :initial-element NIL))
          (loop-count 0))
      (setf (aref checked (cdar positions) (caar positions)) T) ;; Guard start position.
      (loop for (x . y) in positions
            for (next-x . next-y) in (rest positions)
            for dir-x = (- next-x x)
            for dir-y = (- next-y y)
            when (aref map next-y next-x) do (error "A wall is already at (~a, ~a)" next-x next-y)
            do (unless (aref checked next-y next-x)
                 (setf (aref map next-y next-x) T) ;; Add the wall.
                 (when (day6-loop-p dir-x dir-y x y visits map width height)
                   (incf loop-count))
                 (setf (aref checked next-y next-x) T) ;; Prevent checking again on a return.
                 (setf (aref map next-y next-x) NIL)) ;; Remove the wall.
            do (push (cons dir-x dir-y) (aref visits y x))
            finally (return loop-count)))))

;; 1424
