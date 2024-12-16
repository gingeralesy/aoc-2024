(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/16

(defstruct (day16-node (:constructor day16-node (facing x y)))
  (x 0 :type (unsigned-byte 32))
  (y 0 :type (unsigned-byte 32))
  (facing :east :type keyword)
  (g-score #xffffffff :type (unsigned-byte 32))
  (f-score #xffffffff :type (unsigned-byte 32)))

(defun day16-data ()
  (declare (optimize (speed 3)))
  (let ((start-x 0)
        (start-y 0)
        (end-x 0)
        (end-y 0)
        (map (make-queue))
        (width 0)
        (height 0))
    (declare (type (unsigned-byte 32) start-x start-y end-x end-y width height))
    (do-file (line "day16.txt" (values
                                start-x start-y end-x end-y
                                (make-array `(,height ,width) :element-type 'boolean
                                                              :initial-contents (queue-as-list map))
                                width height))
      (when (< width (length line)) (setf width (length line)))
      (loop for x from 0 below width
            collect (ecase (char line x)
                      (#\# T)
                      (#\. NIL)
                      (#\S
                       (setf start-x x)
                       (setf start-y height)
                       NIL)
                      (#\E
                       (setf end-x x)
                       (setf end-y height)
                       NIL))
            into row
            finally (queue-push row map))
      (incf height))))

(defun day16-solve (&optional all-paths-p) ;; A proper use of A* finally!
  ;; Note: Because of part 2 I started trying out different nonsense. This became rather terrible.
  ;;       Please ignore.
  (multiple-value-bind (start-x start-y end-x end-y map) (day16-data)
    (let ((nodes (make-hash-table :test 'equal))
          (path (make-hash-table :test 'equal))
          (final-path (when all-paths-p (make-hash-table :test 'equal)))
          (queued (make-hash-table :test 'equal)))
      (labels ((heuristic (from to)
                 (let ((from-x (day16-node-x from))
                       (from-y (day16-node-y from))
                       (to-x (day16-node-x to))
                       (to-y (day16-node-y to))
                       (facing (day16-node-facing from)))
                   (if (and (= from-x to-x) (= from-y to-y))
                       0
                       (+ (abs (- to-x from-x)) (abs (- to-y from-y))
                          (case facing
                            (:north (if (< from-y to-y) 2000 (if (= from-x to-x) 1000 0)))
                            (:south (if (< to-y from-y) 2000 (if (= from-x to-x) 1000 0)))
                            (:west  (if (< from-x to-x) 2000 (if (= from-y to-y) 1000 0)))
                            (:east  (if (< to-x from-x) 2000 (if (= from-y to-y) 1000 0))))))))
               (node-key (node)
                 (list (day16-node-facing node) (day16-node-x node) (day16-node-y node)))
               (node (facing x y)
                 (let* ((key (list facing x y))
                        (node (gethash key nodes)))
                   (unless node
                     (setf node (day16-node facing x y))
                     (setf (gethash key nodes) node))
                   node))
               (next-nodes (node)
                 (let* ((facing (day16-node-facing node))
                        (x (day16-node-x node))
                        (y (day16-node-y node))
                        (next-x (ecase facing
                                  (:east (1+ x))
                                  (:west (1- x))
                                  ((:north :south) x)))
                        (next-y (ecase facing
                                  (:south (1+ y))
                                  (:north (1- y))
                                  ((:east :west) y)))
                        (left (node (ecase facing
                                      (:east :north)
                                      (:west :south)
                                      (:south :east)
                                      (:north :west))
                                    x y))
                        (right (node (ecase facing
                                       (:east :south)
                                       (:west :north)
                                       (:south :west)
                                       (:north :east))
                                     x y)))
                   (loop for opt in '(:left :right :next)
                         unless (and (eql opt :next) (aref map next-y next-x))
                         collect (ecase opt
                                   (:left left)
                                   (:right right)
                                   (:next (node facing next-x next-y))))))
               (push-node (node queue)
                 (let ((key (node-key node)))
                   (cond
                     ((gethash key queued)
                      (unless (heap-update queue node)
                        (error "Failed to update node in heap: ~a" node)))
                     (T
                      (heap-push queue node)
                      (setf (gethash key queued) T)))))
               (pop-node (queue)
                 (let ((node (heap-pop queue)))
                   (setf (gethash (node-key node) queued) NIL)
                   node))
               (path-cost (node)
                 (loop with full-path = (list node)
                       for current = node then prev
                       for prev = (gethash (node-key current) path)
                       when all-paths-p do (setf (gethash (node-key current) final-path) current)
                       while prev
                       do (push prev full-path)
                       sum (if (eql (day16-node-facing current) (day16-node-facing prev)) 1 1000)
                       into total-sum
                       finally (return (values total-sum full-path)))))
        (let ((queue (make-heap #'< :key #'day16-node-f-score
                                    :element-type '(or null day16-node)
                                    :initial-element NIL
                                    :test #'(lambda (a b) (equal (node-key a) (node-key b)))))
              (start (node :east start-x start-y))
              (end (node :east end-x end-y)))
          (setf (day16-node-f-score start) 0)
          (setf (day16-node-g-score start) (heuristic start end))
          (push-node start queue)
          (loop while (< 0 (heap-size queue))
                with optional-ends = (make-queue)
                for current = (pop-node queue)
                when (and (= end-x (day16-node-x current)) (= end-y (day16-node-y current)))
                do (if all-paths-p
                       (unless (< 0 (hash-table-count final-path))
                         (path-cost current))
                       (return (path-cost current)))
                do (loop for next in (next-nodes current)
                         for next-key = (node-key next)
                         and cost = (if (eql (day16-node-facing current) (day16-node-facing next))
                                        1 1000)
                         for tentative-g = (+ (day16-node-g-score current) cost)
                         if (and all-paths-p (gethash next-key final-path))
                         do (cond
                              ((< tentative-g (day16-node-g-score next))
                               (error "Somehow found a shorter path to ~a" next))
                              ((= tentative-g (day16-node-g-score next))
                               (loop for prev = current then (gethash (node-key prev) path)
                                     until (gethash (node-key prev) final-path)
                                     do (setf (gethash (node-key prev) final-path) prev))))
                         else
                         do (cond
                              ((and (< tentative-g (day16-node-g-score next))
                                    (or (not all-paths-p) (null (gethash next-key final-path))))
                               (setf (gethash next-key path) current)
                               (setf (day16-node-g-score next) tentative-g)
                               (setf (day16-node-f-score next) (+ tentative-g (heuristic next end)))
                               (push-node next queue))
                              ((and (= tentative-g (day16-node-g-score next))
                                    all-paths-p (null (gethash next-key final-path)))
                               (queue-push current optional-ends))))
                finally (when all-paths-p
                          ;; I should clean this up and put the stuff back into the main loop.
                          ;; But I won't. I'm tired and just want to move on.
                          (loop
                            with pending = (make-queue)
                            for current = (queue-pop optional-ends)
                            while current
                            unless (gethash (node-key current) final-path)
                            do (loop
                                 for next in (next-nodes current)
                                 for next-key = (node-key next)
                                 and cost = (if (eql (day16-node-facing current)
                                                     (day16-node-facing next))
                                                1 1000)
                                 for tentative-g = (+ (day16-node-g-score current) cost)
                                 if (gethash next-key final-path)
                                 do (cond
                                      ((< tentative-g (day16-node-g-score next))
                                       (error "Somehow found a shorter path to ~a" next))
                                      ((= tentative-g (day16-node-g-score next))
                                       (loop for prev = current then (gethash (node-key prev) path)
                                             until (gethash (node-key prev) final-path)
                                             do (setf (gethash (node-key prev) final-path) prev))
                                       (loop until (queue-empty-p pending)
                                             do (queue-push (queue-pop pending) optional-ends))))
                                 else
                                 do (cond
                                      ((< tentative-g (day16-node-g-score next))
                                       (setf (gethash next-key path) current)
                                       (setf (day16-node-g-score next) tentative-g)
                                       (setf (day16-node-f-score next)
                                             (+ tentative-g (heuristic next end)))
                                       (queue-push next optional-ends))
                                      ((= tentative-g (day16-node-g-score next))
                                       (queue-push current pending)))))
                          (return final-path))))))))


(defun d16p1 ()
  (nth-value 0 (day16-solve)))

;; 114476

(defun d16p2 (&optional print-route-p)
  (loop with tiles = (make-hash-table :test 'equal)
        and path = (day16-solve T)
        for (facing x y) in (hash-table-keys path)
        for tile = (cons x y)
        for found-p = (gethash tile tiles)
        unless found-p do (setf (gethash tile tiles) T)
        unless found-p count x
        into sum
        finally (return (if print-route-p
                            (let ((map (nth-value 4 (day16-data))))
                              (dotimes (y (array-dimension map 0) sum)
                                (format T "~&")
                                (dotimes (x (array-dimension map 1))
                                  (format T "~c" (cond
                                                   ((aref map y x) #\#)
                                                   ((gethash (cons x y) tiles) #\O)
                                                   (T #\.))))
                                (format T "~%")))
                            sum))))

;; 506
