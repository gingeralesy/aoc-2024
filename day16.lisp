(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/16

(defstruct (day16-node (:constructor day16-node (facing x y)))
  (x 0 :type (unsigned-byte 32))
  (y 0 :type (unsigned-byte 32))
  (facing :east :type keyword))

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

(defun day16-solve () ;; A proper use of A* finally!
  (multiple-value-bind (start-x start-y end-x end-y map) (day16-data)
    (let ((nodes (make-hash-table :test 'equal))
          (path (make-hash-table :test 'equalp))
          (queued (make-hash-table :test 'equalp))
          (g-score (make-hash-table :test 'equalp))
          (f-score (make-hash-table :test 'equalp)))
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
                 (unless (gethash node queued)
                   (heap-push queue node)
                   (setf (gethash node queued) T)))
               (pop-node (queue)
                 (let ((node (heap-pop queue)))
                   (setf (gethash node queued) NIL)
                   node))
               (f-score (node) (gethash node f-score #xffffffff))
               (g-score (node) (gethash node g-score #xffffffff))
               (path-cost (node)
                 (loop with full-path = (list node)
                       for current = node then prev
                       for prev = (gethash current path)
                       while prev
                       do (push prev full-path)
                       sum (if (eql (day16-node-facing current)
                                    (day16-node-facing prev))
                               1 1000)
                       into total-sum
                       finally (return (values total-sum full-path)))))
        (let ((queue (make-heap #'< :key #'(lambda (node) (f-score node))
                                    :element-type '(or null day16-node)
                                    :initial-element NIL))
              (start (node :east start-x start-y))
              (end (node :east end-x end-y)))
          (setf (gethash start f-score) 0)
          (setf (gethash start g-score) (heuristic start end))
          (push-node start queue)
          (loop while (< 0 (heap-size queue))
                for current = (pop-node queue)
                when (and (= end-x (day16-node-x current))
                          (= end-y (day16-node-y current)))
                do (return (path-cost current))
                do (loop for next in (next-nodes current)
                         for cost = (if (eql (day16-node-facing current) (day16-node-facing next))
                                        1 1000)
                         for tentative-g = (+ (g-score current) cost)
                         do (when (< tentative-g (g-score next))
                              (setf (gethash next path) current)
                              (setf (gethash next g-score) tentative-g)
                              (setf (gethash next f-score) (+ tentative-g (heuristic next end)))
                              (push-node next queue)))))))))

(defun d16p1 ()
  (nth-value 0 (day16-solve)))

;; 114476

(defun d16p2 (&optional print-route-p)
  ;; TODO: Solve the path, exhaust the queue, only test if score would be cheaper or equal to the
  ;;       path cost, and don't test nodes already in the queue.
  (loop with tiles = (make-hash-table :test 'equal)
        and path = (nth-value 1 (day16-solve))
        for node in path
        for tile = (cons (day16-node-x node) (day16-node-y node))
        for found-p = (gethash tile tiles)
        unless found-p do (setf (gethash tile tiles) T)
        unless found-p count node
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
