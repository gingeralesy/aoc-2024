(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/12

(defun day12-data ()
  (let ((map (make-queue))
        (plants (make-hash-table))
        (plant-count 0)
        (width 0)
        (height 0))
    (do-file (line "day12.txt" (values
                                (make-array `(,height ,width)
                                            :element-type '(unsigned-byte 8)
                                            :initial-contents (queue-as-list map))
                                width height))
      (when (< width (length line))
        (setf width (length line)))
      (loop for x below width
            for plant-ch = (char line x)
            for plant = (gethash plant-ch plants plant-count)
            do (when (= plant plant-count)
                 (incf plant-count)
                 (setf (gethash plant-ch plants) plant))
            collect plant into row
            finally (queue-push row map))
      (incf height))))

(defun day12-count-region-perimeter (from-x from-y map width height visited)
  (setf (aref visited from-y from-x) T)
  (loop with plant = (aref map from-y from-x)
        and region = 0
        and perimeter = 0
        and next = (list (cons from-x from-y))
        while next
        for (x . y) = (pop next)
        do (incf region)
        do (loop for (dx . dy) in '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))
                 for next-x = (+ x dx)
                 for next-y = (+ y dy)
                 do (cond
                      ((or (< next-x 0) (<= width next-x)
                           (< next-y 0) (<= height next-y)
                           (/= plant (aref map next-y next-x)))
                       (incf perimeter))
                      ((not (aref visited next-y next-x))
                       (setf (aref visited next-y next-x) T)
                       (push (cons next-x next-y) next))))
        finally (return (values region perimeter))))

(defun d12p1 ()
  (multiple-value-bind (map width height) (day12-data)
    (let ((visited (make-array `(,height ,width) :element-type 'boolean
                                                 :initial-element NIL))
          (total 0))
      (dotimes (y height total)
        (dotimes (x width)
          (unless (aref visited y x)
            (multiple-value-bind (region perimeter)
                (day12-count-region-perimeter x y map width height visited)
              (incf total (* region perimeter)))))))))

;; 1371306

(defun day12-count-region-sides (from-x from-y map width height visited)
  (declare (type (unsigned-byte 32) from-x from-y width height))
  (declare (type (simple-array (unsigned-byte 8) (* *)) map))
  (declare (type (simple-array boolean (* *)) visited))
  ;; (declare (optimize (speed 3)))
  (flet ((plot-matches-p (x y)
           (and (<= 0 x) (< x width)
                (<= 0 y) (< y height)
                (= (aref map y x) (aref map from-y from-x))))
         (queue-plot (x y &optional queue)
           (let ((queue (or queue (make-queue))))
             (unless (aref visited y x)
               (setf (aref visited y x) T)
               (queue-push (cons x y) queue))
             queue)))
    ;; Go through the region matching this plot and mark them as visited.
    (loop with queue = (queue-plot from-x from-y)
          until (queue-empty-p queue)
          for (x . y) of-type ((unsigned-byte 32) . (unsigned-byte 32)) = (queue-pop queue)
          count x into region-size of-type (unsigned-byte 32)
          ;; Queue any neighbouring plots that match the region.
          sum (loop for (dx . dy) of-type ((integer -1 1) . (integer -1 1))
                    in '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))
                    for next-x of-type (signed-byte 32) = (+ x dx)
                    and next-y of-type (signed-byte 32) = (+ y dy)
                    if (plot-matches-p next-x next-y)
                    do (queue-plot next-x next-y queue)
                    else ;; Else there's an edge so check for corners counter-clockwise.
                    when (or (not (plot-matches-p (if (zerop dx) (+ x dy) x) ;; Side
                                                  (if (zerop dy) (- y dx) y)))
                             (plot-matches-p (if (zerop dx) (+ x dy) next-x) ;; Diagonal
                                             (if (zerop dy) (- y dx) next-y)))
                    count dx)
          into side-count of-type (unsigned-byte 32)
          finally (return (values region-size side-count)))))

(defun d12p2 ()
  (multiple-value-bind (map width height) (day12-data)
    (let ((visited (make-array `(,height ,width) :element-type 'boolean
                                                 :initial-element NIL))
          (total 0))
      (dotimes (y height total)
        (dotimes (x width)
          (unless (aref visited y x)
            (multiple-value-bind (region sides)
                (day12-count-region-sides x y map width height visited)
              (incf total (* region sides)))))))))

;; 805880
