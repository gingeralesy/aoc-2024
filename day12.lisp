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
  (flet ((match-plot (x y plant)
           (and (<= 0 x) (< x width)
                (<= 0 y) (< y height)
                (= plant (aref map y x)))))
    (loop
      ;; Go through the region matching this plot and mark them as visited.
      with plant = (aref map from-y from-x)
      and queued = (make-array `(,height ,width) :element-type 'boolean
                                                 :initial-element NIL)
      and region = 0
      and sides = 0
      and next = (make-queue (cons from-x from-y))
      until (queue-empty-p next)
      for (x . y) = (queue-pop next)
      do (setf (aref visited y x) T)
      do (incf region)
      do (loop
           ;; Queue any neighbouring plots that match the region.
           for (dx . dy) in '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))
           for next-x = (+ x dx)
           for next-y = (+ y dy)
           for in-region-p = (match-plot next-x next-y plant)
           do (when (and in-region-p
                         (not (aref visited next-y next-x))
                         (not (aref queued next-y next-x)))
                (setf (aref queued next-y next-x) T)
                (queue-push (cons next-x next-y) next))
           unless in-region-p ;; Edge found.
           do (loop
                ;; There's got to be a better way to count the edges..
                ;; TODO: Maybe count corners instead?
                with checked-p = NIL
                for dir-x in (if (zerop dx) '(-1 1) '(0 0))
                for dir-y in (if (zerop dy) '(-1 1) '(0 0))
                do (loop for x1 = (+ x dir-x) then (+ x1 dir-x)
                         for y1 = (+ y dir-y) then (+ y1 dir-y)
                         while (and (not checked-p) ;; Part of the edge?
                                    (match-plot x1 y1 plant)
                                    (not (match-plot (+ x1 dx) (+ y1 dy) plant)))
                         when (aref visited y1 x1) do (setf checked-p T)) ;; Already tested?
                finally (unless checked-p(incf sides))))
      finally (return (values region sides)))))

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
