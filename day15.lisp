(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/15

(declaim (inline day15-char->tile day15-tile->char
                 day15-char->direction day15-direction->char))
(defun day15-char->tile (ch)
  (declare (type character ch))
  (ecase ch (#\. :floor) (#\# :wall) (#\O :box) (#\@ :robot) (#\[ :box-left) (#\] :box-right)))
(defun day15-tile->char (tile)
  (declare (type keyword tile))
  (ecase tile (:floor #\.) (:wall #\#) (:box #\O) (:robot #\@) (:box-left #\[) (:box-right #\])))
(defun day15-char->direction (ch)
  (declare (type character ch))
  (ecase ch (#\< :west) (#\> :east) (#\^ :north) (#\v :south)))
(defun day15-direction->char (dir)
  (declare (type keyword dir))
  (ecase dir (:west #\<) (:east #\>) (:north #\^) (:south #\v)))

(defun day15-data (&optional double-width-p)
  (declare (optimize (speed 3)))
  (let ((map (make-queue))
        (directions (make-queue))
        (robot-x 0)
        (robot-y 0)
        (width 0)
        (height 0)
        (dir-count 0)
        (map-input-p T))
    (declare (type (unsigned-byte 32) width height dir-count))
    (do-file (line "day15.txt" (values
                                robot-x robot-y
                                (make-array `(,height ,width) :element-type 'keyword
                                                              :initial-contents (queue-as-list map))
                                (make-array dir-count :element-type 'keyword
                                                      :initial-contents (queue-as-list directions))
                                width height dir-count))
      (cond
        ((and map-input-p (zerop (length line)))
         (setf map-input-p NIL)) ;; Swap to direction inputs.
        (map-input-p
         (let ((row-width (if double-width-p (* 2 (length line)) (length line))))
           (when (< width row-width) (setf width row-width)))
         (loop with row = (make-queue)
               for character across line
               for tile = (day15-char->tile character)
               and x of-type (unsigned-byte 32) from 0
               do (ecase tile
                    (:robot
                     (setf robot-x (if double-width-p (* 2 x) x))
                     (setf robot-y height)
                     (queue-push tile row)
                     (when double-width-p
                       (queue-push :floor row)))
                    (:box
                     (cond
                       (double-width-p
                        (queue-push :box-left row)
                        (queue-push :box-right row))
                       (T (queue-push tile row))))
                    ((:wall :floor)
                     (queue-push tile row)
                     (when double-width-p
                       (queue-push tile row))))
               finally (queue-push (queue-as-list row) map))
         (incf height))
        (T
         (loop for character across line
               do (queue-push (day15-char->direction character) directions)
               do (incf dir-count)))))))

(defun day15-move-test (map from-x from-y dx dy width height &optional tested)
  (declare (type (simple-array keyword (* *)) map))
  (declare (type (unsigned-byte 32) from-x from-y width height))
  (declare (type (integer -1 1) dx dy))
  (declare (type (or null (simple-array boolean (* *))) tested))
  (unless (or (zerop dx) (zerop dy)) (error "Diagonal movement."))
  (when (and (zerop dx) (zerop dy)) (error "No movement."))
  (when (or (eql :floor (aref map from-y from-x))
            (eql :wall (aref map from-y from-x)))
    (error "Moving a ~(~a~) from ~a,~a" (aref map from-y from-x) from-x from-y))
  (let ((to-x (+ from-x dx))
        (to-y (+ from-y dy))
        (tested (or tested (make-array `(,height ,width) :element-type 'boolean
                                                         :initial-element NIL))))
    (when (aref tested from-y from-x) (return-from day15-move-test T))
    (setf (aref tested from-y from-x) T)
    (when (and (<= 0 to-x) (<= 0 to-y) (< to-x width) (< to-y height))
      (ecase (aref map to-y to-x)
        (:box
         (day15-move-test map to-x to-y dx dy width height tested))
        (:box-left
         (if (zerop dy)
             (day15-move-test map to-x to-y dx dy width height tested)
             (and (day15-move-test map (1+ to-x) to-y dx dy width height tested)
                  (day15-move-test map to-x to-y dx dy width height tested))))
        (:box-right
         (if (zerop dy)
             (day15-move-test map to-x to-y dx dy width height tested)
             (and (day15-move-test map (1- to-x) to-y dx dy width height tested)
                  (day15-move-test map to-x to-y dx dy width height tested))))
        (:wall NIL)
        (:floor T)
        (:robot (error "Pushing into the robot at ~a,~a???" to-x to-y))))))

(defun day15-move (map from-x from-y dx dy width height &optional moved)
  (declare (type (simple-array keyword (* *)) map))
  (declare (type (unsigned-byte 32) from-x from-y width height))
  (declare (type (integer -1 1) dx dy))
  (declare (type ((or null simple-array) boolean (* *)) moved))
  (let ((to-x (+ from-x dx))
        (to-y (+ from-y dy))
        (moved (or moved (make-array `(,height ,width) :element-type 'boolean
                                                       :initial-element NIL))))
    (when (aref moved from-y from-x) (return-from day15-move T))
    (setf (aref moved from-y from-x) T)
    (ecase (aref map to-y to-x)
      (:box (day15-move map to-x to-y dx dy width height moved))
      (:box-left
       (when (zerop dx)
         (day15-move map (1+ to-x) to-y dx dy width height moved))
       (day15-move map to-x to-y dx dy width height moved))
      (:box-right
       (when (zerop dx)
         (day15-move map (1- to-x) to-y dx dy width height moved))
       (day15-move map to-x to-y dx dy width height moved))
      (:floor)
      (:wall (error "Pushing into a wall at ~a,~a" to-x to-y))
      (:robot (error "Pushing into the robot at ~a,~a???" to-x to-y)))
    (setf (aref map to-y to-x) (aref map from-y from-x))
    (setf (aref map from-y from-x) :floor)))

(defun day15-sum-boxes (map width height)
  (let ((sum 0))
    (dotimes (y height sum)
      (dotimes (x width)
        (when (or (eql :box (aref map y x)) (eql :box-left (aref map y x)))
          (incf sum (+ x (* y 100))))))))

(defun day15-print-map (map width height)
  (dotimes (y height)
    (format T "~&")
    (dotimes (x width)
      (format T "~c" (day15-tile->char (aref map y x))))
    (format T "~%")))

(defun day15-simulate (start-x start-y map directions width height dir-count
                       &key (steps 0) print-dirs-p)
  (declare (type (unsigned-byte 32) start-x start-y width height dir-count steps))
  (declare (type boolean print-dirs-p))
  (declare (type (simple-array keyword (* *)) map))
  (declare (type (simple-array keyword (*)) directions))
  (loop with x = start-x
        and y = start-y
        for i from 0 below dir-count
        for dir = (aref directions i)
        for (dx . dy) = (ecase dir
                          (:west  '(-1 .  0))
                          (:east  '( 1 .  0))
                          (:north '( 0 . -1))
                          (:south '( 0 .  1)))
        counting dir into step
        while (or (zerop steps) (<= step steps))
        when print-dirs-p do (format T "~&~a. ~a ~3d,~3d -> ~3d,~3d~%"
                                     step
                                     (ecase dir
                                       (:west  "West ")
                                       (:east  "East ")
                                       (:north "North")
                                       (:south "South"))
                                     x y (+ x dx) (+ y dy))
        do (when (day15-move-test map x y dx dy width height)
             (day15-move map x y dx dy width height)
             (incf x dx)
             (incf y dy))))

(defun d15p1 (&optional (steps 0) print-map-p print-dirs-p)
  (multiple-value-bind (start-x start-y map directions width height dir-count) (day15-data)
    (day15-simulate start-x start-y map directions width height dir-count
                    :steps steps :print-dirs-p print-dirs-p)
    (when print-map-p (day15-print-map map width height))
    (day15-sum-boxes map width height)))

;; 1349898

(defun d15p2 (&optional (steps 0) print-map-p print-dirs-p)
  (multiple-value-bind (start-x start-y map directions width height dir-count) (day15-data T)
    (day15-simulate start-x start-y map directions width height dir-count
                    :steps steps :print-dirs-p print-dirs-p)
    (when print-map-p (day15-print-map map width height))
    (day15-sum-boxes map width height)))

;; 1376686
