(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/19

(deftype day19-pattern () '(simple-array (or boolean simple-array) (6)))

(declaim (inline day19-char->index))
(defun day19-char->index (ch)
  (if ch
      (ecase ch
        (#\w 1)
        (#\u 2)
        (#\b 3)
        (#\r 4)
        (#\g 5))
      0))

(defun day19-data ()
  (declare (optimize (speed 3)))
  (flet ((make-pattern ()
           (make-array 6 :element-type '(or boolean (integer 1 5)) :initial-element NIL)))
    (let ((parser (cl-ppcre:create-scanner "[wubrg]+"))
          (patterns (make-pattern))
          (towels (make-queue))
          (towel-count 0)
          (patterns-p T))
      (declare (type (unsigned-byte 32) towel-count))
      (do-file (line "day19.txt" (values
                                  patterns
                                  (make-array towel-count :element-type 'list
                                                          :initial-contents (queue-as-list towels))
                                  towel-count))
        (cond
          (patterns-p
           (when (zerop (length line)) (error "Expected a pattern line"))
           (loop for pattern-chars of-type (simple-array character (*))
                 in (cl-ppcre:all-matches-as-strings parser line)
                 for colors = (loop for i from 0 below (length pattern-chars)
                                    collect (day19-char->index (char pattern-chars i)))
                 for pattern of-type day19-pattern
                 = (or (aref patterns (first colors)) (make-pattern))
                 do (setf (aref patterns (first colors)) pattern)
                 if (rest colors)
                 do (loop for color in (rest colors)
                          and parent of-type day19-pattern = pattern then child
                          for child of-type day19-pattern = (or (aref parent color) (make-pattern))
                          do (setf (aref parent color) child)
                          finally (setf (aref child 0) T))
                 else do (setf (aref pattern 0) T))
           (setf patterns-p NIL))
          ((< 0 (length line))
           (loop for i from 0 below (length line)
                 collect (day19-char->index (char line i)) into towel
                 finally (queue-push towel towels))
           (incf towel-count)))))))

(defun day19-matches (towel patterns)
  (declare (type day19-pattern patterns))
  (declare (optimize (speed 3)))
  (loop for current on towel
        for color = (or (car current) 0)
        for next-color = (or (second current) 0)
        for pattern of-type (or null day19-pattern) = (aref patterns color) then next-pattern
        while pattern
        for next-pattern = (aref pattern next-color)
        counting color into i
        when (aref pattern 0) collect i into lengths
        while next-pattern
        finally (return lengths)))

(defun day19-possible-p (towel patterns)
  (declare (type list towel))
  (declare (type day19-pattern patterns))
  (declare (optimize (speed 3)))
  (let ((lengths (day19-matches towel patterns)))
    (declare (type list lengths))
    (or (find (length towel) lengths :test #'(lambda (a b)
                                               (declare (type (unsigned-byte 32) a b))
                                               (= a b)))
        (loop for length in lengths
              when (day19-possible-p (subseq towel length) patterns)
              do (return T)))))

(defun d19p1 ()
  (multiple-value-bind (patterns towels towel-count) (day19-data)
    (loop for i from 0 below towel-count
          for towel = (aref towels i)
          when (day19-possible-p towel patterns) count towel)))

;; 220

(defun day19-count (towel patterns)
  ;; FIXME: This won't work. Even going through the first one with the tree pattern takes forever.
  (declare (type list towel))
  (declare (optimize (speed 3)))
  (loop for length of-type (unsigned-byte 32) in (day19-matches towel patterns)
        sum (if (/= length (length towel))
                (day19-count (subseq towel length) patterns)
                1)
        into total of-type (unsigned-byte 32)
        finally (return total)))

(defun d19p2 ()
  (multiple-value-bind (patterns towels) (day19-data)
    (loop for towel across towels
          sum (day19-count towel patterns))))
