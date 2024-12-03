(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/3

(defun d3p1 ()
  (declare (optimize (speed 3)))
  (let ((parser (cl-ppcre:create-scanner "mul\\((\\d+),(\\d+)\\)"))
        (sum 0))
    (declare (type (unsigned-byte 32) sum))
    (do-file (line "day3.txt" sum)
      (loop for line-match in (cl-ppcre:all-matches-as-strings parser line)
            for (match groups) = (multiple-value-list (cl-ppcre:scan-to-strings parser line-match))
            when match
            do (let ((a (parse-integer (svref groups 0)))
                     (b (parse-integer (svref groups 1))))
                 (declare (type (unsigned-byte 32) a b))
                 (incf sum (* a b)))))))

;; 167650499

(defun d3p2 ()
  (declare (optimize (speed 3)))
  (let ((parser (cl-ppcre:create-scanner "mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\)"))
        (enabled-p T)
        (sum 0))
    (declare (type (unsigned-byte 32) sum))
    (do-file (line "day3.txt" sum)
      (loop for line-match in (cl-ppcre:all-matches-as-strings parser line)
            for (match groups) = (multiple-value-list (cl-ppcre:scan-to-strings parser line-match))
            when match
            do (cond
                 ((string= (the (simple-array character (*)) match) "do()")
                  (setf enabled-p T))
                 ((string= (the (simple-array character (*)) match) "don't()")
                  (setf enabled-p NIL))
                 (enabled-p
                  (let ((a (parse-integer (svref groups 0)))
                        (b (parse-integer (svref groups 1))))
                    (declare (type (unsigned-byte 32) a b))
                    (incf sum (* a b)))))))))
