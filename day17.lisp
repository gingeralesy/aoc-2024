(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/17

(defun day17-data (&optional test-p)
  (declare (optimize (speed 3)))
  (let ((num-parser (cl-ppcre:create-scanner "\\d+"))
        (reg-parser (cl-ppcre:create-scanner "^Register (A|B|C): (\\d+)$"))
        (prog-parser (cl-ppcre:create-scanner "^Program: \\d(,\\d)+$"))
        (registers (make-queue))
        (program (make-queue))
        (prog-count 0))
    (declare (type (unsigned-byte 32) prog-count))
    (with-local-file (stream (if test-p "test.txt" "day17.txt"))
      (dotimes (i 3)
        (let ((line (read-clean-line stream)))
          (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings reg-parser line)
            (declare (type (simple-vector 2) groups))
            (unless match (error "Invalid register line: ~a" line))
            (queue-push (parse-integer (svref groups 1)) registers))))
      (let ((line (read-clean-line stream)))
        (unless (or (eql :eof line) (zerop (length (the (simple-array character (*)) line))))
          (error "Invalid empty line: ~a" line)))
      (let ((line (read-clean-line stream)))
        (unless (cl-ppcre:scan prog-parser line)
          (error "Invalid program line: ~a" line))
        (dolist (num (cl-ppcre:all-matches-as-strings num-parser line))
          (queue-push (parse-integer num) program)
          (incf prog-count)))
      (let ((line (read-clean-line stream)))
        (unless (eql line :eof)
          (error "Invalid end line: ~a" line))))
    (values
     (make-array 3 :element-type '(unsigned-byte 64)
                   :initial-contents (queue-as-list registers))
     (make-array prog-count :element-type '(unsigned-byte 3)
                            :initial-contents (queue-as-list program))
     prog-count)))

(declaim (inline day17-operation day17-combo))
(defun day17-operation (opcode)
  (ecase (the (unsigned-byte 3) opcode)
    (0 :adv)
    (1 :bxl)
    (2 :bst)
    (3 :jnz)
    (4 :bxc)
    (5 :out)
    (6 :bdv)
    (7 :cdv)))
(defun day17-combo (value registers)
  (declare (type (unsigned-byte 3) value))
  (declare (type (simple-array (unsigned-byte 64) (*)) registers))
  (ecase value
    ((0 1 2 3) value)
    (4 (aref registers 0))
    (5 (aref registers 1))
    (6 (aref registers 2))
    (7 (error "Reserved combo operand value: 7"))))

(defun day17-run (registers program count &optional (max-length 0))
  (declare (type (simple-array (unsigned-byte 64) (3)) registers))
  (declare (type (simple-array (unsigned-byte 3) (*)) program))
  (declare (type (unsigned-byte 8) count max-length))
  (declare (optimize (speed 3)))
  ;; (when debug-p
  ;;   (format T "~&    INIT      | A: ~9d | B: ~9d | C: ~9d |~%"
  ;;           (aref registers 0) (aref registers 1) (aref registers 2)))
  (loop with pointer = 0
        and output = (make-queue)
        and output-count of-type (unsigned-byte 32) = 0
        while (< pointer count)
        for opcode = (aref program pointer)
        and operand = (if (< pointer (1- count))
                          (aref program (1+ pointer))
                          (error "Operand out of bounds for instruction pointer: ~a" pointer))
        and prev-pointer = pointer
        do (ecase (day17-operation opcode)
             (:adv
              (unless (= (u32 (day17-combo operand registers)) (day17-combo operand registers))
                (error "Register A is too high: ~a" registers))
              (let ((value (u64 (expt 2 (u32 (day17-combo operand registers))))))
                (setf (aref registers 0) (floor (aref registers 0) value)))
              (incf pointer 2))
             (:bdv
              (unless (= (u32 (day17-combo operand registers)) (day17-combo operand registers))
                (error "Register A is too high: ~a" registers))
              (let ((value (u64 (expt 2 (u32 (day17-combo operand registers))))))
                (setf (aref registers 1) (floor (aref registers 0) value)))
              (incf pointer 2))
             (:cdv
              (unless (= (u32 (day17-combo operand registers)) (day17-combo operand registers))
                (error "Register A is too high: ~a" registers))
              (let ((value (u64 (expt 2 (u32 (day17-combo operand registers))))))
                (setf (aref registers 2) (floor (aref registers 0) value)))
              (incf pointer 2))
             (:bxl
              (setf (aref registers 1) (logxor (aref registers 1) operand))
              (incf pointer 2))
             (:bst
              (setf (aref registers 1) (mod (day17-combo operand registers) 8))
              (incf pointer 2))
             (:jnz
              (if (or (zerop (aref registers 0)) (= pointer operand))
                  (incf pointer 2)
                  (setf pointer operand)))
             (:bxc
              (setf (aref registers 1) (logxor (aref registers 2) (aref registers 1)))
              (incf pointer 2))
             (:out
              (let ((value (mod (day17-combo operand registers) 8)))
                (unless (or (zerop max-length) (= value (aref program output-count)))
                  (return-from day17-run)) ;; Quit out early.
                (incf output-count)
                (queue-push value output))
              (incf pointer 2)))
           ;; when debug-p
           ;; do (format T "~&~2d. ~a(~d), ~d | A: ~9d | B: ~9d | C: ~9d | Output: ~{~d~^,~}"
           ;;            prev-pointer (day17-operation opcode) opcode operand
           ;;            (aref registers 0) (aref registers 1) (aref registers 2)
           ;;            (queue-as-list output))
           ;; when (and debug-p (string= "q" (string-downcase (read-line)))) do (return)
        when (and (< 0 max-length) (< max-length (queue-length output)))
        do (return (queue-as-list output))
        finally (return
                  (when (or (zerop max-length) (= max-length (queue-length output)))
                    (prog1 (queue-as-list output)
                      (format T "~&~{~d~^,~}~%" (queue-as-list output)))))))

(defun d17p1 ()
  (multiple-value-bind (registers program count) (day17-data)
    (day17-run registers program count)))

;; 6,0,6,3,0,2,3,1,6

(defun d17p2 ()
  ;; FIXME: This is silly. Figure out the pattern or work it backwards.
  (multiple-value-bind (registers program count) (day17-data)
    (loop with out-length = (length program)
          for i from #x100000000 upto #xffffffffffffffff
          do (setf (aref registers 0) i)
          do (let ((result (day17-run registers program count out-length)))
               (when result (return (values i result)))))))
