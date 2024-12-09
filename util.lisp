(in-package #:aoc-2024)

;; Copied mostly from aoc-2023 util.lisp

(declaim (inline u32 u64 u62))
(defun u32 (value)
  "Ensures that the integer value is a short unsigned integer."
  (logand #xffffffff value))
(defun u64 (value)
  "Ensures that the integer value is an unsigned integer."
  (logand #xffffffffffffffff value))
(defun u62 (value)
  "Ensures that the integer value is an unsigned 62-bit integer."
  (logand #x3fffffffffffffff value))

(declaim (inline char->digit))
(defun char->digit (ch)
  (declare (type (and character) ch))
  (declare (optimize (speed 3)))
  (ecase ch (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7) (#\8 8) (#\9 9)))

(defmacro profile-run (&body body)
  "Runs a profiler for the action to see how long it takes to execute."
  (let ((start-var (gensym "START")))
    `(let ((,start-var (local-time:now)))
       (unwind-protect (progn ,@body)
         (format T "~&Run took ~3$ sec.~%"
                 (local-time:timestamp-difference (local-time:now) ,start-var))))))
