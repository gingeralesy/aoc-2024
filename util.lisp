(in-package #:aoc-2024)

;; Copied mostly from aoc-2023 util.lisp

(declaim (inline u32 u64))
(defun u32 (value)
  "Ensures that the integer value is a short unsigned integer."
  (logand #xffffffff value))
(defun u64 (value)
  "Ensures that the integer value is an unsigned integer."
  (logand #xffffffffffffffff value))

(defmacro profile-run (&body body)
  "Runs a profiler for the action to see how long it takes to execute."
  (let ((start-var (gensym "START")))
    `(let ((,start-var (local-time:now)))
       (unwind-protect (progn ,@body)
         (format T "~&Run took ~3$ sec.~%"
                 (local-time:timestamp-difference (local-time:now) ,start-var))))))
