(in-package #:aoc-2024)

;; Copied mostly from aoc-2023 util.lisp

(deftype input-line ()
  "Line of text read from a file."
  '(or (and keyword (eql :eof)) (simple-array character (*))))

(defparameter *clean-re*
  (cl-ppcre:create-scanner "^(.*\\S?)\\s*$")
  "Regular expression to clear trailing whitespace.")

(defun local-file (filename &key error)
  "Tests that the file with the name exists on the filesystem."
  (declare (optimize (speed 3)))
  (let ((file (asdf:system-relative-pathname :aoc-2024 filename)))
    (when (and error (not (probe-file file)))
      (error "Missing file: ~a" filename))
    file))

(defmacro with-local-file ((stream-var filename) &body body)
  "Opens a local file from the file system and closes it once done."
  (let ((filename-var (gensym "FILENAME"))
        (file-var (gensym "FILE")))
    `(let* ((,filename-var ,filename)
            (,file-var (local-file ,filename-var :error T)))
       (with-open-file (,stream-var ,file-var :direction :input
                                              :element-type 'character
                                              :if-does-not-exist :error
                                              :external-format :utf-8)
         ,@body))))

(defun clean-line (line)
  "Cleans the string from trailing whitespace."
  (declare (optimize (speed 3)))
  (if (stringp line)
      (multiple-value-bind (match groups)
          (cl-ppcre:scan-to-strings *clean-re* line)
        (declare (type (or null (simple-array T (*))) groups))
        (or (and match groups (aref groups 0)) ""))
      line))

(declaim (inline read-clean-line))
(defun read-clean-line (stream)
  "Reads a line of text from the stream and cleans out the trailing whitespace."
  (declare (optimize (speed 3)))
  (let ((line (read-line stream NIL :eof)))
    (if (eql line :eof) line (clean-line line))))

(defmacro do-file ((line-var filename &optional return-value) &body body)
  "Performs the operation for each line of text in the file."
  (let ((stream-var (gensym "STREAM")))
    `(with-local-file (,stream-var ,filename)
       (loop for ,line-var of-type input-line = (read-clean-line ,stream-var)
             unless ,line-var do (error "Unexpected error reading file: ~a" ,filename)
             until (eql ,line-var :eof)
             do (progn ,@body)
             finally (return ,return-value)))))
