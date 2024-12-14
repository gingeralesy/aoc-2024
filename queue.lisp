(in-package #:aoc-2024)

;; Copied from aoc-2023

(declaim (inline make-queue))
(defun make-queue (&rest values)
  "Creates a linked list based queue."
  (let ((queue (cons NIL NIL))) ;; (HEAD . TAIL)
    (dolist (value values queue)
      (queue-push value queue))))

(declaim (inline queue-length))
(defun queue-length (queue)
  "Counts the number of items in the queue. If you need this consider using AQUEUE."
  (declare (optimize (speed 3)))
  (length (the list (car queue))))

(declaim (inline queue-clear))
(defun queue-clear (queue)
  "Clears all items from the queue."
  (setf (car queue) NIL)
  (setf (cdr queue) NIL)
  queue)

(declaim (inline queue-empty-p))
(defun queue-empty-p (queue)
  "Tests if the queue is empty."
  (declare (optimize (speed 3)))
  (null (the list (car queue))))

(defun queue-push (obj queue)
  "Pushes an item into the queue without ordering."
  (declare (type list queue))
  (declare (optimize (speed 3)))
  (if (cdr queue)
      (setf (cddr queue) (cons obj NIL)
            (cdr queue) (cddr queue))
      (setf (cdr queue) (cons obj NIL)
            (car queue) (cdr queue)))
  queue)

(defun pqueue-push (obj queue &optional (key #'identity) (test #'<))
  "Use this to ensure ordering in a priority queue."
  (cond
    ((queue-empty-p queue)
     (queue-push obj queue))
    ((funcall test (funcall key obj) (funcall key (caar queue)))
     (let ((new (cons obj (car queue))))
       (setf (car queue) new)))
    (T
     (loop with obj-key = (funcall key obj)
           for prev = (car queue) then next
           for next = (cdr prev)
           for prev-key = (and prev (funcall key (car prev))) then next-key
           for next-key = (and next (funcall key (car next)))
           until (or (null next) (and (funcall test prev-key obj-key)
                                      (funcall test obj-key next-key)))
           finally (let ((new (cons obj next)))
                     (setf (cdr prev) new)
                     (when (null next)
                       (setf (cdr queue) new))))))
  queue)

(defun queue-pop (queue)
  "Pops an item from the queue."
  (declare (type list queue))
  (declare (optimize (speed 3)))
  (when (car queue)
    (let ((obj (caar queue)))
      (setf (car queue) (cdar queue))
      (unless (car queue)
        (setf (cdr queue) NIL))
      obj)))

(defun pqueue-pop (queue &key (key #'identity) (test #'<))
  "Use this if you need ordering but can't trust the queue to be ordered."
  (when (car queue)
    (loop with min-prev = NIL
          with min = (car queue)
          for cur = (car queue) then next
          for next = (cdr cur) then (cdr next)
          while next
          do (when (funcall test (funcall key (car next)) (funcall key (car min)))
               (setf min-prev cur)
               (setf min next))
          finally (progn
                    (if min-prev
                        (setf (cdr min-prev) (cdr min))
                        (setf (car queue) (cdr min)))
                    (unless (cdr min)
                      (setf (cdr queue) min-prev))
                    (return (car min))))))

(declaim (inline queue-as-list))
(defun queue-as-list (queue &optional copy)
  "Converts the queue into a new list."
  (declare (optimize (speed 3)))
  (if copy (copy-list (the list (car queue))) (the list (car queue))))

(defun queue-copy (queue)
  "Copies the queue into another structure."
  (declare (type list queue))
  (declare (optimize (speed 3)))
  (let ((copy (make-queue)))
    (loop for item in (queue-as-list queue)
          do (queue-push item copy)
          finally (return copy))))

(defun queue-find (item queue &key (key #'identity) (test #'eql))
  "Finds an item in the queue."
  (declare (type list queue))
  (declare (type function key test))
  (declare (optimize (speed 3)))
  (find item (the list (car queue)) :key key :test test))

(defun queue-remove (item queue &key (key #'identity) (test #'eql))
  "Removes a specific item from the queue."
  (declare (type list queue))
  (declare (type function key test))
  (declare (optimize (speed 3)))
  (loop for prev = NIL then slot
        for slot on (the list (car queue))
        for match-p = (funcall test (funcall key (car slot)) item)
        until match-p
        finally (return
                  (when match-p
                    (cond
                      (prev
                       (setf (cdr prev) (cdr slot))
                       (unless (cdr slot) (setf (cdr queue) prev)))
                      (T
                       (setf (car queue) (cdr slot))
                       (unless (cdr slot) (setf (cdr queue) NIL))))
                    (car slot)))))

(defstruct (aqueue (:constructor %make-aqueue ()))
  "Array based queue."
  (count 0 :type (unsigned-byte 32))
  (size #x100 :type (unsigned-byte 32))
  (start 0 :type (unsigned-byte 32))
  (array (make-array #x100) :type (simple-array T (*))))

(defmethod aqueue-expand ((queue aqueue))
  "Expands the array for the queue."
  (declare (optimize (speed 3)))
  (with-slots (count size start array) queue
    (declare (type (unsigned-byte 32) count size start))
    (declare (type (simple-array T (*)) array))
    (unless (< count (1- size))
      (let ((new-array (make-array (* 2 size))))
        (declare (type (simple-array T (*)) new-array))
        (dotimes (i count)
          (setf (aref new-array i) (aref array (mod (+ i start) size))))
        (setf start 0)
        (setf size (* 2 size))
        (setf array new-array)
        queue))))

(defmethod aqueue-push ((queue aqueue) value)
  "Pushes an item into the queue."
  (declare (optimize (speed 3)))
  (aqueue-expand queue)
  (with-slots (count size start array) queue
    (declare (type (unsigned-byte 32) count size start))
    (declare (type (simple-array T (*)) array))
    (setf (aref array (mod (+ start count) size)) value)
    (setf count (1+ count))
    queue))

(defmethod aqueue-pop ((queue aqueue))
  "Pops an item from the queue."
  (declare (optimize (speed 3)))
  (with-slots (count size start array) queue
    (declare (type (unsigned-byte 32) count size start))
    (declare (type (simple-array T (*)) array))
    (when (< 0 count)
      (setf count (1- count))
      (let ((value (aref array start)))
        (setf (aref array start) NIL)
        (setf start (mod (1+ start) size))
        value))))

(defmethod aqueue-clear ((queue aqueue))
  "Clears all items from the queue."
  (declare (optimize (speed 3)))
  (with-slots (count size start array) queue
    (declare (type (unsigned-byte 32) count size start))
    (declare (type (simple-array T (*)) array))
    (when (< 0 count)
      (dotimes (i size)
        (setf (aref array i) NIL)))
    (setf start 0)
    (setf count 0)
    queue))

(defmethod aqueue-empty-p ((queue aqueue))
  "Tests if the queue is empty."
  (declare (optimize (speed 3)))
  (= 0 (aqueue-count queue)))
