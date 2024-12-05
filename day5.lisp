(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/5

;; TODO: Make a faster to check map out of the order rules rather than having to search them.

(deftype d5-page () '(integer 0 99))

(defun day5-data (&optional sort-p)
  (declare (optimize (speed 3)))
  (let ((page-parser (cl-ppcre:create-scanner "^(\\d+)\\|(\\d+)$"))
        (page-orders (make-queue))
        (page-count 0)
        (update-parser (cl-ppcre:create-scanner "\\b\\d+\\b"))
        (updates (make-queue))
        (update-count 0)
        (page-orders-p T))
    (declare (type (unsigned-byte 32) page-count update-count))
    (do-file (line "day5.txt" (values
                               (make-array page-count
                                           :element-type '(simple-array d5-page (2))
                                           :initial-contents (queue-as-list page-orders))
                               (make-array update-count
                                           :element-type '(simple-array d5-page (*))
                                           :initial-contents (queue-as-list updates))))
      (cond
        ((= 0 (length line)) (setf page-orders-p NIL))
        (page-orders-p
         (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings page-parser line)
           (unless match (error "Invalid page order line: ~a" line))
           (let* ((a (parse-integer (svref groups 0)))
                  (b (parse-integer (svref groups 1)))
                  (page-pair (make-array 2 :element-type 'd5-page
                                           :initial-contents (list a b))))
             (when (= a b) (error "Cannot order the same page with itself."))
             (if sort-p
                 (pqueue-push page-pair page-orders #'identity
                              #'(lambda (a b)
                                  (declare (type (simple-array d5-page (2)) a b))
                                  (or (< (aref a 0) (aref b 0))
                                      (and (= (aref a 0) (aref b 0))
                                           (< (aref a 1) (aref b 1))))))
                 (queue-push page-pair page-orders))
             (incf page-count))))
        (T
         (loop for page in (cl-ppcre:all-matches-as-strings update-parser line)
               count page into size
               collect (parse-integer page) into pages
               finally (queue-push (make-array size :element-type 'd5-page
                                                    :initial-contents pages)
                                   updates))
         (incf update-count))))))

(defun day5-validate (orders update)
  (declare (type (simple-array (simple-array d5-page (2)) (*)) orders))
  (declare (type (simple-array d5-page (*)) update))
  (declare (optimize (speed 3)))
  (loop with valid-p = T
        while valid-p
        for page-a of-type d5-page across update
        count page-a into current
        do (loop while valid-p
                 for order across orders
                 for order-a of-type d5-page = (aref order 0)
                 for order-b of-type d5-page = (aref order 1)
                 until (< page-a order-a)
                 when (= page-a order-a)
                 do (loop while valid-p
                          for page-b of-type d5-page across update
                          count page-b into other
                          until (< current other)
                          when (= page-b order-b)
                          do (setf valid-p NIL)))
        finally (return valid-p)))

(defun d5p1 ()
  (declare (optimize (speed 3)))
  (multiple-value-bind (page-orders updates)
      (day5-data T)
    (declare (type (simple-array (simple-array d5-page (*)) (*)) updates))
    (loop for update of-type (simple-array d5-page (*)) across updates
          when (day5-validate page-orders update)
          sum (aref update (floor (length update) 2))
          into sum of-type (unsigned-byte 32)
          finally (return sum))))

;; 6951

(defun day5-fix (orders update)
  (declare (type (simple-array (simple-array d5-page (2)) (*)) orders))
  (declare (type (simple-array d5-page (*)) update))
  (declare (optimize (speed 3)))
  (loop
    do (loop
         ;; If there's a swap, start from the beginning.
         ;; TODO: Optimise so that it doesn't check parts that are already sorted.
         with swap-p = NIL
         until swap-p
         for index-a of-type (unsigned-byte 32) from 1 below (length update)
         for page-a of-type d5-page = (aref update index-a)
         do (loop
              until swap-p
              for order across orders
              for order-a of-type d5-page = (aref order 0)
              for order-b of-type d5-page = (aref order 1)
              until (< page-a order-a)
              when (= page-a order-a)
              do (loop
                   until swap-p
                   for index-b of-type (unsigned-byte 32) from 0 below index-a
                   for page-b of-type d5-page = (aref update index-b)
                   do (when (= page-b order-b)
                        (setf (aref update index-a) page-b)
                        (setf (aref update index-b) page-a)
                        (setf swap-p T)))))
    until (day5-validate orders update)
    finally (return update)))

(defun d5p2 ()
  (declare (optimize (speed 3)))
  (multiple-value-bind (page-orders updates)
      (day5-data T)
    (declare (type (simple-array (simple-array d5-page (*)) (*)) updates))
    (loop for update of-type (simple-array d5-page (*)) across updates
          unless (day5-validate page-orders update)
          sum (aref (the (simple-array d5-page (*)) (day5-fix page-orders update))
                    (floor (length update) 2))
          into sum of-type (unsigned-byte 32)
          finally (return sum))))

;; 4121
