(in-package #:aoc-2024)

;; https://adventofcode.com/2024/day/5

(deftype d5-page () '(integer 0 99))

(defun day5-data ()
  (declare (optimize (speed 3)))
  (let ((page-parser (cl-ppcre:create-scanner "^(\\d+)\\|(\\d+)$"))
        (page-orders (make-array 100 :element-type 'list :initial-element ()))
        (update-parser (cl-ppcre:create-scanner "\\b\\d+\\b"))
        (updates (make-queue))
        (update-count 0)
        (page-orders-p T))
    (declare (type (unsigned-byte 32) update-count))
    (do-file (line "day5.txt" (values
                               page-orders
                               (make-array update-count
                                           :element-type '(simple-array d5-page (*))
                                           :initial-contents (queue-as-list updates))))
      (cond
        ((= 0 (length line)) (setf page-orders-p NIL))
        (page-orders-p
         (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings page-parser line)
           (unless match (error "Invalid page order line: ~a" line))
           (let* ((a (parse-integer (svref groups 0)))
                  (b (parse-integer (svref groups 1))))
             (declare (type d5-page a b))
             (when (= a b) (error "Cannot order the same page with itself."))
             (setf (aref page-orders a) (sort (push b (aref page-orders a)) #'<)))))
        (T
         (loop for page in (cl-ppcre:all-matches-as-strings update-parser line)
               count page into size
               collect (parse-integer page) into pages
               finally (queue-push (make-array size :element-type 'd5-page
                                                    :initial-contents pages)
                                   updates))
         (incf update-count))))))

(defun day5-validate (orders update)
  (declare (type (simple-array list (100)) orders))
  (declare (type (simple-array d5-page (*)) update))
  (declare (optimize (speed 3)))
  (loop with valid-p = T
        while valid-p
        for index-a of-type (unsigned-byte 32) from 1 below (length update)
        for page-a of-type d5-page = (aref update index-a)
        for before-a of-type list = (aref orders page-a)
        do (loop while valid-p
                 for index-b of-type (unsigned-byte 32) from 0 below index-a
                 for page-b of-type d5-page = (aref update index-b)
                 when (find page-b before-a :test #'(lambda (a b)
                                                      (declare (type d5-page a b))
                                                      (= a b)))
                 do (setf valid-p NIL))
        finally (return valid-p)))

(defun d5p1 ()
  (declare (optimize (speed 3)))
  (multiple-value-bind (page-orders updates) (day5-data)
    (declare (type (simple-array (simple-array d5-page (*)) (*)) updates))
    (loop for update of-type (simple-array d5-page (*)) across updates
          when (day5-validate page-orders update)
          sum (aref update (floor (length update) 2))
          into sum of-type (unsigned-byte 32)
          finally (return sum))))

;; 6951

(defun day5-fix (orders update)
  (declare (type (simple-array list (100)) orders))
  (declare (type (simple-array d5-page (*)) update))
  (declare (optimize (speed 3)))
  (loop
    with done-to of-type (unsigned-byte 32) = 0
    do (loop
         with swap-p = NIL
         until swap-p
         for index-a of-type (unsigned-byte 32) from (1+ done-to) below (length update)
         for page-a of-type d5-page = (aref update index-a)
         for before-a of-type list = (aref orders page-a)
         do (loop
              until swap-p
              for index-b of-type (unsigned-byte 32) from 0 below index-a
              for page-b of-type d5-page = (aref update index-b)
              do (when (find page-b before-a :test #'(lambda (a b)
                                                       (declare (type d5-page a b))
                                                       (= a b)))
                   (setf (aref update index-a) page-b)
                   (setf (aref update index-b) page-a)
                   (setf swap-p T)))
         unless swap-p do (setf done-to index-a))
    until (day5-validate orders update)
    finally (return update)))

(defun d5p2 ()
  (declare (optimize (speed 3)))
  (multiple-value-bind (page-orders updates) (day5-data)
    (declare (type (simple-array (simple-array d5-page (*)) (*)) updates))
    (loop for update of-type (simple-array d5-page (*)) across updates
          unless (day5-validate page-orders update)
          sum (aref (the (simple-array d5-page (*)) (day5-fix page-orders update))
                    (floor (length update) 2))
          into sum of-type (unsigned-byte 32)
          finally (return sum))))

;; 4121
