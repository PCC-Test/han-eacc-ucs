;;;; eacc-loc-unihan.lisp  -*- mode: lisp; coding: utf-8; folded-file:t -*-
;;;;
;;;; Process LoC EACC Ideographic and Unihan mappings
;;;; Author: temerson (Tom Emerson)
;;;; Date: 2013-01-21

(defpackage #:eacc
  (:use #:cl #:cl-ppcre))

(in-package #:eacc)

(defun trim-whitespace (s)
  "Removes leading and trailing whitespace from the string S, or
nil if S is nil."
  (when (not (null s))
    (string-trim '(#\Space #\Tab) s)))

(defun read-mappings-from-file (filename kv-fun)
  "Returns a hash table containing the mappings from FILENAME. KV-FUN is a
function that returns a cons whose car is the source value and cdr is the
destination value."
  (with-open-file (in filename :direction :input)
    (loop for line = (trim-whitespace (read-line in nil))
         with result = (make-hash-table :test 'equal)
         while line
         do (when (and (> (length line) 0)
                       (char/= (char line 0) #\#))
              (let ((mapping (funcall kv-fun (cl-ppcre:split "\\t" line))))
                (when mapping
                  (if (gethash (car mapping) result)
                      (format *error-output* "Unexpected duplicate: ~A~%"
                              (car mapping))
                      (setf (gethash (car mapping) result) (cdr mapping))))))
         finally (return result))))

(defun read-unihan-eacc-mappings (filename)
  "Read the kEACC mappings from Unihan_OtherMappings.txt"
  (flet ((get-keacc-mapping (fields)
           (when (string= (second fields) "kEACC")
             (cons (third fields)
                   (parse-integer (first fields) :start 2 :radix 16)))))
    (read-mappings-from-file filename #'get-keacc-mapping)))

(defun read-loc-eacc-mappings (filename)
  "Read the EACC/UCS mappings extracted from the LoC EACC table."
  (read-mappings-from-file filename
                           #'(lambda (x)
                               (cons (first x)
                                     (parse-integer (second x) :radix 16)))))

(defun compare-entries (from to)
  "Display any values in FROM that are either absent or different from
those in TO."
  (flet ((compare (k v)
           (let ((to-value (gethash k to)))
             (if to-value
                 (unless (= to-value v)
                   (format t "~A	~5,'0X	~5,'0X~%" k v to-value))
                 (format t "~A		~5,'0X~%" k v)))))
    (maphash #'compare from)))
