;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;; Copyright (c) 2012, Vasily Postnicov
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 

;; 1. Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution. 

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :simplifier)

(defun inverse-elements-elimination-1 (rest-values rest1 rest2)
  (let* ((rest (append rest-values rest1 rest2))
	 (len (length rest)))
    (cond
     ((> len 1)
      (cons '+ rest))
     
     ((= len 1)
      (car rest))
     
     (t 0))))

(defun inverse-elements-elimination-2 (rest-values rest1 rest2)
  (let* ((rest (append rest-values rest1 rest2))
	 (len (length rest)))
    (cond
     ((> len 1)
      (append (list '-  0) rest))
     
     ((= len 1)
      (list '- (car rest)))
     
     (t 0))))

(defun inverse-elements-elimination-3 (rest-values sym rest1 rest2)
  (let ((rest (append rest-values rest1 rest2)))

    (if rest
	(append (list '-  sym) rest)
      sym)))

(defun sum-numbers (rest-values number1 number2 rest1 rest2)
  (let ((rest (append rest-values rest1 rest2))
	(sum (+ number1 number2)))
    (cond
     (rest
      (append (list '+ sum) rest))

     (t sum))))

(defun sub-numbers-1 (rest-values number1 number2 rest1)
  (let ((rest (append rest-values rest1))
	(sub (- number1 number2)))
    (cond
     (rest
      (append (list '- sub) rest))

     (t sub))))

(defun sub-numbers-2 (rest-values symbol number1 number2 rest1 rest2)
  (let ((rest (append rest-values rest1 rest2))
	(sum (+ number1 number2)))

    (append (list '- symbol) (cons sum rest))))

(defun inverse (rest-form number)
  (if rest-form (append (list '- number) rest-form)
    (- number)))

(defparameter *rules*
  `(((+ a1 r2 (- a1)) (nil r2) ,#'inverse-elements-elimination-1)
    ((+ (- a1) r2 a1) (nil r2) ,#'inverse-elements-elimination-1)
    ((+ a1 a2 r2 (- a2)) ((a1) r2) ,#'inverse-elements-elimination-1)
    ((+ a1 a2 a3 r2 (- a3)) ((a1 a2) r2) ,#'inverse-elements-elimination-1)
    ((+ r1 n1 r2 n2) (n1 n2 r1 r2) ,#'sum-numbers)

    ((- a1 r1 a1) (nil r1) ,#'inverse-elements-elimination-2)
    ((- a1 a2 r1 (- a2)) (a1 nil r1) ,#'inverse-elements-elimination-3)
    ((- a1 (- a2) r1 a2) (a1 nil r1) ,#'inverse-elements-elimination-3)
    ((- a1 a2 a3 r1 (- a3)) (a1 (a2) r1) ,#'inverse-elements-elimination-3)
    ((- n1 r1 n2) (n1 n2 r1) ,#'sub-numbers-1)
    ((- s1 r1 n1 r2 n2) (s1 n1 n2 r1 r2) ,#'sub-numbers-2)
    
    ((+ r1 0) (nil r1) ,#'inverse-elements-elimination-1)
    ((- a1 r1 0) (a1 nil r1) ,#'inverse-elements-elimination-3)

    ((- n1) (n1) ,#'inverse))
  
  "List of values (list rule args func).
   If function #'simplify finds a match between 'rule'
   and supplied form, it calls 'func'. Func must be a function
   with rest-form (third returned value of match-rule) as its
   first argument and other arguments as they are specified in
   'args'.")
