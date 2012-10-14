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

;; This simplifier is based on simple pattern matching mechanism:
;; #'Simplify compares an algebraic form to each rule from *rules*
;; (possibly, many times). If match is found, function associated
;; with this rule is evaluated (see below)

;; Each rule is a list, which contains atom elements or more lists
;; Atom elements can be something like that:
;; a_atom_name (in other words, a symbol, which name begins with A) - matches to an atom in algebraic form
;; l_list_name - matches to a list
;; n_number_name - matches to a number
;; s_symbol_name - matches to a symbol
;; r_rest_name - matches to anything until next rule matches (works in non-greedy fashion)

;; All above is called a 'rule variable' (i.e. a1, l1, a2, nnumber - All of these are 'rule variables')
;; If any atom besides these is specified (i.e. +, *, /) it matches to form if it is 'eql' to form

;; If a 'rule variable' is used two (or more times), in second time (and later) a corresponding element in algebraic
;; form MUST be eql to previous value of variable
;; i.e. a rule '(+ a1 r1 a1) matches to a form '(+ a b c a), but does not match to (+ a b c d)

;; Each element of rule is being checked for match with (car form)
;; If it does not match (values nil nil nil) is returned by match-rule
;; If it match next element is being checkt with (setq form (cdr form)) and so on
;; until there are no more rule elements. Rest of form is returned as third argument of match-rule

;; Examples:
;; (match-rule '(+ a1 a2) '(+ 4 6)) => (values t '((a1 . 4) (a2 . 5)) nil)
;; (match-rule '(+ a1 a2) '(+ 4 6 8)) => (values t '((a1 . 4) (a2 . 5)) '(8))
;; (match-rule '(+ a1 a1) '(+ 5 6)) => (values nil nil nil)
;; (match-rule '(+ a1 a1) '(+ 5 5)) => (values t '((a1 . 5)) nil)
;; (match-rule '(- a1 a1) '(+ 5 5)) => (values nil nil nil)
;; (match-rule '(+ a1 (- a1)) '(+ d (- d))) => (values t '((a1 . d)) nil)
;; (match-rule '(+ a1 r1 a1) '(+ a b c d)) => (values nil nil nil)
;; (match-rule '(+ a1 r1 a1) '(+ a b c a)) => (values t '((a1 . a) (r1 b c)) nil)
;; (match-rule '(+ a1 r1 a1) '(+ a a)) => (values t '((a1 . a) (r1)) nil)
;; (match-rule '(+ a1 r1 a1) '(+ a a s)) => (values t '((a1 . a) (r1)) '(s))

(defun simplify (form &optional (rules *rules*))
  "Tries to match each rule from *rules*
   If match, function assigned to rule is evaluated
   and process is repeated on the result. Process is stopped
   and result of last evaluation is returned when full cycle
   is completed (each rule was tried) and simplification brings
   no more new results"
  (if (atom form) form
    (let ((new-form form))
      (dolist (rule rules)
	
	(multiple-value-bind (matchp symbols rest-form)
	    (match-rule (car rule) form)
	  (if matchp
	      (setq new-form (apply (cdr rule)
				    rest-form
				    (loop for arg in symbols collect (cdr arg)))))))

      (if (not (equalp form new-form))
	  (simplify new-form)
	form))))

(defun match-rule (rule form &optional symbols)
  "Compares rule with arithmetic form
   If match, three values are returned:
   (values matchp symbols rest-form), where
   'matchp' is t, 'symbols' is associative list of rule variables
   and their actual value and 'rest-form' is last elements of form,
   for which there are no more rules left.

   If no match detected (values nil nil nil) are returned"

  ;; For our purposes rule is always a list
  ;; Because of an atom cannot match a list, following check is required
  (if (atom form) (return-from match-rule (values nil nil nil)))

  ;; Each element of rule is called 'sub-rule'
  (loop for i below (length rule)
	for sub-rule in rule do

	(let ((prev-value (cdr (assoc sub-rule symbols)))
	      (sub-form (car form))
	      (data-type (if (atom sub-rule)
			     (elt (symbol-name sub-rule) 0))))
	  
	  (cond
	   ;; If sub-rule is a list, call match-rule recursively
	   ((listp sub-rule)
	    (multiple-value-bind (matchp new-symbols rest-form)
		(match-rule sub-rule sub-form symbols)
	      (if (and (not rest-form) matchp)
		  (setq symbols new-symbols)
		(return-from match-rule (values nil nil nil)))))
	   
	   
	   ;; If we already met such sub-rule variable and it's actual
	   ;; value is prev-value, it must be the same again
	   (prev-value 
	    (if (not (eql prev-value sub-form))
		(return-from match-rule (values nil nil nil))))


	   ;; Collect elements of form in non-greedy manner
	   ;; (Until next rule matches)
	   ((and sub-form
		 (char= #\R data-type))
	    
	    (let ((next-rule (list (nth (1+ i) rule)))
		  rest-end)

	      (dotimes (next-start (length form))
		(multiple-value-bind (matchp new-symbols rest-form)
		    (match-rule next-rule
				(subseq form next-start)
				symbols)
		  (declare (ignore rest-form new-symbols))
		  (if matchp
		      (progn
			(setq rest-end next-start)
			(return)))))

	      (setq symbols
		    (nconc symbols
			   (list
			    (cons sub-rule
				  (if rest-end
				      (subseq form 0 rest-end)
				    form))))

		    form (cond
			  ((eql rest-end 0) (cons nil form))
			  (rest-end (subseq form (1- rest-end)))
			  (t nil)))))

	   ;; Basic types of sub-rules, ie. atom, list, etc
	   ((and sub-form
		 (or (and (char= #\A data-type)
			  (atom sub-form))
		     
		     (and (char= #\L data-type)
			  (listp sub-form))
		     
		     (and (char= #\N data-type)
			  (numberp sub-form))
		     
		     (and (char= #\S data-type)
			  (symbolp sub-form))))
	    
	    (setq symbols
		  (nconc symbols
			 (list (cons sub-rule sub-form)))))


	   ;; If there is no match until this line
	   ;; try exact match with sub-form with 'eql'
	   ((not (eql sub-form sub-rule))
	    (return-from match-rule (values nil nil nil))))
	  
	  (setq form (cdr form))))
  
  (values t symbols form))
