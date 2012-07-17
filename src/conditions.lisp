#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Decimal Numbers Library
  Copyright (c) 2011 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package "DARTS.LIB.DECIMAL")


(define-condition parse-error (error)
  ((value 
	 :initarg :value :initform nil :reader parse-error-value
	 :documentation "The string to parse")
   (start 
	 :initarg :start :initform nil :reader parse-error-start
	 :documentation "Start index into the string of the region to parse")
   (end 
	 :initarg :end :initform nil :reader parse-error-end
	 :documentation "End index into the string of the region to parse")
   (position  
	 :initarg :position :initform nil :reader parse-error-position
	 :documentation "Position, where parsing had to stop")
   (reason
	 :initarg :reason :initform nil :reader parse-error-reason
	 :documentation "A keyword describing the type of the failure"))
  (:report (lambda (condition stream)
			 (format stream "failed to parse the portion between ~S and ~S of ~S; parsing stopped at index ~S~@[ with error code ~A~]"
					 (parse-error-start condition) 
					 (parse-error-end condition) 
					 (parse-error-value condition) 
					 (parse-error-position condition)
					 (parse-error-reason condition))))
  (:documentation "Condition to be signalled by parser functions, if they
cannot parse a given region of a string. This class is intended as base
class for more specific condition types."))


(define-condition number-format-error (parse-error) ()
  (:documentation "Condition to be signalled by parse-decimal and related
functions, if they fail to parse a given input string. The condition instance
provides information the context of the error, i.e., the string being parsed
as well as the position where parsing had to stop."))
