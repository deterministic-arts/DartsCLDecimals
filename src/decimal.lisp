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

(defvar *decimal-places* 2
  "Default number of decimal places used by round-decimal.")

(defvar *rounding-mode* :half-up
  "Default rounding mode used by round-decimal.")


(defun round-decimal (value &key (places *decimal-places*) (mode *rounding-mode*))
  "round-decimal VALUE &key PLACES MODE => NUMBER

Rounds the given rational number to at most PLACES decimal places, using the
method specified by MODE, which may be one of the keywords

   :up          away from zero
   :down        towards zero
   :floor       towards negative infinity
   :ceiling     towards positive infinity
   :half-up     towards the nearest integer, 0.5 away from zero
   :half-down   towards the nearest integer, 0.5 towards zero
   :half-even   towards the nearest integer, or to even on 0.5

The special rounding mode :none does not perform any rounding at all, but 
makes sure, that the original value can be represented exactly as a decimal
number with at most PLACES decimal places.

Note, that this function is currently only defined for rational numbers. Use
rational(ize) on floating point values first."
  (if (minusp places) 
	  (error "decimal places must be specified as non-negative integer value")
	  (etypecase value
		(integer value)
		(rational
		 (let* ((factor (expt 10 places))
				(scaled (* 10 factor value)))
		   (macrolet ((truncate-adjusted (term div)
						`(truncate (if (minusp value) (- scaled ,term) (+ scaled ,term)) ,div)))
			 (/ (ecase mode
				  ((:none)
				   (multiple-value-bind (quotient remainder) (truncate scaled 10)
					 (if (zerop remainder) quotient (error "value ~S needs more than ~D places" value places))))
				  ((:down) (truncate scaled 10))
				  ((:floor) (floor scaled 10))
				  ((:ceiling) (ceiling scaled 10))
				  ((:half-even) (round scaled 10))
				  ((:up) (truncate-adjusted 9 10))
				  ((:half-up) (truncate-adjusted 5 10))
				  ((:half-down) (truncate-adjusted 4 10)))
				factor)))))))


(defun parse-decimal (value 
					  &key (start 0) (end (length value)) (radix 10)
  					       (junk-allowed nil) (decimal-point #\.) 
					       (grouping-symbol #\,))
  "parse-decimal VALUE &key START END RADIX JUNK-ALLOWED DECIMAL-POINT GROUPING-SYMBOL => NUMBER, POSITION

Parses the part of string VALUE between indices START (inclusive, 
defaults to 0) and END (exclusive, defaults to the length of the 
given string) as decimal number with base RADIX. 

The value of GROUPING-SYMBOL must be a character. It will be 
ignored when parsing the integer part of the number, assuming 
it is a group separator, e.g., when the number was formatted 
with group-by-thousands. The value of GROUPING-SYMBOL defaults 
to #\\,. Note, that the character is simply ignored. No checking 
is performed as to whether the grouping makes actually sense.

The value of DECIMAL-POINT must be a character, which is 
taken to be the decimal point marker, which separates the 
integer part from the fractional part. It defaults to #\\..

Unless JUNK-ALLOWED is true, the function will signal a 
condition of type  number-format-error, if it cannot parse the 
entire portion of VALUE between START and END as a decimal 
number, or if the number is not well-formed. If JUNK-ALLOWED 
is true, the function may stop parsing early, returning a 
possibly  incomplete result or nil, if it could not parse a 
number at all. In either case, the secondary return value 
POSITION is the index into VALUE of the first character
not parsed by parse-decimal.

Note, that the resulting numeric value of this function 
is always an exact number, i.e., either an integer or a ratio."
  (check-type value string)
  (check-type start (integer 0))
  (check-type end (integer 0))
  (check-type radix (integer 2 36))
  (check-type decimal-point character)
  (check-type grouping-symbol character)
  (assert (<= 0 start end (length value)) (start end))
  (labels ((digit (char)
			 (let* ((code (char-code char))
					(value (cond ((<= #.(char-code #\0) code #.(char-code #\9)) (- code #.(char-code #\0)))
								 ((<= #.(char-code #\A) code #.(char-code #\Z)) (- code #.(- (char-code #\A) 10)))
								 ((<= #.(char-code #\a) code #.(char-code #\z)) (- code #.(- (char-code #\a) 10)))
								 (t nil))))
			   (and value (< value radix) value)))
		   (bad-input (p &optional reason)
			 (error 'number-format-error :value value :start start :end end :position p :reason reason))
		   (maybe-yield (p value)
			 (if junk-allowed
				 (values value p)
				 (bad-input p)))
		   (before-sign (p)
			 (if (>= p end) 
				 (maybe-yield p nil)
				 (case (char value p)
				   ((#\space #\newline #\tab #\return) (before-sign (1+ p)))
				   ((#\+) (after-sign (1+ p) 1))
				   ((#\-) (after-sign (1+ p) -1))
				   (t (after-sign p 1)))))
		   (after-sign (p sign)
			 (if (>= p end)
				 (maybe-yield p nil)
				 (case (char value p)
				   ((#\space #\newline #\tab #\return) (after-sign (1+ p) sign))
				   (t (let ((digit (digit (char value p))))
						(if (null digit)
							(maybe-yield p nil)
							(integer (1+ p) digit sign)))))))
		   (integer (p accu sign)
			 (if (>= p end)
				 (values (* sign accu) p)
				 (let ((char (char value p)))
				   (cond ((char= char grouping-symbol) (integer (1+ p) accu sign))
						 ((char= char decimal-point) (after-point (1+ p) accu sign))
						 (t (let ((value (digit char)))
							  (if (null value)
								  (maybe-yield p (* sign accu))
								  (integer (1+ p) (+ value (* radix accu)) sign))))))))
		   (after-point (p accu sign)
			 (if (>= p end)
				 (maybe-yield (- p 1) (* accu sign))
				 (let ((digit (digit (char value p))))
				   (if digit
					   (fraction (1+ p) (+ digit (* radix accu)) 10 sign)
					   (maybe-yield (- p 1) (* sign accu))))))
		   (fraction (p accu divisor sign)
			 (if (>= p end)
				 (values (* sign (/ accu divisor)) p)
				 (let ((digit (digit (char value p))))
				   (if digit
					   (fraction (1+ p) (+ digit (* radix accu)) (* 10 divisor) sign)
					   (maybe-yield p (* sign (/ accu divisor))))))))
	(before-sign 0)))