#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Decimal Numbers Library
  Copyright (c) 2013, 2015, 2020 Dirk Esser

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

(in-package #:darts.lib.decimal)

(defvar *decimal-places* 2
  "Default number of decimal places used by round-decimal.")

(defvar *rounding-mode* :half-up
  "Default rounding mode used by round-decimal.")

(defvar *default-grouping-symbol* #\,
  "Default grouping symbol to use.")

(defvar *default-decimal-point* #\.
  "Character to use as decimal point symbol by default")

(defvar *default-primary-group-size* 3
  "Default number of characters in the \"primary\" group of an
   integer. The terminology follows the ICU library here.")

(defvar *default-secondary-group-size* 3
  "Default number of characters in the \"primary\" group of an
   integer. The terminology follows the ICU library here.")

(defvar *default-plus-symbol* #\+)
(defvar *default-minus-symbol* #\-)


(deftype rounding-mode ()
  '(member :none :down :floor :ceiling :half-even :up :half-down :half-up))

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
					 (if (zerop remainder) quotient 
                         (error 'simple-type-error 
                                :datum value :expected-type `(decimal ,places)
                                :format-control "value ~S needs more than ~D places and rounding is prohibited" 
                                :format-arguments (list value places)))))
				  ((:down) (truncate scaled 10))
				  ((:floor) (floor scaled 10))
				  ((:ceiling) (ceiling scaled 10))
				  ((:half-even) (round scaled 10))
				  ((:up) (truncate-adjusted 9 10))
				  ((:half-up) (truncate-adjusted 5 10))
				  ((:half-down) (truncate-adjusted 4 10)))
				factor)))))))


(defun parse-decimal (value 
		      &key (start 0) end (radix 10)
                        (junk-allowed nil) (decimal-point *default-decimal-point*)
                        (grouping-symbol *default-grouping-symbol*))
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
  (let* ((value (string value))
         (end (or end (length value))))

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
      (before-sign 0))))


(defun decimalp (value &key (min '*) (max '*) (scale '*))
  "decimalp VALUE &key MIN MAX SCALE => RESULT

   Tests, whether VALUE is a decimal number (as defined by this module),
   which requires a numeric precision of at most SCALE digits after the
   decimal point, and falls in the range defined by MIN and MAX."
  (and (rationalp value)
       (etypecase min
         ((eql *) t)
         ((cons real null) (> value (car min)))
         (real (>= value min)))
       (etypecase max
         ((eql *) t)
         ((cons real null) (< value (car max)))
         (real (<= value max)))
       (or (eql scale '*) 
           (integerp value)
           (integerp (* value (expt 10 scale))))))


(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; These must be available at compile-time for the benefit of the
  ;; deftype for `decimal'

  (defun %scale-0-p (value) (integerp value))
  (defun %scale-1-p (value) (integerp (* value 10)))
  (defun %scale-2-p (value) (integerp (* value 100)))
  (defun %scale-3-p (value) (integerp (* value 1000)))
  (defun %scale-4-p (value) (integerp (* value 10000)))
  (defun %scale-5-p (value) (integerp (* value 100000)))
  (defun %scale-6-p (value) (integerp (* value 1000000)))
  (defun %scale-7-p (value) (integerp (* value 10000000))))


(deftype decimal (&optional (scale '*) (min '*) (max '*))
  (if (eql scale '*)
      `(rational ,min ,max)
      `(and (rational ,min ,max)
            ,(ecase scale
               ((0) 'integer)
               ((1) '(satisfies %scale-1-p))
               ((2) '(satisfies %scale-2-p))
               ((3) '(satisfies %scale-3-p))
               ((4) '(satisfies %scale-4-p))
               ((5) '(satisfies %scale-5-p))
               ((6) '(satisfies %scale-6-p))
               ((7) '(satisfies %scale-7-p))))))


(defun decimal (value &key (scale '*) (min '*) (max '*) (rounding-mode *rounding-mode*))
  "decimal VALUE &key SCALE MIN MAX ROUNDING-MODE => RATIONAL

   Tries to coerce VALUE into a decimal with properties as specified
   via SCALE, MIN, and MAX. Note, that the range checks against MIN
   and MAX are performed against the rounded value, not VALUE."
  (let ((rounded (if (eq scale '*) value (round-decimal value :places scale :mode rounding-mode))))
    (when (etypecase min
            ((eql *) nil)
            (real (< rounded min))
            ((cons real null) (<= rounded (first min))))
      (error 'simple-type-error :datum value :expected-type `(decimal ,scale ,min ,max)
             :format-control "~S is too small, acceptable values are ~A ~S"
             :format-arguments (list value (if (consp min) ">" ">=") (if (consp min) (car min) min))))
    (when (etypecase max
            ((eql *) nil)
            (real (> rounded max))
            ((cons real null) (> rounded (first max))))
      (error 'simple-type-error :datum value :expected-type `(decimal ,scale ,min ,max)
             :format-control "~S is too large, acceptable values are ~A ~S"
             :format-arguments (list value (if (consp max) "<" "<=") (if (consp max) (car max) max)))) 
    rounded))




(defun group-digits (string grouping-symbol primary-group-size secondary-group-size)
  (let ((length (length string)))
    (if (<= length primary-group-size)
        string
        (let ((secondary-length (- length primary-group-size)))
          (multiple-value-bind (num-groups rest) (floor secondary-length secondary-group-size)
            (let* ((size (+ primary-group-size 
                            num-groups (* num-groups secondary-group-size)
                            (if (zerop rest) 0 1) rest))
                   (buffer (make-string size :initial-element #\0)))
              (labels 
                  ((copy-group (rp wp len)
                     (loop
                       :for k :downfrom len :above 0
                       :until (zerop rp) 
                       :do (setf (char buffer (decf wp)) (char string (decf rp)))
                       :finally (return (values rp wp)))))
                (multiple-value-bind (rp wp) (copy-group (length string) size primary-group-size)
                  (loop
                    :while (plusp rp)
                    :do (setf (char buffer (decf wp)) grouping-symbol)
                        (multiple-value-bind (rp* wp*) (copy-group rp wp secondary-group-size)
                          (setf rp rp*)
                          (setf wp wp*))))
                buffer)))))))




(defun format-natural (value
                       &key (grouping-symbol #\,) (group-size 3) 
                            (minimum-width 0) (primary-group-size group-size)
                            (secondary-group-size group-size))
  (check-type value (integer 0))
  (group-digits (format nil "~v,'0D" minimum-width value)
                grouping-symbol primary-group-size secondary-group-size))


(defun format-integer (value 
                       &key ((:stream stream) nil)
                            (grouping-symbol *default-grouping-symbol*)
                            (plus-symbol *default-plus-symbol*)
                            (minus-symbol *default-minus-symbol*)
                            (minimum-width 0) 
                            (primary-group-size *default-primary-group-size*)
                            (secondary-group-size *default-secondary-group-size*)
                            (force-sign nil))
  (check-type value integer)
  (let* ((destination (if stream stream (make-array 8 :element-type 'character :fill-pointer 0 :adjustable t :initial-element #\space)))
         (minus (minusp value))
         (sign (if minus minus-symbol plus-symbol))
         (want-sign (or minus force-sign))
         (string (format-natural (if minus (- value) value) 
                                 :primary-group-size primary-group-size :secondary-group-size secondary-group-size
                                 :minimum-width minimum-width :grouping-symbol grouping-symbol)))
    (format destination "~@[~A~]~A" (and want-sign sign) string)
    (unless stream
      (coerce destination 'simple-string))))


(defun format-decimal (value
                       &key ((:stream stream) nil)
                            (min-fraction-digits 2) (max-fraction-digits 2)
                            (min-integer-digits 1) 
                            (primary-group-size *default-primary-group-size*)
                            (secondary-group-size *default-secondary-group-size*)
                            (decimal-point *default-decimal-point*)
                            (grouping-symbol *default-grouping-symbol*)
                            (plus-symbol *default-plus-symbol*)
                            (minus-symbol *default-minus-symbol*)
                            (force-sign nil) (rounding-mode *rounding-mode*))
  (when (< max-fraction-digits min-fraction-digits) (error "~S must be equal to or greater than ~S" :max-fraction-digits :min-fraction-digits))
  (when (< min-fraction-digits 0) (error "~S must be non-negative" :min-fraction-digits))
  (when (< min-integer-digits 1) (error "~S must be positive" :min-integer-digits))
  (let* ((destination (if (null stream) 
                          (make-array 8 :element-type 'character :fill-pointer 0 :adjustable t :initial-element #\space)
                          stream))
         (minus (minusp value))
         (sign (if minus minus-symbol plus-symbol))
         (want-sign (or minus force-sign))
         (value (etypecase value
                  (integer (abs value))
                  (rational (round-decimal (abs value) :places max-fraction-digits :mode rounding-mode))
                  (real (round-decimal (rational (abs value)) :places max-fraction-digits :mode rounding-mode)))))
    (multiple-value-bind (integer fraction) (floor value 1)
      (let ((grouped-integer (format-natural integer 
                                             :grouping-symbol grouping-symbol :minimum-width min-integer-digits
                                             :primary-group-size primary-group-size
                                             :secondary-group-size secondary-group-size)))
        (if (zerop fraction)
            (if (zerop min-fraction-digits) 
                (format destination "~A~A" (if want-sign sign "") grouped-integer)
                (format destination "~A~A~A~v,'0D" (if want-sign sign "") grouped-integer decimal-point min-fraction-digits 0))
            ;; XXX - This is really, really stupid! And I am not sure, how
            ;; portable it is.
            (let ((string (subseq (format nil "~,vF" max-fraction-digits fraction) 2)))
              (loop
                :for end := (length string)
                :while (> end min-fraction-digits)
                :unless (eql #\0 (char string (1- end))) 
                  :do (loop-finish)
                :else
                  :do (decf end)
                :finally 
                   (return 
                     (if (zerop end)
                         (format destination "~A~A" (if want-sign sign "") grouped-integer)
                         (format destination "~A~A~A~A"
                                 (if want-sign sign "") grouped-integer 
                                 decimal-point (subseq string 0 end)))))))))
    (unless stream
      (coerce destination 'simple-string))))
