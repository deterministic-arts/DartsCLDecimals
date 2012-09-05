#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Decimal Numbers Library
  Copyright (c) 2012 Dirk Esser

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


(defun valid-gtin-value-p (value)
  "valid-gtin-value-p VALUE => BOOLEAN

Tests, whether VALUE is a well-formed GTIN/EAN code number.
Basically, this function checks the check sum digit."
  (labels
      ((shift (value sum count)
         (if (zerop value) 
             (zerop sum)
             (multiple-value-bind (quotient remainder) (floor value 10)
               (let* ((weight (if (evenp count) 1 3))
                      (new-sum (mod (+ sum (* weight remainder)) 10)))
                 (shift quotient new-sum (1+ count)))))))
    (shift value 0 0)))


(deftype gtin-value (&optional (width '*))
  "gtin-value &optional WIDTH

An non-negative integer value, which fits into WIDTH decimal 
digits, and satisfies the valid-gtin-value-p predicate. If 
WIDTH is omitted or supplied as *, the resulting type does 
not constrain the integer value's upper bound."
  (cond
    ((eq width '*) `(and (integer 0) (satisfies valid-gtin-value-p)))
    ((and (integerp width) (> width 0)) `(and (integer 1 ,(expt 10 width)) (satisfies valid-gtin-value-p)))
    (t (error "invalid GTIN width specification ~S" width))))


(defun find-best-gtin-width (w)
  (cond
    ((<= w 8) 8)
    ((<= w 13) 13)
    (t w)))


(defun format-gtin-value (value &optional (preferred-width nil))
  "format-gtin-value VALUE &optional PREFERRED-WIDTH => STRING

Formats a GTIN/EAN number. If PREFERRED-WIDTH is supplied and not
nil, the function tries to make the result have the width specified,
possibly padding with zero from the left. If the actual value is
too large to fit into PREFERRED-WIDTH characters, or if no value
was specified, the function chooses the \"best\" width according
to the numeric value of VALUE. Note, that the function prefers
widths of 8 or 13 characters."
  (let* ((actual-width (ceiling (log value 10)))
         (width (cond
                  ((null preferred-width) (find-best-gtin-width actual-width))
                  ((<= actual-width preferred-width) preferred-width)
                  (t (find-best-gtin-width actual-width)))))
    (format nil "~v,'0D" width value)))


(defun parse-gtin-value (string &key (start 0) (end nil) (junk-allowed nil))
  "parse-gtin-value STRING &key START END JUNK-ALLOWED => VALUE POSITION ERROR COUNT

Parses a GTIN/EAN string into an integer value. The function parses
the portion of STRING between indices START (default 0, inclusive)
and END (defaults to the length of STRING, exclusive). If parsing
succeeds, the function makes sure, that the value looks like a well-formed
GTIN, including verifying the check sum.

The function returns four values:

  VALUE    the integer value seen
  POSITION index into STRING where parsing stopped
  ERROR    a keyword indicating an error condition or nil in case of success
  COUNT    number of significant digits processed

If JUNK-ALLOWED is false (the default), the function signals a 
condition of type simple-error, if parsing fails. Otherwise, it may
return a partial and/or invalid GTIN number."
  (let ((end (or end (length string))))
    (labels 
        ((fail (code value position count)
           (if junk-allowed
               (values value position code count)
               (error 'gtin-format-error 
                      :partial-result value :digits count
                      :value string :start start :end end :position position
                      :reason code)))
                      
         (parse (position value count)
           (if (>= position end) 
               (cond
                 ((< count 2) (fail :too-short value position count))
                 ((valid-gtin-value-p value) (values value position nil count))
                 (t (fail :invalid-checksum value position count)))
               (let ((char (char string position)))
                 (cond
                   ((char<= #\0 char #\9) 
                    (let ((digit (- (char-code char) #.(char-code #\0))))
                      (parse (1+ position) (+ (* 10 value) digit) (1+ count))))
                   ((char= #\space char) (parse (1+ position) value count))
                   (t (fail :invalid-character value position count)))))))
      (parse start 0 0))))

                  
