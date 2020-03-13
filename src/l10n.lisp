#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Decimal Numbers Library
  Copyright (c) 2011, 2015, 2020 Dirk Esser

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

(defpackage #:darts.lib.localized-decimal
  (:use #:common-lisp #:darts.lib.decimal #:darts.lib.locale
        #:darts.lib.locale-categories #:alexandria)
  (:shadow #:format-decimal #:format-integer #:parse-decimal)
  (:local-nicknames (#:decimal #:darts.lib.decimal))
  (:export #:format-decimal #:format-integer #:parse-decimal #:decimal #:round-decimal
           #:decimalp #:number-format-error #:parsing-error #:parsing-error-value
           #:parsing-error-start #:parsing-error-end #:parsing-error-position 
           #:parsing-error-reason #:number-format-error #:number-symbols #:number-symbols-p
           #:read-number-symbols #:number-symbols-decimal-point #:number-symbols-grouping-character
           #:number-symbols-primary-group-size #:number-symbols-secondary-group-size
           #:number-symbols-plus-symbol #:number-symbols-minus-symbol #:number-symbols-default-decimal-places
           #:number-symbols-default-rounding-mode))

(in-package #:darts.lib.localized-decimal)

(define-constant +whitespace+ #.(concatenate 'string '(#\newline #\tab #\return #\space))
                 :test #'string=)

(define-locale-category number-symbols
    ((decimal-point
       :type character :default #\.
       :parser (lambda (value) (char (string-trim +whitespace+ value) 0))
       :documentation "The character used as decimal point.")
     (grouping-symbol
       :type character :default #\,
       :parser (lambda (value) (char (string-trim +whitespace+ value) 0))
       :documentation "The character used as separator between groups of digits.")
     (primary-group-size
       :type integer :default 3
       :parser parse-integer
       :documentation "The number of digits in the \"primary\" group.")
     (secondary-group-size
       :type integer :default 3
       :parser parse-integer
       :documentation "The number of digits in all but the \"primary\" group.")
     (plus-symbol
       :type character :default #\+
       :parser (lambda (value) (char (string-trim +whitespace+ value) 0))
       :documentation "The default symbol used as sign for positive numbers.")
     (minus-symbol
       :type character :default #\-
       :parser (lambda (value) (char (string-trim +whitespace+ value) 0))
       :documentation "The default symbol used as sign for negative numbers.")
     (default-decimal-places
       :type integer :default 2
       :parser parse-integer
       :documentation "The number of decimal places to show by default.")
     (default-rounding-mode
       :type (member :up :down :half-up :half-down :half-even) :default :half-even
       :parser (lambda (value)
                 (or (car (member (string-trim +whitespace+ value) '(:up :down :half-up :half-down :half-even) :test #'string-equal))
                     +inherit+))
       :documentation "The rounding mode applied by default."))
  (:cache-function t))
  
(add-locale-resource-directory (asdf:system-relative-pathname '#:darts.lib.localized-decimal "./data/"))

(defun format-decimal (value
                       &key (stream *standard-output*) (locale *default-locale*)
                         force-sign min-fraction-digits max-fraction-digits
                         (min-integer-digits 1) rounding-mode)
  (let ((syms (if locale (number-symbols (locale locale)) +number-symbols-defaults+)))
    (decimal:format-decimal value
                            :stream stream :force-sign force-sign
                            :min-fraction-digits (or min-fraction-digits (number-symbols-default-decimal-places syms))
                            :max-fraction-digits (or max-fraction-digits (number-symbols-default-decimal-places syms))
                            :min-integer-digits min-integer-digits
                            :primary-group-size (number-symbols-primary-group-size syms)
                            :secondary-group-size (number-symbols-secondary-group-size syms)
                            :decimal-point (number-symbols-decimal-point syms)
                            :grouping-symbol (number-symbols-grouping-symbol syms)
                            :plus-symbol (number-symbols-plus-symbol syms)
                            :minus-symbol (number-symbols-minus-symbol syms)
                            :rounding-mode (or rounding-mode (number-symbols-default-rounding-mode syms))
                            )))

(defun format-integer (value
                       &key (stream *standard-output*) (locale *default-locale*)
                         force-sign)
  (let ((syms (if locale (number-symbols (locale locale)) +number-symbols-defaults+)))
    (decimal:format-integer value
                            :stream stream :force-sign force-sign
                            :primary-group-size (number-symbols-primary-group-size syms)
                            :secondary-group-size (number-symbols-secondary-group-size syms)
                            :grouping-symbol (number-symbols-grouping-symbol syms)
                            :plus-symbol (number-symbols-plus-symbol syms)
                            :minus-symbol (number-symbols-minus-symbol syms))))

(defun parse-decimal (value &key (start 0) end (radix 10) junk-allowed (locale *default-locale*))
  (let ((syms (if locale (number-symbols (locale locale)) +number-symbols-defaults+)))
    (decimal:parse-decimal value :start start :end end :radix radix
                           :junk-allowed junk-allowed
                           :decimal-point (number-symbols-decimal-point syms)
                           :grouping-symbol (number-symbols-grouping-symbol syms))))
  
