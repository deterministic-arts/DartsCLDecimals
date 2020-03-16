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

(defpackage #:darts.lib.decimal
  (:use #:common-lisp #:darts.lib.locale #:darts.lib.locale-categories #:alexandria)
  (:export #:round-decimal #:parse-decimal #:*decimal-places* #:decimal
           #:decimalp #:rounding-mode #:*rounding-mode* #:number-format-error
           #:parsing-error #:parsing-error-value #:parsing-error-start
           #:parsing-error-end #:parsing-error-position 
           #:parsing-error-reason #:number-format-error #:format-integer
           #:format-decimal #:localized-decimal-point #:localized-grouping-symbol
           #:localized-plus-symbol #:localized-minus-symbol #:localized-primary-group-size
           #:localized-secondary-group-size #:number-symbols #:make-number-symbols))

