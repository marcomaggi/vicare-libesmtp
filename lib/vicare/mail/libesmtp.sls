;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Libesmtp
;;;Contents: Libesmtp binding backend
;;;Date: Thu Feb 14, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
#!(load-shared-library "vicare-libesmtp")
(library (vicare mail libesmtp)
  (export

    ;; version numbers and strings
    vicare-libesmtp-version-interface-current
    vicare-libesmtp-version-interface-revision
    vicare-libesmtp-version-interface-age
    vicare-libesmtp-version

    )
  (import (vicare)
    (vicare mail libesmtp constants)
    (prefix (vicare mail libesmtp unsafe-capi) capi.)
    (vicare syntactic-extensions)
    #;(prefix (vicare words) words.))


;;;; arguments validation

#;(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))

#;(define-argument-validation (pointer who obj)
  (pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

#;(define-argument-validation (callback who obj)
  (pointer? obj)
  (assertion-violation who "expected callback as argument" obj))

#;(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))



;;;; version functions

(define (vicare-libesmtp-version-interface-current)
  (capi.vicare-libesmtp-version-interface-current))

(define (vicare-libesmtp-version-interface-revision)
  (capi.vicare-libesmtp-version-interface-revision))

(define (vicare-libesmtp-version-interface-age)
  (capi.vicare-libesmtp-version-interface-age))

(define (vicare-libesmtp-version)
  (ascii->string (capi.vicare-libesmtp-version)))


;;;; done

#;(set-rtd-printer! (type-descriptor XML_ParsingStatus) %struct-XML_ParsingStatus-printer)

#;(post-gc-hooks (cons %free-allocated-parser (post-gc-hooks)))

)

;;; end of file
