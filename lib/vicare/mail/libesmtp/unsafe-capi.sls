;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Libesmtp
;;;Contents: unsafe interface to the C language API
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare mail libesmtp unsafe-capi)
  (export

    ;; version functions
    vicare-libesmtp-version-interface-current
    vicare-libesmtp-version-interface-revision
    vicare-libesmtp-version-interface-age
    vicare-libesmtp-version

;;; --------------------------------------------------------------------
;;; still to be implemented

    )
  (import (vicare))


;;;; helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))


;;;; version functions

(define-inline (vicare-libesmtp-version-interface-current)
  (foreign-call "ikrt_libesmtp_version_interface_current"))

(define-inline (vicare-libesmtp-version-interface-revision)
  (foreign-call "ikrt_libesmtp_version_interface_revision"))

(define-inline (vicare-libesmtp-version-interface-age)
  (foreign-call "ikrt_libesmtp_version_interface_age"))

(define-inline (vicare-libesmtp-version)
  (foreign-call "ikrt_libesmtp_version"))


;;;; still to be implemented

#;(define-inline (vicare-libesmtp)
  (foreign-call "iktr_libesmtp"))


;;;; done

)

;;; end of file
