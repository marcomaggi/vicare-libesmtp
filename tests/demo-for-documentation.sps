;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Libesmtp
;;;Contents: proof for documentation
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


#!r6rs
(import (vicare)
  (vicare mail libesmtp)
  (vicare mail libesmtp constants)
  (prefix (vicare ffi) ffi.)
  (vicare syntactic-extensions))


;;;; helpers

(define-inline (%pretty-print ?thing)
  (pretty-print ?thing (current-error-port)))


;;;; version functions

(let ()

  (%pretty-print (list 'version-informations
		       (vicare-libesmtp-version-interface-current)
		       (vicare-libesmtp-version-interface-revision)
		       (vicare-libesmtp-version-interface-age)
		       (vicare-libesmtp-version)
		       (smtp-version)))

  #t)


;;;; send a message

(when #t
  (let ()

    (define message-text
      "From: <marco@localhost>\r\n
To: <root@localhost>\r\n
Subject: demo of vicare/libesmtp\r\n
\r\n
This is the text.\r\n")

    (define monitor-cb
      (make-smtp-monitorcb
       (lambda (buf.ptr buf.len writing)
	 (fprintf (current-error-port)
		  "monitor: ~a, ~a\n" writing (ffi.cstring->string buf.ptr buf.len)))))

    (define event-cb
      (make-smtp-eventcb
       (lambda (session event-no)
	 (fprintf (current-error-port)
		  "event: ~a\n" (smtp-event->symbol event-no)))))

    (let* ((sex (smtp-create-session))
	   (msg (smtp-add-message sex))
	   (rec (smtp-add-recipient msg "<marco@localhost>")))
      (assert (smtp-set-hostname sex "localhost"))
      (assert (smtp-set-server sex "localhost:smtp"))
      (assert (smtp-set-reverse-path msg "<marco@localhost>"))
      (assert (smtp-set-monitorcb sex monitor-cb #t))
      (assert (smtp-set-eventcb sex event-cb))
      (assert (smtp-set-message-str msg (ffi.string->cstring message-text)))
      (assert (smtp-start-session sex))
      (let ((errno (smtp-errno)))
	(fprintf (current-error-port)
		 "API errno: ~a (~a), \"~a\"\n"
		 errno (smtp-errno->symbol errno) (smtp-strerror errno)))
      (fprintf (current-error-port)
	       "message transfer status: ~a\n" (smtp-message-transfer-status msg))
      (fprintf (current-error-port)
	       "reverse path status: ~a\n" (smtp-reverse-path-status msg))
      (fprintf (current-error-port)
	       "recipient status: ~a\n" (smtp-recipient-status rec))
      (fprintf (current-error-port)
	       "recipient complete?: ~a\n" (smtp-recipient-check-complete rec))
      (assert (smtp-destroy-session sex)))

    #f))


;;;; send a message to gmail

(when #f
  (let ()

    (define message-text
      "From: <marco.maggi-ipsu@poste.it>\r\n\
     To: <mrc.mgg@gmail.com>\r\n\
     Subject: demo of vicare/libesmtp\r\n\
     \r\n\
     This is the text.\r\n\r\n")

    (define monitor-cb
      (make-smtp-monitorcb
       (lambda (buf.ptr buf.len writing)
	 (fprintf (current-error-port)
		  "monitor: ~a\n" (ffi.cstring->string buf.ptr buf.len)))))

    (define event-cb
      (make-smtp-eventcb
       (lambda (session event-no)
	 (fprintf (current-error-port)
		  "event: ~a\n" (smtp-event->symbol event-no)))))

    (let* ((sex (smtp-create-session))
	   (msg (smtp-add-message sex))
	   (rec (smtp-add-recipient msg "<mrc.mgg@gmail.com>")))
      (assert (smtp-set-hostname sex "localhost"))
      (assert (smtp-set-server sex "smtp.gmail.com:25"))
      (assert (smtp-set-reverse-path msg "<marco.maggi-ipsu@poste.it>"))
      (assert (smtp-set-monitorcb sex monitor-cb #f))
      (assert (smtp-set-eventcb sex event-cb))
      (assert (smtp-set-message-str msg (ffi.string->cstring message-text)))
      (assert (smtp-start-session sex))
      (let ((errno (smtp-errno)))
	(fprintf (current-error-port)
		 "API errno: ~a (~a), \"~a\"\n"
		 errno (smtp-errno->symbol errno) (smtp-strerror errno)))
      (fprintf (current-error-port)
	       "message transfer status: ~a\n" (smtp-message-transfer-status msg))
      (fprintf (current-error-port)
	       "reverse path status: ~a\n" (smtp-reverse-path-status msg))
      (fprintf (current-error-port)
	       "recipient complete?: ~a\n" (smtp-recipient-check-complete rec))
      (assert (smtp-destroy-session sex)))

    #f))


;;;; done


;;; end of file
