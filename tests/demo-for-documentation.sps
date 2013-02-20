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
  (prefix (vicare posix)
	  px.)
  (prefix (vicare mail libesmtp)
	  esmtp.)
  (prefix (vicare mail libesmtp constants)
	  esmtp.)
  (prefix (vicare ffi)
	  ffi.)
  (vicare syntactic-extensions))

(px.signal-bub-init)


;;;; helpers

(define-inline (%pretty-print ?thing)
  (pretty-print ?thing (current-error-port)))


;;;; version functions

(let ()

  (%pretty-print (list 'version-informations
		       (esmtp.vicare-libesmtp-version-interface-current)
		       (esmtp.vicare-libesmtp-version-interface-revision)
		       (esmtp.vicare-libesmtp-version-interface-age)
		       (esmtp.vicare-libesmtp-version)
		       (esmtp.smtp-version)))

  #t)


;;;; send a message using SMTP-SET-MESSAGE-STR

(when #f
  (let ()

    (define message-text
      "From: <marco@localhost>\r\n\
To: <marco@localhost>\r\n\
Subject: demo of vicare/libesmtp\r\n\
\r\n\
This is the text.\r\n")

    (define monitor-cb
      (esmtp.make-smtp-monitorcb
       (lambda (buf.ptr buf.len writing)
	 (fprintf (current-error-port)
		  "monitor: ~a, ~a"
		  (esmtp.smtp-cb->symbol writing)
		  (ffi.cstring->string buf.ptr buf.len)))))

    (define event-cb
      (esmtp.make-smtp-eventcb
       (lambda (session event-no)
	 (fprintf (current-error-port)
		  "event: ~a\n" (esmtp.smtp-event->symbol event-no)))))

    (let* ((sex (esmtp.smtp-create-session))
	   (msg (esmtp.smtp-add-message sex))
	   (rec (esmtp.smtp-add-recipient msg "marco@localhost")))
      (assert (esmtp.smtp-set-monitorcb sex monitor-cb #t))
      (assert (esmtp.smtp-set-server sex "localhost:smtp"))
      (assert (esmtp.smtp-set-eventcb sex event-cb))
      (assert (esmtp.smtp-set-reverse-path msg "marco@localhost"))
      (assert (esmtp.smtp-set-hostname sex "localhost"))
      (assert (esmtp.smtp-set-message-str msg (ffi.string->cstring message-text)))
      (assert (esmtp.smtp-start-session sex))
      (let ((errno (esmtp.smtp-errno)))
	(fprintf (current-error-port)
		 "API errno: ~a (~a), \"~a\"\n"
		 errno
		 (esmtp.smtp-errno->symbol errno)
		 (esmtp.smtp-strerror errno)))
      (fprintf (current-error-port)
	       "message transfer status: ~a\n"
	       (esmtp.smtp-message-transfer-status msg))
      (fprintf (current-error-port)
	       "reverse path status: ~a\n"
	       (esmtp.smtp-reverse-path-status msg))
      (fprintf (current-error-port)
	       "recipient status: ~a\n"
	       (esmtp.smtp-recipient-status rec))
      (fprintf (current-error-port)
	       "recipient complete?: ~a\n"
	       (esmtp.smtp-recipient-check-complete rec))
      (assert (esmtp.smtp-destroy-session sex)))

    #f))


;;;; send a message using SMTP-SET-MESSAGECB

(when #t
  (let ()

    (define message-text
      "From: <marco@localhost>\r\n\
To: <marco@localhost>\r\n\
Subject: demo of vicare/libesmtp\r\n\
\r\n\
This is the text.\r\n")

    (define cstr.ptr (ffi.string->cstring message-text))
    (define cstr.len (ffi.strlen cstr.ptr))

    (define monitor-cb
      (esmtp.make-smtp-monitorcb
       (lambda (buf.ptr buf.len writing)
	 (fprintf (current-error-port)
		  "monitor: ~a, ~a"
		  (esmtp.smtp-cb->symbol writing)
		  (ffi.cstring->string buf.ptr buf.len)))))

    (define event-cb
      (esmtp.make-smtp-eventcb
       (lambda (session event-no)
	 (fprintf (current-error-port)
		  "event: ~a\n" (esmtp.smtp-event->symbol event-no)))))

    (define message-cb
      (esmtp.make-smtp-messagecb
       (let ((sending? #t))
	 (lambda (unused len.ptr)
	   (pretty-print (list 'message-cb 'enter)
			 (current-error-port))
	   (ffi.pointer-set-c-pointer! unused 0 (null-pointer))
	   (if (ffi.pointer-null? len.ptr)
	       ;;If LEN.PTR is set to NULL:  this call is to ask to rewind
	       ;;the message; the return value is not used.
	       (begin
		 (pretty-print (list 'message-cb 'rewind)
			       (current-error-port))
		 (null-pointer))
	     ;;If LEN.PTR is  not NULL: this callback must  a pointer to
	     ;;the  start of  the message  buffer and  set the  location
	     ;;referenced by LEN.PTR to the  number of octets of data in
	     ;;the buffer.
	     (if sending?
		 (begin
		   (ffi.pointer-set-c-signed-int! len.ptr 0 cstr.len)
		   (set! sending? #f)
		   (pretty-print (list 'message-cb 'sending-data
				       cstr.ptr cstr.len
				       (ffi.pointer-ref-c-signed-int len.ptr 0))
				 (current-error-port))
		   cstr.ptr)
	       ;;The  callback is  called  repeatedly  until the  entire
	       ;;message has been processed.   When all the message data
	       ;;has been read the callback should return NULL.
	       (begin
		 (pretty-print (list 'message-cb 'end-of-data)
			       (current-error-port))
		 (ffi.pointer-set-c-signed-int! len.ptr 0 0)
		 cstr.ptr #;(null-pointer))))))))

    (let* ((sex (esmtp.smtp-create-session))
	   (msg (esmtp.smtp-add-message sex))
	   (rec (esmtp.smtp-add-recipient msg "marco@localhost")))
      (assert (esmtp.smtp-set-monitorcb sex monitor-cb #t))
      (assert (esmtp.smtp-set-server sex "localhost:smtp"))
      (assert (esmtp.smtp-set-eventcb sex event-cb))
      (assert (esmtp.smtp-set-reverse-path msg "marco@localhost"))
      (assert (esmtp.smtp-set-hostname sex "localhost"))
      (assert (esmtp.smtp-set-messagecb msg message-cb))
      (assert (esmtp.smtp-start-session sex))
      (let ((errno (esmtp.smtp-errno)))
	(fprintf (current-error-port)
		 "API errno: ~a (~a), \"~a\"\n"
		 errno
		 (esmtp.smtp-errno->symbol errno)
		 (esmtp.smtp-strerror errno)))
      (fprintf (current-error-port)
	       "message transfer status: ~a\n"
	       (esmtp.smtp-message-transfer-status msg))
      (fprintf (current-error-port)
	       "reverse path status: ~a\n"
	       (esmtp.smtp-reverse-path-status msg))
      (fprintf (current-error-port)
	       "recipient status: ~a\n"
	       (esmtp.smtp-recipient-status rec))
      (fprintf (current-error-port)
	       "recipient complete?: ~a\n"
	       (esmtp.smtp-recipient-check-complete rec))
      (assert (esmtp.smtp-destroy-session sex)))

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
      (esmtp.make-smtp-monitorcb
       (lambda (buf.ptr buf.len writing)
	 (fprintf (current-error-port)
		  "monitor: ~a\n" (ffi.cstring->string buf.ptr buf.len)))))

    (define event-cb
      (esmtp.make-smtp-eventcb
       (lambda (session event-no)
	 (fprintf (current-error-port)
		  "event: ~a\n" (esmtp.smtp-event->symbol event-no)))))

    (let* ((sex (esmtp.smtp-create-session))
	   (msg (esmtp.smtp-add-message sex))
	   (rec (esmtp.smtp-add-recipient msg "<mrc.mgg@gmail.com>")))
      (assert (esmtp.smtp-set-hostname sex "localhost"))
      (assert (esmtp.smtp-set-server sex "smtp.gmail.com:25"))
      (assert (esmtp.smtp-set-reverse-path msg "<marco.maggi-ipsu@poste.it>"))
      (assert (esmtp.smtp-set-monitorcb sex monitor-cb #f))
      (assert (esmtp.smtp-set-eventcb sex event-cb))
      (assert (esmtp.smtp-set-message-str msg (ffi.string->cstring message-text)))
      (assert (esmtp.smtp-start-session sex))
      (let ((errno (esmtp.smtp-errno)))
	(fprintf (current-error-port)
		 "API errno: ~a (~a), \"~a\"\n"
		 errno (esmtp.smtp-errno->symbol errno) (esmtp.smtp-strerror errno)))
      (fprintf (current-error-port)
	       "message transfer status: ~a\n" (esmtp.smtp-message-transfer-status msg))
      (fprintf (current-error-port)
	       "reverse path status: ~a\n" (esmtp.smtp-reverse-path-status msg))
      (fprintf (current-error-port)
	       "recipient complete?: ~a\n" (esmtp.smtp-recipient-check-complete rec))
      (assert (esmtp.smtp-destroy-session sex)))

    #f))


;;;; done


;;; end of file
