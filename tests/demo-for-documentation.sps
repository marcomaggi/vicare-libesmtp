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
(import (rnrs)
  (only (vicare language-extensions)
	pretty-print
	fprintf)
  (prefix (vicare posix)
	  px.)
  (prefix (vicare mail libesmtp)
	  esmtp.)
  (prefix (vicare mail libesmtp constants)
	  esmtp.)
  (prefix (vicare ffi)
	  ffi.)
  (only (vicare syntactic-extensions)
	unwind-protect))


;;;; helpers

(define-syntax %pretty-print
  (syntax-rules ()
    ((_ ?thing)
     (pretty-print ?thing (current-error-port)))))


;;;; version functions

(when #t

  (%pretty-print (list 'version-informations
		       (esmtp.vicare-libesmtp-version-interface-current)
		       (esmtp.vicare-libesmtp-version-interface-revision)
		       (esmtp.vicare-libesmtp-version-interface-age)
		       (esmtp.vicare-libesmtp-version)
		       (esmtp.smtp-version)))

  #t)


;;;; send a message from localhost to localhost
;;
;;* Use SMTP-SET-MESSAGE-STR.
;;
;;* No debugging callbacks.
;;

(when #t
  (let ()

    (define local-hostname
      "localhost")

    (define smtp-server
      "localhost:smtp")

    (define sender-mailbox
      "marco@localhost")

    (define recipient-mailbox
      "marco@localhost")

    (define message-text
      "From: <marco@localhost>\r\n\
       To: <marco@localhost>\r\n\
       Subject: demo of vicare/libesmtp\r\n\
       \r\n\
       This is the text.\r\n")

    (let* ((sex  (esmtp.smtp-create-session))
	   (msg  (esmtp.smtp-add-message sex))
	   (rec  (esmtp.smtp-add-recipient msg recipient-mailbox))
	   (cstr (ffi.string->cstring message-text)))
      (unwind-protect
	  (begin
	    (esmtp.smtp-set-server sex smtp-server)
	    (esmtp.smtp-set-reverse-path msg sender-mailbox)
	    (esmtp.smtp-set-hostname sex local-hostname)
	    (esmtp.smtp-set-message-str msg cstr)
	    (esmtp.smtp-start-session sex)
	    (fprintf (current-error-port)
		     "recipient complete? ~a\n"
		     (esmtp.smtp-recipient-check-complete rec)))
	(esmtp.smtp-destroy-session sex)))

    #f))


;;;; send a message from localhost to localhost
;;
;;* Use SMTP-SET-MESSAGECB.
;;
;;* No debugging callbacks.
;;

(when #t
  (let ()

    (define local-hostname
      "localhost")

    (define smtp-server
      "localhost:smtp")

    (define sender-mailbox
      "marco@localhost")

    (define recipient-mailbox
      "marco@localhost")

    (define message-text
      "From: <marco@localhost>\r\n\
       To: <marco@localhost>\r\n\
       Subject: demo of vicare/libesmtp\r\n\
       \r\n\
       This is the text.\r\n")

    (define (make-message-cb message-text)
      (let ((cstr.ptr	#f)
	    (cstr.len	#f))
	(lambda (unused len.ptr)
	  (cond ((ffi.pointer-null? len.ptr)
		 ;;If LEN.PTR  is set to NULL:  this call is to  ask the
		 ;;application to  rewind the message; the  return value
		 ;;is not used, but it must be a pointer.
		 (set! cstr.ptr (ffi.string->cstring message-text))
		 (set! cstr.len (ffi.strlen cstr.ptr))
		 (ffi.null-pointer))
		(cstr.len
		 ;;If LEN.PTR is  not NULL: this callback  must return a
		 ;;pointer to  the start of  the next message  chunk and
		 ;;set the location referenced  by LEN.PTR to the number
		 ;;of octets of data in the buffer.
		 (ffi.pointer-set-c-signed-int! len.ptr 0 cstr.len)
		 (set! cstr.len #f)
		 cstr.ptr)
		(else
		 ;;The callback  is called  repeatedly until  the entire
		 ;;message  has been  processed.  When  all the  message
		 ;;data has been read the callback must return NULL.
		 (ffi.pointer-set-c-signed-int! len.ptr 0 0)
		 (ffi.null-pointer))))))

    (let* ((sex  (esmtp.smtp-create-session))
	   (msg  (esmtp.smtp-add-message sex))
	   (rec  (esmtp.smtp-add-recipient msg recipient-mailbox))
	   (mcb  (esmtp.make-smtp-messagecb
		  (make-message-cb message-text))))
      (unwind-protect
	  (begin
	    (esmtp.smtp-set-server sex smtp-server)
	    (esmtp.smtp-set-reverse-path msg sender-mailbox)
	    (esmtp.smtp-set-hostname sex local-hostname)
	    (esmtp.smtp-set-messagecb msg mcb)
	    (esmtp.smtp-start-session sex)
	    (fprintf (current-error-port)
		     "recipient complete? ~a\n"
		     (esmtp.smtp-recipient-check-complete rec)))
	(esmtp.smtp-destroy-session sex)
	(ffi.free-c-callback mcb)))

    #f))


;;;; send a message from localhost to localhost
;;
;;* Use SMTP-SET-MESSAGE-STR.
;;
;;* Show monitor callback.
;;
;;* Show event callback.
;;

(when #t
  (let ()

    (define local-hostname
      "localhost")

    (define smtp-server
      "localhost:smtp")

    (define sender-mailbox
      "marco@localhost")

    (define recipient-mailbox
      "marco@localhost")

    (define message-text
      "From: <marco@localhost>\r\n\
       To: <marco@localhost>\r\n\
       Subject: demo of vicare/libesmtp\r\n\
       \r\n\
       This is the text.\r\n")

    (define (monitor-cb buf.ptr buf.len writing)
      (fprintf (current-error-port)
	       "monitor: ~a, ~a"
	       (esmtp.smtp-cb->symbol writing)
	       (ffi.cstring->string buf.ptr buf.len)))

    (define (event-cb session event-no)
      (fprintf (current-error-port)
	       "event: ~a\n"
	       (esmtp.smtp-event->symbol event-no)))

    (let* ((sex  (esmtp.smtp-create-session))
	   (msg  (esmtp.smtp-add-message sex))
	   (rec  (esmtp.smtp-add-recipient msg recipient-mailbox))
	   (cstr (ffi.string->cstring message-text))
	   (mcb  (esmtp.make-smtp-monitorcb monitor-cb))
	   (ecb  (esmtp.make-smtp-eventcb   event-cb)))
      (unwind-protect
	  (begin
	    (esmtp.smtp-set-monitorcb sex mcb #t)
	    (esmtp.smtp-set-eventcb   sex ecb)
	    (esmtp.smtp-set-server sex smtp-server)
	    (esmtp.smtp-set-reverse-path msg sender-mailbox)
	    (esmtp.smtp-set-hostname sex local-hostname)
	    (esmtp.smtp-set-message-str msg cstr)
	    (esmtp.smtp-start-session sex)
	    (fprintf (current-error-port)
		     "recipient complete? ~a\n"
		     (esmtp.smtp-recipient-check-complete rec)))
	(esmtp.smtp-destroy-session sex)
	(ffi.free-c-callback mcb)
	(ffi.free-c-callback ecb)))

    #f))


;;;; send a message from localhost to localhost
;;
;;* Use SMTP-SET-MESSAGE-STR.
;;
;;* Show monitor callback.
;;
;;* Show event callback.
;;
;;* At the end print result informations.
;;

(when #t
  (let ()

    (define local-hostname
      "localhost")

    (define smtp-server
      "localhost:smtp")

    (define sender-mailbox
      "marco@localhost")

    (define recipient-mailbox
      "marco@localhost")

    (define message-text
      "From: <marco@localhost>\r\n\
       To: <marco@localhost>\r\n\
       Subject: demo of vicare/libesmtp\r\n\
       \r\n\
       This is the text.\r\n")

    (define (monitor-cb buf.ptr buf.len writing)
      (fprintf (current-error-port)
	       "monitor: ~a, ~a"
	       (esmtp.smtp-cb->symbol writing)
	       (ffi.cstring->string buf.ptr buf.len)))

    (define (event-cb session event-no)
      (fprintf (current-error-port)
	       "event: ~a\n"
	       (esmtp.smtp-event->symbol event-no)))

    (let* ((sex  (esmtp.smtp-create-session))
	   (msg  (esmtp.smtp-add-message sex))
	   (rec  (esmtp.smtp-add-recipient msg recipient-mailbox))
	   (cstr (ffi.string->cstring message-text))
	   (mcb  (esmtp.make-smtp-monitorcb monitor-cb))
	   (ecb  (esmtp.make-smtp-eventcb   event-cb)))
      (unwind-protect
	  (begin
	    (esmtp.smtp-set-monitorcb sex mcb #t)
	    (esmtp.smtp-set-eventcb   sex ecb)
	    (esmtp.smtp-set-server sex smtp-server)
	    (esmtp.smtp-set-reverse-path msg sender-mailbox)
	    (esmtp.smtp-set-hostname sex local-hostname)
	    (esmtp.smtp-set-message-str msg cstr)
	    (esmtp.smtp-start-session sex)

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
		     (esmtp.smtp-recipient-check-complete rec)))
	(esmtp.smtp-destroy-session sex)
	(ffi.free-c-callback mcb)
	(ffi.free-c-callback ecb)))

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
