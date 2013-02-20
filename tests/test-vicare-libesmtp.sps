;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Libesmtp
;;;Contents: tests for Libesmtp bindings
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
  (prefix (vicare mail libesmtp)
	  esmtp.)
  (prefix (vicare mail libesmtp constants)
	  esmtp.)
  (prefix (vicare ffi) ffi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare Libesmtp bindings\n")


;;;; helpers



(parametrise ((check-test-name	'version))

  (check
      (fixnum? (esmtp.vicare-libesmtp-version-interface-current))
    => #t)

  (check
      (fixnum? (esmtp.vicare-libesmtp-version-interface-revision))
    => #t)

  (check
      (fixnum? (esmtp.vicare-libesmtp-version-interface-age))
    => #t)

  (check
      (string? (esmtp.vicare-libesmtp-version))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string? (esmtp.smtp-version))
    => #t)

  #t)


(parametrise ((check-test-name		'session)
	      (struct-guardian-logger	#f))

  (check
      ;;This will be destroyed by the garbage collector.
      (let ((sex (esmtp.smtp-create-session)))
	(esmtp.smtp-session? sex))
    => #t)

  (check
      ;;This will be destroyed by the garbage collector.
      (let ((sex (esmtp.smtp-create-session)))
	(esmtp.smtp-session?/alive sex))
    => #t)

  (check
      (let ((sex (esmtp.smtp-create-session)))
	(esmtp.smtp-destroy-session sex))
    => (void))

  (check
      (let ((sex (esmtp.smtp-create-session)))
	(esmtp.smtp-destroy-session sex)
	(esmtp.smtp-destroy-session sex)
	(esmtp.smtp-destroy-session sex))
    => (void))

  (check
      (let ((sex (esmtp.smtp-create-session)))
	(esmtp.smtp-destroy-session sex)
	(esmtp.smtp-session?/alive sex))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((sex (esmtp.smtp-create-session)))
	(esmtp.smtp-set-hostname sex "localhost"))
    => #t)

  (check
      (let ((sex (esmtp.smtp-create-session)))
	(esmtp.smtp-set-hostname sex))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((sex (esmtp.smtp-create-session)))
	(esmtp.smtp-set-server sex "localhost:25"))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((sex (esmtp.smtp-create-session)))
	(esmtp.smtp-set-timeout sex esmtp.Timeout_GREETING 123))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((sex (esmtp.smtp-create-session))
	    (cb  (esmtp.make-smtp-eventcb
		  (lambda (session event-no)
		    (void)))))
	(esmtp.smtp-set-eventcb sex cb))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((sex (esmtp.smtp-create-session))
	    (cb  (esmtp.make-smtp-monitorcb
		  (lambda (buf.ptr buf.len writing)
		    (void)))))
	(esmtp.smtp-set-monitorcb sex cb #f))
    => #t)

  (check
      (let ((sex (esmtp.smtp-create-session))
	    (cb  (esmtp.make-smtp-monitorcb
		  (lambda (buf.ptr buf.len writing)
		    (void)))))
	(esmtp.smtp-set-monitorcb sex cb 123))
    => #t)


  (collect))


(parametrise ((check-test-name		'message)
	      (struct-guardian-logger	#f))

  (check
      ;;This will be destroyed by the garbage collector.
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-message? msg))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-result
       (let* ((sex  (esmtp.smtp-create-session))
	      (msg1 (esmtp.smtp-add-message sex))
	      (msg2 (esmtp.smtp-add-message sex))
	      (cb   (esmtp.make-smtp-enumerate-messagecb
		     (lambda (msg)
		       (add-result msg)))))
	 (esmtp.smtp-enumerate-messages sex cb)))
    (=> (lambda (result expected)
	  (and (equal? (void) (car result))
	       (let ((msgs (cadr result)))
		 (and (= 2 (length msgs))
		      (for-all esmtp.smtp-message? msgs))))))
    #t)

;;; --------------------------------------------------------------------

  (check
      (with-result
       (let* ((sex  (esmtp.smtp-create-session))
	      (msg1 (esmtp.smtp-add-message sex))
	      (msg2 (esmtp.smtp-add-message sex))
	      (cb   (lambda (msg)
		      (add-result msg))))
	 (esmtp.smtp-enumerate-messages* sex cb)))
    (=> (lambda (result expected)
	  (and (equal? (void) (car result))
	       (let ((msgs (cadr result)))
		 (and (= 2 (length msgs))
		      (for-all esmtp.smtp-message? msgs))))))
    #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-reverse-path msg "marco@localhost"))
    => #t)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-reverse-path msg))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex))
	     (cb  (esmtp.make-smtp-messagecb (lambda (dummy len-pointer)
					       (null-pointer)))))
	(esmtp.smtp-set-messagecb msg cb))
    => #t)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-message-fp msg (null-pointer)))
    => #t)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-message-str msg "From: marco@localhost\n\r\
                                   To: marco@localhost\n\r\

                                   ciao\n\r"))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-message-reset-status msg))
    => #t)

  (collect))


(parametrise ((check-test-name		'recipient)
	      (struct-guardian-logger	#f))

  (check
      ;;This will be destroyed by the garbage collector.
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex))
	     (rec (esmtp.smtp-add-recipient msg "marco@localhost")))
	(esmtp.smtp-recipient? rec))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-result
       (let* ((sex  (esmtp.smtp-create-session))
	      (msg  (esmtp.smtp-add-message sex))
	      (rec1 (esmtp.smtp-add-recipient msg "marco@localhost"))
	      (rec2 (esmtp.smtp-add-recipient msg "root@localhost"))
	      (cb   (esmtp.make-smtp-enumerate-recipientcb
		     (lambda (rec mbox)
		       (add-result (cons rec (ffi.cstring->string mbox)))))))
	 (esmtp.smtp-enumerate-recipients msg cb)))
    (=> (lambda (result expected)
	  (and (equal? #t (car result))
	       (let ((pairs (cadr result)))
		 (and (= 2 (length pairs))
		      (for-all (lambda (pair)
				 (and (esmtp.smtp-recipient? (car pair))
				      (string? (cdr pair))))
			pairs))))))
    #t)

;;; --------------------------------------------------------------------

  (check
      (with-result
       (let* ((sex  (esmtp.smtp-create-session))
	      (msg  (esmtp.smtp-add-message sex))
	      (rec1 (esmtp.smtp-add-recipient msg "marco@localhost"))
	      (rec2 (esmtp.smtp-add-recipient msg "root@localhost"))
	      (cb   (lambda (rec)
		      (add-result rec))))
	 (esmtp.smtp-enumerate-recipients* msg cb)))
    (=> (lambda (result expected)
	  (and (equal? (void) (car result))
	       (let ((recs (cadr result)))
		 (and (= 2 (length recs))
		      (for-all esmtp.smtp-recipient? recs))))))
    #t)

;;; --------------------------------------------------------------------

  (check
      (let ((sex (esmtp.smtp-create-session)))
	(esmtp.smtp-option-require-all-recipients sex #t))
    => #t)

  (check
      (let ((sex (esmtp.smtp-create-session)))
	(esmtp.smtp-option-require-all-recipients sex #f))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex))
	     (rec (esmtp.smtp-add-recipient msg "marco@localhost")))
	(esmtp.smtp-recipient-reset-status rec))
    => #t)

  (collect))


(parametrise ((check-test-name		'headers)
	      (struct-guardian-logger	#f))

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-header msg "X-Loop" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-header msg "Date" 123000))
    => #t)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-header msg "Message-Id" "123"))
    => #t)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-header msg "From" "Marco Maggi" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-header msg "Disposition-Notification-To" "Marco Maggi" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-header msg "To" "Marco Maggi" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-header msg "Cc" "Marco Maggi" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-header msg "Bcc" "Marco Maggi" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-header msg "Reply-To" "Marco Maggi" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-header msg "Sender" "Marco Maggi" "marco@localhost"))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-header-option msg "X-Loop" esmtp.Hdr_OVERRIDE))
    => #t)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-header-option msg "X-Loop" esmtp.Hdr_PROHIBIT))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-resent-headers msg #t))
    => #f)

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-set-resent-headers msg #f))
    => #t)

  (collect))


(parametrise ((check-test-name		'data)
	      (struct-guardian-logger	#f))

  (check
      (let ((sex (esmtp.smtp-create-session)))
	(esmtp.smtp-set-application-data sex (integer->pointer 123))
	(esmtp.smtp-get-application-data sex))
    => (integer->pointer 123))

;;; --------------------------------------------------------------------

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex)))
	(esmtp.smtp-message-set-application-data msg (integer->pointer 123))
	(esmtp.smtp-message-get-application-data msg))
    => (integer->pointer 123))

;;; --------------------------------------------------------------------

  (check
      (let* ((sex (esmtp.smtp-create-session))
	     (msg (esmtp.smtp-add-message sex))
	     (rec (esmtp.smtp-add-recipient msg "marco@localhost")))
	(esmtp.smtp-recipient-set-application-data rec (integer->pointer 123))
	(esmtp.smtp-recipient-get-application-data rec))
    => (integer->pointer 123))

  (collect))


(parametrise ((check-test-name		'headers))

  (check
      (esmtp.smtp-event->symbol esmtp.SMTP_EV_MAILSTATUS)
    => 'SMTP_EV_MAILSTATUS)

  (check
      (esmtp.smtp-errno->symbol esmtp.SMTP_ERR_INVALID_RESPONSE_SYNTAX)
    => 'SMTP_ERR_INVALID_RESPONSE_SYNTAX)

  (check
      (esmtp.smtp-timeout->symbol esmtp.Timeout_GREETING)
    => 'Timeout_GREETING)

  (check
      (esmtp.smtp-cb->symbol esmtp.SMTP_CB_READING)
    => 'SMTP_CB_READING)

  (check
      (esmtp.smtp-hdr->symbol esmtp.Hdr_OVERRIDE)
    => 'Hdr_OVERRIDE)

  (check
      (esmtp.smtp-notify->symbol esmtp.Notify_NOTSET)
    => 'Notify_NOTSET)

  (check
      (esmtp.smtp-e8bitmime->symbol esmtp.E8bitmime_NOTSET)
    => 'E8bitmime_NOTSET)

  (check
      (esmtp.smtp-by->symbol esmtp.By_NOTSET)
    => 'By_NOTSET)

  (check
      (esmtp.smtp-starttls->symbol esmtp.Starttls_DISABLED)
    => 'Starttls_DISABLED)

  (check
      (esmtp.smtp-ret->symbol esmtp.Ret_NOTSET)
    => 'Ret_NOTSET)

  #t)


;;;; done

(check-report)

;;; end of file
