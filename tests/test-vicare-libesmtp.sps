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
  (vicare mail libesmtp)
  (vicare mail libesmtp constants)
  (prefix (vicare ffi) ffi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare Libesmtp bindings\n")


;;;; helpers



(parametrise ((check-test-name	'version))

  (check
      (fixnum? (vicare-libesmtp-version-interface-current))
    => #t)

  (check
      (fixnum? (vicare-libesmtp-version-interface-revision))
    => #t)

  (check
      (fixnum? (vicare-libesmtp-version-interface-age))
    => #t)

  (check
      (string? (vicare-libesmtp-version))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string? (smtp-version))
    => #t)

  #t)


(parametrise ((check-test-name		'session)
	      (struct-guardian-logger	#f))

  (check
      ;;This will be destroyed by the garbage collector.
      (let ((sex (smtp-create-session)))
	(smtp-session? sex))
    => #t)

  (check
      ;;This will be destroyed by the garbage collector.
      (let ((sex (smtp-create-session)))
	(smtp-session?/alive sex))
    => #t)

  (check
      (let ((sex (smtp-create-session)))
	(smtp-destroy-session sex))
    => (void))

  (check
      (let ((sex (smtp-create-session)))
	(smtp-destroy-session sex)
	(smtp-destroy-session sex)
	(smtp-destroy-session sex))
    => (void))

  (check
      (let ((sex (smtp-create-session)))
	(smtp-destroy-session sex)
	(smtp-session?/alive sex))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((sex (smtp-create-session)))
	(smtp-set-hostname sex "localhost"))
    => #t)

  (check
      (let ((sex (smtp-create-session)))
	(smtp-set-hostname sex))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((sex (smtp-create-session)))
	(smtp-set-server sex "localhost:25"))
    => #t)

  (collect))


(parametrise ((check-test-name		'message)
	      (struct-guardian-logger	#f))

  (check
      ;;This will be destroyed by the garbage collector.
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-session? sex))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-result
       (let* ((sex  (smtp-create-session))
	      (msg1 (smtp-add-message sex))
	      (msg2 (smtp-add-message sex))
	      (cb   (make-smtp-enumerate-messagecb
		     (lambda (msg)
		       (add-result msg)))))
	 (smtp-enumerate-messages sex cb)))
    (=> (lambda (result expected)
	  (and (equal? (void) (car result))
	       (let ((msgs (cadr result)))
		 (and (= 2 (length msgs))
		      (for-all smtp-message? msgs))))))
    #t)

;;; --------------------------------------------------------------------

  (check
      (with-result
       (let* ((sex  (smtp-create-session))
	      (msg1 (smtp-add-message sex))
	      (msg2 (smtp-add-message sex))
	      (cb   (lambda (msg)
		      (add-result msg))))
	 (smtp-enumerate-messages* sex cb)))
    (=> (lambda (result expected)
	  (and (equal? (void) (car result))
	       (let ((msgs (cadr result)))
		 (and (= 2 (length msgs))
		      (for-all smtp-message? msgs))))))
    #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-reverse-path msg "marco@localhost"))
    => #t)

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-reverse-path msg))
    => #t)

  (collect))


(parametrise ((check-test-name		'recipient)
	      (struct-guardian-logger	#f))

  (check
      ;;This will be destroyed by the garbage collector.
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex))
	     (rec (smtp-add-recipient msg "marco@localhost")))
	(smtp-recipient? rec))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-result
       (let* ((sex  (smtp-create-session))
	      (msg  (smtp-add-message sex))
	      (rec1 (smtp-add-recipient msg "marco@localhost"))
	      (rec2 (smtp-add-recipient msg "root@localhost"))
	      (cb   (make-smtp-enumerate-recipientcb
		     (lambda (rec mbox)
		       (add-result (cons rec (ffi.cstring->string mbox)))))))
	 (smtp-enumerate-recipients msg cb)))
    (=> (lambda (result expected)
	  (and (equal? #t (car result))
	       (let ((pairs (cadr result)))
		 (and (= 2 (length pairs))
		      (for-all (lambda (pair)
				 (and (smtp-recipient? (car pair))
				      (string? (cdr pair))))
			pairs))))))
    #t)

;;; --------------------------------------------------------------------

  (check
      (with-result
       (let* ((sex  (smtp-create-session))
	      (msg  (smtp-add-message sex))
	      (rec1 (smtp-add-recipient msg "marco@localhost"))
	      (rec2 (smtp-add-recipient msg "root@localhost"))
	      (cb   (lambda (rec)
		      (add-result rec))))
	 (smtp-enumerate-recipients* msg cb)))
    (=> (lambda (result expected)
	  (and (equal? (void) (car result))
	       (let ((recs (cadr result)))
		 (and (= 2 (length recs))
		      (for-all smtp-recipient? recs))))))
    #t)

  (collect))


(parametrise ((check-test-name		'headers)
	      (struct-guardian-logger	#f))

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-header msg "X-Loop:" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-header msg "Date:" 123000))
    => #t)

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-header msg "Message-Id:" "123"))
    => #t)

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-header msg "From:" "Marco Maggi" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-header msg "Disposition-Notification-To:" "Marco Maggi" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-header msg "To:" "Marco Maggi" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-header msg "Cc:" "Marco Maggi" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-header msg "Bcc:" "Marco Maggi" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-header msg "Reply-To:" "Marco Maggi" "marco@localhost"))
    => #t)

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-header msg "Sender:" "Marco Maggi" "marco@localhost"))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-header-option msg "X-Loop:" Hdr_OVERRIDE))
    => #t)

  (check
      (let* ((sex (smtp-create-session))
	     (msg (smtp-add-message sex)))
	(smtp-set-header-option msg "X-Loop:" Hdr_PROHIBIT))
    => #t)

  (collect))


;;;; done

(check-report)

;;; end of file
