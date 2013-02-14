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

    smtp-version

    ;; session management
    smtp-session
    smtp-session?			smtp-session?/alive
    smtp-session.vicare-arguments-validation
    smtp-session/alive.vicare-arguments-validation

    smtp-create-session
    smtp-destroy-session

;;; --------------------------------------------------------------------
;;; still to be implemented

    smtp-add-message
    smtp-enumerate-messages
    smtp-set-server
    smtp-set-hostname
    smtp-set-reverse-path
    smtp-add-recipient
    smtp-enumerate-recipients
    smtp-set-header
    smtp-set-header-option
    smtp-set-resent-headers
    smtp-set-messagecb
    smtp-set-eventcb
    smtp-set-monitorcb
    smtp-start-session
    smtp-message-transfer-status
    smtp-reverse-path-status
    smtp-message-reset-status
    smtp-recipient-status
    smtp-recipient-check-complete
    smtp-recipient-reset-status
    smtp-errno
    smtp-strerror
    smtp-set-application-data
    smtp-get-application-data
    smtp-message-set-application-data
    smtp-message-get-application-data
    smtp-recipient-set-application-data
    smtp-recipient-get-application-data
    smtp-option-require-all-recipients
    smtp-auth-set-context
    smtp-set-timeout
    smtp-dsn-set-ret
    smtp-dsn-set-envid
    smtp-dsn-set-notify
    smtp-dsn-set-orcpt
    smtp-size-set-estimate
    smtp-8bitmime-set-body
    smtp-deliverby-set-mode
    smtp-starttls-enable
    smtp-starttls-set-ctx
    smtp-starttls-set-password-cb
    smtp-etrn-add-node
    smtp-etrn-enumerate-nodes
    smtp-etrn-node-status
    smtp-etrn-set-application-data
    smtp-etrn-get-application-data
    )
  (import (vicare)
    (vicare mail libesmtp constants)
    (prefix (vicare mail libesmtp unsafe-capi) capi.)
    (vicare syntactic-extensions)
    (vicare arguments validation)
    #;(prefix (vicare words) words.))


;;;; arguments validation

(define-argument-validation (smtp-session who obj)
  (smtp-session? obj)
  (assertion-violation who "expected smtp-session structure as argument" obj))

(define-argument-validation (smtp-session/alive who obj)
  (smtp-session?/alive obj)
  (assertion-violation who "expected live smtp-session structure as argument" obj))


;;;; helpers

(define-syntax %struct-destructor-application
  ;;Data structures might have a field called DESTRUCTOR holding #f or a
  ;;function  to be  applied to  the struct  instance upon  finalisation
  ;;(either when the finaliser is  explicitly called by the application,
  ;;or when  the garbage collector  performs the finalisation  through a
  ;;guardian).
  ;;
  ;;This macro should  be used in the finalisation  function to properly
  ;;apply the destructor to the structure.
  ;;
  ;;For example, given the definition:
  ;;
  ;;  (define-struct the-type (the-field destructor))
  ;;
  ;;the code:
  ;;
  ;;  (define (%unsafe.the-type-final struct)
  ;;    (%struct-destructor-application struct
  ;;      the-type-destructor set-the-type-destructor!))
  ;;
  ;;expands to:
  ;;
  ;;  (define (%unsafe.the-type-final struct)
  ;;    (let ((destructor (the-type-destructor struct)))
  ;;      (when destructor
  ;;        (guard (E (else (void)))
  ;;          (destructor struct))
  ;;        (?mutator ?struct #f))))
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?struct ?accessor ?mutator)
       (and (identifier? #'?struct)
	    (identifier? #'?accessor))
       #'(let ((destructor (?accessor ?struct)))
	   (when destructor
	     (guard (E (else (void)))
	       (destructor ?struct))
	     (?mutator ?struct #f)))))))


;;;; version functions

(define (vicare-libesmtp-version-interface-current)
  (capi.vicare-libesmtp-version-interface-current))

(define (vicare-libesmtp-version-interface-revision)
  (capi.vicare-libesmtp-version-interface-revision))

(define (vicare-libesmtp-version-interface-age)
  (capi.vicare-libesmtp-version-interface-age))

(define (vicare-libesmtp-version)
  (ascii->string (capi.vicare-libesmtp-version)))

;;; --------------------------------------------------------------------

(define (smtp-version)
  (define who 'smtp-version)
  (let ((rv (capi.smtp-version)))
    (and rv (ascii->string rv))))


;;;; data structures: session

(define-struct smtp-session
  (pointer
		;Pointer  object  equivalent to  an  instance  of the  C
		;language type "smtp_session_t".
   owner?
		;Boolean, true if this Scheme  structure is the owner of
		;the data structure referenced by the "pointer" field.
   destructor
		;False or a user-supplied function to be called whenever
		;this instance  is closed.  The function  must accept at
		;least one argument being the data structure itself.
   ))

(define (%unsafe.smtp-destroy-session session)
  (%struct-destructor-application session
    $smtp-session-destructor $set-smtp-session-destructor!)
  (when ($smtp-session-owner? session)
    (capi.smtp-destroy-session session)))

(define (smtp-session?/alive obj)
  (and (smtp-session? obj)
       (not (pointer-null? ($smtp-session-pointer obj)))))

(define (%struct-smtp-session-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[smtp-session")
  (%display " pointer=")	(%display ($smtp-session-pointer  S))
  (%display " owner?=")		(%write   ($smtp-session-owner?   S))
  (%display "]"))


;;;; data structures: message

(define-struct smtp-message
  (pointer
		;Pointer  object  equivalent to  an  instance  of the  C
		;language type "smtp_message_t".
   owner?
		;Boolean, true if this Scheme  structure is the owner of
		;the data structure referenced by the "pointer" field.
   destructor
		;False or a user-supplied function to be called whenever
		;this instance  is closed.  The function  must accept at
		;least one argument being the data structure itself.
   ))

(define (%unsafe.smtp-message-close message)
  (%struct-destructor-application message
    $smtp-message-destructor $set-smtp-message-destructor!)
  #;(when ($smtp-message-owner? message)
    (capi.smtp-destroy-message message)))

(define (smtp-message?/open obj)
  (and (smtp-message? obj)
       (not (pointer-null? ($smtp-message-pointer obj)))))

(define (%struct-smtp-message-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[smtp-message")
  (%display " pointer=")	(%display ($smtp-message-pointer  S))
  (%display " owner?=")		(%write   ($smtp-message-owner?   S))
  (%display "]"))


;;;; data structures: recipient

(define-struct smtp-recipient
  (pointer
		;Pointer  object  equivalent to  an  instance  of the  C
		;language type "smtp_recipient_t".
   owner?
		;Boolean, true if this Scheme  structure is the owner of
		;the data structure referenced by the "pointer" field.
   destructor
		;False or a user-supplied function to be called whenever
		;this instance  is closed.  The function  must accept at
		;least one argument being the data structure itself.
   ))

(define (%unsafe.smtp-recipient-close recipient)
  (%struct-destructor-application recipient
    $smtp-recipient-destructor $set-smtp-recipient-destructor!)
  #;(when ($smtp-recipient-owner? recipient)
    (capi.smtp-destroy-recipient recipient)))

(define (smtp-recipient?/open obj)
  (and (smtp-recipient? obj)
       (not (pointer-null? ($smtp-recipient-pointer obj)))))

(define (%struct-smtp-recipient-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[smtp-recipient")
  (%display " pointer=")	(%display ($smtp-recipient-pointer  S))
  (%display " owner?=")		(%write   ($smtp-recipient-owner?   S))
  (%display "]"))


;;;; session management

(define (smtp-create-session)
  (let ((rv (capi.smtp-create-session)))
    (and rv (make-smtp-session rv #t #f))))

(define (smtp-destroy-session session)
  (define who 'smtp-destroy-session)
  (with-arguments-validation (who)
      ((smtp-session	session))
    (%unsafe.smtp-destroy-session session)))


;;;; callback makers

;; void (*smtp_enumerate_recipientcb_t)
;;		(smtp_recipient_t recipient,
;;		 const char *mailbox,
;;		 void *arg);

;; const char *(*smtp_messagecb_t)
;;		(void **ctx,
;;		 int *len,
;;		 void *arg);

;; void (*smtp_eventcb_t)
;;		(smtp_session_t session,
;;		 int event_no,
;;		 void *arg,
;;		 ...);

;; void (*smtp_monitorcb_t)
;;		(const char *buf,
;;		 int buflen,
;;		 int writing,
;;		 void *arg);

;; int (*smtp_starttls_passwordcb_t)
;;		(char *buf,
;;		 int buflen,
;;		 int rwflag,
;;		 void *arg);

;; void (*smtp_etrn_enumerate_nodecb_t)
;;		(smtp_etrn_node_t node,
;;		 int option,
;;		 const char *domain,
;;		 void *arg);


;;;; still to be implemented

(define (smtp-add-message)
  (define who 'smtp-add-message)
  (with-arguments-validation (who)
      ()
    (capi.smtp-add-message)))

(define (smtp-enumerate-messages)
  (define who 'smtp-enumerate-messages)
  (with-arguments-validation (who)
      ()
    (capi.smtp-enumerate-messages)))

(define (smtp-set-server)
  (define who 'smtp-set-server)
  (with-arguments-validation (who)
      ()
    (capi.smtp-set-server)))

(define (smtp-set-hostname)
  (define who 'smtp-set-hostname)
  (with-arguments-validation (who)
      ()
    (capi.smtp-set-hostname)))

(define (smtp-set-reverse-path)
  (define who 'smtp-set-reverse-path)
  (with-arguments-validation (who)
      ()
    (capi.smtp-set-reverse-path)))

(define (smtp-add-recipient)
  (define who 'smtp-add-recipient)
  (with-arguments-validation (who)
      ()
    (capi.smtp-add-recipient)))

(define (smtp-enumerate-recipients)
  (define who 'smtp-enumerate-recipients)
  (with-arguments-validation (who)
      ()
    (capi.smtp-enumerate-recipients)))

(define (smtp-set-header)
  (define who 'smtp-set-header)
  (with-arguments-validation (who)
      ()
    (capi.smtp-set-header)))

(define (smtp-set-header-option)
  (define who 'smtp-set-header-option)
  (with-arguments-validation (who)
      ()
    (capi.smtp-set-header-option)))

(define (smtp-set-resent-headers)
  (define who 'smtp-set-resent-headers)
  (with-arguments-validation (who)
      ()
    (capi.smtp-set-resent-headers)))

(define (smtp-set-messagecb)
  (define who 'smtp-set-messagecb)
  (with-arguments-validation (who)
      ()
    (capi.smtp-set-messagecb)))

(define (smtp-set-eventcb)
  (define who 'smtp-set-eventcb)
  (with-arguments-validation (who)
      ()
    (capi.smtp-set-eventcb)))

(define (smtp-set-monitorcb)
  (define who 'smtp-set-monitorcb)
  (with-arguments-validation (who)
      ()
    (capi.smtp-set-monitorcb)))

(define (smtp-start-session)
  (define who 'smtp-start-session)
  (with-arguments-validation (who)
      ()
    (capi.smtp-start-session)))

(define (smtp-message-transfer-status)
  (define who 'smtp-message-transfer-status)
  (with-arguments-validation (who)
      ()
    (capi.smtp-message-transfer-status)))

(define (smtp-reverse-path-status)
  (define who 'smtp-reverse-path-status)
  (with-arguments-validation (who)
      ()
    (capi.smtp-reverse-path-status)))

(define (smtp-message-reset-status)
  (define who 'smtp-message-reset-status)
  (with-arguments-validation (who)
      ()
    (capi.smtp-message-reset-status)))

(define (smtp-recipient-status)
  (define who 'smtp-recipient-status)
  (with-arguments-validation (who)
      ()
    (capi.smtp-recipient-status)))

(define (smtp-recipient-check-complete)
  (define who 'smtp-recipient-check-complete)
  (with-arguments-validation (who)
      ()
    (capi.smtp-recipient-check-complete)))

(define (smtp-recipient-reset-status)
  (define who 'smtp-recipient-reset-status)
  (with-arguments-validation (who)
      ()
    (capi.smtp-recipient-reset-status)))

(define (smtp-errno)
  (define who 'smtp-errno)
  (with-arguments-validation (who)
      ()
    (capi.smtp-errno)))

(define (smtp-strerror)
  (define who 'smtp-strerror)
  (with-arguments-validation (who)
      ()
    (capi.smtp-strerror)))

(define (smtp-set-application-data)
  (define who 'smtp-set-application-data)
  (with-arguments-validation (who)
      ()
    (capi.smtp-set-application-data)))

(define (smtp-get-application-data)
  (define who 'smtp-get-application-data)
  (with-arguments-validation (who)
      ()
    (capi.smtp-get-application-data)))

(define (smtp-message-set-application-data)
  (define who 'smtp-message-set-application-data)
  (with-arguments-validation (who)
      ()
    (capi.smtp-message-set-application-data)))

(define (smtp-message-get-application-data)
  (define who 'smtp-message-get-application-data)
  (with-arguments-validation (who)
      ()
    (capi.smtp-message-get-application-data)))

(define (smtp-recipient-set-application-data)
  (define who 'smtp-recipient-set-application-data)
  (with-arguments-validation (who)
      ()
    (capi.smtp-recipient-set-application-data)))

(define (smtp-recipient-get-application-data)
  (define who 'smtp-recipient-get-application-data)
  (with-arguments-validation (who)
      ()
    (capi.smtp-recipient-get-application-data)))

(define (smtp-option-require-all-recipients)
  (define who 'smtp-option-require-all-recipients)
  (with-arguments-validation (who)
      ()
    (capi.smtp-option-require-all-recipients)))

(define (smtp-auth-set-context)
  (define who 'smtp-auth-set-context)
  (with-arguments-validation (who)
      ()
    (capi.smtp-auth-set-context)))

(define (smtp-set-timeout)
  (define who 'smtp-set-timeout)
  (with-arguments-validation (who)
      ()
    (capi.smtp-set-timeout)))

(define (smtp-dsn-set-ret)
  (define who 'smtp-dsn-set-ret)
  (with-arguments-validation (who)
      ()
    (capi.smtp-dsn-set-ret)))

(define (smtp-dsn-set-envid)
  (define who 'smtp-dsn-set-envid)
  (with-arguments-validation (who)
      ()
    (capi.smtp-dsn-set-envid)))

(define (smtp-dsn-set-notify)
  (define who 'smtp-dsn-set-notify)
  (with-arguments-validation (who)
      ()
    (capi.smtp-dsn-set-notify)))

(define (smtp-dsn-set-orcpt)
  (define who 'smtp-dsn-set-orcpt)
  (with-arguments-validation (who)
      ()
    (capi.smtp-dsn-set-orcpt)))

(define (smtp-size-set-estimate)
  (define who 'smtp-size-set-estimate)
  (with-arguments-validation (who)
      ()
    (capi.smtp-size-set-estimate)))

(define (smtp-8bitmime-set-body)
  (define who 'smtp-8bitmime-set-body)
  (with-arguments-validation (who)
      ()
    (capi.smtp-8bitmime-set-body)))

(define (smtp-deliverby-set-mode)
  (define who 'smtp-deliverby-set-mode)
  (with-arguments-validation (who)
      ()
    (capi.smtp-deliverby-set-mode)))

(define (smtp-starttls-enable)
  (define who 'smtp-starttls-enable)
  (with-arguments-validation (who)
      ()
    (capi.smtp-starttls-enable)))

(define (smtp-starttls-set-ctx)
  (define who 'smtp-starttls-set-ctx)
  (with-arguments-validation (who)
      ()
    (capi.smtp-starttls-set-ctx)))

(define (smtp-starttls-set-password-cb)
  (define who 'smtp-starttls-set-password-cb)
  (with-arguments-validation (who)
      ()
    (capi.smtp-starttls-set-password-cb)))

(define (smtp-etrn-add-node)
  (define who 'smtp-etrn-add-node)
  (with-arguments-validation (who)
      ()
    (capi.smtp-etrn-add-node)))

(define (smtp-etrn-enumerate-nodes)
  (define who 'smtp-etrn-enumerate-nodes)
  (with-arguments-validation (who)
      ()
    (capi.smtp-etrn-enumerate-nodes)))

(define (smtp-etrn-node-status)
  (define who 'smtp-etrn-node-status)
  (with-arguments-validation (who)
      ()
    (capi.smtp-etrn-node-status)))

(define (smtp-etrn-set-application-data)
  (define who 'smtp-etrn-set-application-data)
  (with-arguments-validation (who)
      ()
    (capi.smtp-etrn-set-application-data)))

(define (smtp-etrn-get-application-data)
  (define who 'smtp-etrn-get-application-data)
  (with-arguments-validation (who)
      ()
    (capi.smtp-etrn-get-application-data)))


;;;; done

(set-rtd-printer!	(type-descriptor smtp-session) %struct-smtp-session-printer)
(set-rtd-destructor!	(type-descriptor smtp-session) %unsafe.smtp-destroy-session)

(set-rtd-printer!	(type-descriptor smtp-message) %struct-smtp-message-printer)
(set-rtd-destructor!	(type-descriptor smtp-message) %unsafe.smtp-message-close)

(set-rtd-printer!	(type-descriptor smtp-recipient) %struct-smtp-recipient-printer)
(set-rtd-destructor!	(type-descriptor smtp-recipient) %unsafe.smtp-recipient-close)

)

;;; end of file
