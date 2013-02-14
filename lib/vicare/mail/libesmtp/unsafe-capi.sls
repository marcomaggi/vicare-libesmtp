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

    smtp-version
    smtp-create-session
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
    smtp-destroy-session
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

(define-inline (smtp-version)
  (foreign-call "ikrt_smtp_version"))

(define-inline (smtp-create-session)
  (foreign-call "ikrt_smtp_create_session"))

(define-inline (smtp-add-message)
  (foreign-call "ikrt_smtp_add_message"))

(define-inline (smtp-enumerate-messages)
  (foreign-call "ikrt_smtp_enumerate_messages"))

(define-inline (smtp-set-server)
  (foreign-call "ikrt_smtp_set_server"))

(define-inline (smtp-set-hostname)
  (foreign-call "ikrt_smtp_set_hostname"))

(define-inline (smtp-set-reverse-path)
  (foreign-call "ikrt_smtp_set_reverse_path"))

(define-inline (smtp-add-recipient)
  (foreign-call "ikrt_smtp_add_recipient"))

(define-inline (smtp-enumerate-recipients)
  (foreign-call "ikrt_smtp_enumerate_recipients"))

(define-inline (smtp-set-header)
  (foreign-call "ikrt_smtp_set_header"))

(define-inline (smtp-set-header-option)
  (foreign-call "ikrt_smtp_set_header_option"))

(define-inline (smtp-set-resent-headers)
  (foreign-call "ikrt_smtp_set_resent_headers"))

(define-inline (smtp-set-messagecb)
  (foreign-call "ikrt_smtp_set_messagecb"))

(define-inline (smtp-set-eventcb)
  (foreign-call "ikrt_smtp_set_eventcb"))

(define-inline (smtp-set-monitorcb)
  (foreign-call "ikrt_smtp_set_monitorcb"))

(define-inline (smtp-start-session)
  (foreign-call "ikrt_smtp_start_session"))

(define-inline (smtp-destroy-session)
  (foreign-call "ikrt_smtp_destroy_session"))

(define-inline (smtp-message-transfer-status)
  (foreign-call "ikrt_smtp_message_transfer_status"))

(define-inline (smtp-reverse-path-status)
  (foreign-call "ikrt_smtp_reverse_path_status"))

(define-inline (smtp-message-reset-status)
  (foreign-call "ikrt_smtp_message_reset_status"))

(define-inline (smtp-recipient-status)
  (foreign-call "ikrt_smtp_recipient_status"))

(define-inline (smtp-recipient-check-complete)
  (foreign-call "ikrt_smtp_recipient_check_complete"))

(define-inline (smtp-recipient-reset-status)
  (foreign-call "ikrt_smtp_recipient_reset_status"))

(define-inline (smtp-errno)
  (foreign-call "ikrt_smtp_errno"))

(define-inline (smtp-strerror)
  (foreign-call "ikrt_smtp_strerror"))

(define-inline (smtp-set-application-data)
  (foreign-call "ikrt_smtp_set_application_data"))

(define-inline (smtp-get-application-data)
  (foreign-call "ikrt_smtp_get_application_data"))

(define-inline (smtp-message-set-application-data)
  (foreign-call "ikrt_smtp_message_set_application_data"))

(define-inline (smtp-message-get-application-data)
  (foreign-call "ikrt_smtp_message_get_application_data"))

(define-inline (smtp-recipient-set-application-data)
  (foreign-call "ikrt_smtp_recipient_set_application_data"))

(define-inline (smtp-recipient-get-application-data)
  (foreign-call "ikrt_smtp_recipient_get_application_data"))

(define-inline (smtp-option-require-all-recipients)
  (foreign-call "ikrt_smtp_option_require_all_recipients"))

(define-inline (smtp-auth-set-context)
  (foreign-call "ikrt_smtp_auth_set_context"))

(define-inline (smtp-set-timeout)
  (foreign-call "ikrt_smtp_set_timeout"))

(define-inline (smtp-dsn-set-ret)
  (foreign-call "ikrt_smtp_dsn_set_ret"))

(define-inline (smtp-dsn-set-envid)
  (foreign-call "ikrt_smtp_dsn_set_envid"))

(define-inline (smtp-dsn-set-notify)
  (foreign-call "ikrt_smtp_dsn_set_notify"))

(define-inline (smtp-dsn-set-orcpt)
  (foreign-call "ikrt_smtp_dsn_set_orcpt"))

(define-inline (smtp-size-set-estimate)
  (foreign-call "ikrt_smtp_size_set_estimate"))

(define-inline (smtp-8bitmime-set-body)
  (foreign-call "ikrt_smtp_8bitmime_set_body"))

(define-inline (smtp-deliverby-set-mode)
  (foreign-call "ikrt_smtp_deliverby_set_mode"))

(define-inline (smtp-starttls-enable)
  (foreign-call "ikrt_smtp_starttls_enable"))

(define-inline (smtp-starttls-set-ctx)
  (foreign-call "ikrt_smtp_starttls_set_ctx"))

(define-inline (smtp-starttls-set-password-cb)
  (foreign-call "ikrt_smtp_starttls_set_password_cb"))

(define-inline (smtp-etrn-add-node)
  (foreign-call "ikrt_smtp_etrn_add_node"))

(define-inline (smtp-etrn-enumerate-nodes)
  (foreign-call "ikrt_smtp_etrn_enumerate_nodes"))

(define-inline (smtp-etrn-node-status)
  (foreign-call "ikrt_smtp_etrn_node_status"))

(define-inline (smtp-etrn-set-application-data)
  (foreign-call "ikrt_smtp_etrn_set_application_data"))

(define-inline (smtp-etrn-get-application-data)
  (foreign-call "ikrt_smtp_etrn_get_application_data"))


;;;; done

)

;;; end of file
