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
;;;Copyright (C) 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare email libesmtp unsafe-capi)
  (export

    ;; version functions
    vicare-libesmtp-version-interface-current
    vicare-libesmtp-version-interface-revision
    vicare-libesmtp-version-interface-age
    vicare-libesmtp-version
    smtp-version

    ;; library errors
    smtp-errno
    smtp-strerror

    ;; session management
    smtp-create-session
    smtp-destroy-session
    smtp-set-hostname
    smtp-set-server
    smtp-set-timeout
    smtp-set-eventcb
    smtp-set-monitorcb
    smtp-start-session

    ;; message management
    smtp-add-message
    smtp-enumerate-messages
    smtp-set-reverse-path
    smtp-set-messagecb
    smtp-set-message-fp
    smtp-set-message-str
    smtp-message-transfer-status
    smtp-reverse-path-status
    smtp-message-reset-status

    ;; recipient management
    smtp-add-recipient
    smtp-enumerate-recipients
    smtp-option-require-all-recipients
    smtp-recipient-status
    smtp-recipient-check-complete
    smtp-recipient-reset-status

    ;; headers management
    smtp-set-header
    smtp-set-header-option
    smtp-set-resent-headers

    ;; application data
    smtp-set-application-data
    smtp-get-application-data
    smtp-message-set-application-data
    smtp-message-get-application-data
    smtp-recipient-set-application-data
    smtp-recipient-get-application-data

    ;; SMTP AUTH extension
    smtp-auth-set-context
    smtp-gsasl-set-context

    ;; SMTP StartTLS extension
    smtp-starttls-enable
    smtp-starttls-set-ctx
    smtp-starttls-set-password-cb

    ;; SMTP Deliver By extension
    smtp-deliverby-set-mode

    ;; SMTP Deliver Status Notification extension
    smtp-dsn-set-ret
    smtp-dsn-set-envid
    smtp-dsn-set-notify
    smtp-dsn-set-orcpt

    ;; SMTP Size extension
    smtp-size-set-estimate

    ;; SMTP 8bit-MIME Transport extension
    smtp-8bitmime-set-body

    ;; SMTP Remote Message Queue Starting (ETRN) extension
    smtp-etrn-add-node
    smtp-etrn-enumerate-nodes
    smtp-etrn-node-status
    smtp-etrn-set-application-data
    smtp-etrn-get-application-data

    ;; AUTH module
    auth-client-init
    auth-client-exit
    auth-create-context
    auth-destroy-context
    auth-set-mechanism-flags
    auth-set-mechanism-ssf
    auth-set-interact-cb
    auth-client-enabled
    auth-set-mechanism
    auth-mechanism-name
    auth-response
    auth-get-ssf
    auth-encode
    auth-decode
    auth-set-external-id)
  (import (vicare))


;;;; version functions

(define-syntax-rule (vicare-libesmtp-version-interface-current)
  (foreign-call "ikrt_libesmtp_version_interface_current"))

(define-syntax-rule (vicare-libesmtp-version-interface-revision)
  (foreign-call "ikrt_libesmtp_version_interface_revision"))

(define-syntax-rule (vicare-libesmtp-version-interface-age)
  (foreign-call "ikrt_libesmtp_version_interface_age"))

(define-syntax-rule (vicare-libesmtp-version)
  (foreign-call "ikrt_libesmtp_version"))

;;; --------------------------------------------------------------------

(define-syntax-rule (smtp-version)
  (foreign-call "ikrt_smtp_version"))


;;;; library errors

(define-syntax-rule (smtp-errno)
  (foreign-call "ikrt_smtp_errno"))

(define-syntax-rule (smtp-strerror code)
  (foreign-call "ikrt_smtp_strerror" code))


;;;; session management

(define-syntax-rule (smtp-create-session)
  (foreign-call "ikrt_smtp_create_session"))

(define-syntax-rule (smtp-destroy-session session)
  (foreign-call "ikrt_smtp_destroy_session" session))

(define-syntax-rule (smtp-start-session session)
  (foreign-call "ikrt_smtp_start_session" session))

;;; --------------------------------------------------------------------

(define-syntax-rule (smtp-set-hostname session local-hostname)
  (foreign-call "ikrt_smtp_set_hostname" session local-hostname))

(define-syntax-rule (smtp-set-server session remote-server)
  (foreign-call "ikrt_smtp_set_server" session remote-server))

(define-syntax-rule (smtp-set-timeout session which value)
  (foreign-call "ikrt_smtp_set_timeout" session which value))

(define-syntax-rule (smtp-set-eventcb session c-callback)
  (foreign-call "ikrt_smtp_set_eventcb" session c-callback))

(define-syntax-rule (smtp-set-monitorcb session c-callback headers)
  (foreign-call "ikrt_smtp_set_monitorcb" session c-callback headers))


;;;; message management

(define-syntax-rule (smtp-add-message session)
  (foreign-call "ikrt_smtp_add_message" session))

(define-syntax-rule (smtp-enumerate-messages session callback)
  (foreign-call "ikrt_smtp_enumerate_messages" session callback))

;;; --------------------------------------------------------------------

(define-syntax-rule (smtp-set-reverse-path message mailbox)
  (foreign-call "ikrt_smtp_set_reverse_path" message mailbox))

;;; --------------------------------------------------------------------

(define-syntax-rule (smtp-set-messagecb message c-callback)
  (foreign-call "ikrt_smtp_set_messagecb" message c-callback))

(define-syntax-rule (smtp-set-message-fp message file-pointer)
  (foreign-call "ikrt_smtp_set_message_fp" message file-pointer))

(define-syntax-rule (smtp-set-message-str message string)
  (foreign-call "ikrt_smtp_set_message_str" message string))

;;; --------------------------------------------------------------------

(define-syntax-rule (smtp-message-transfer-status message status-struct)
  (foreign-call "ikrt_smtp_message_transfer_status" message status-struct))

(define-syntax-rule (smtp-reverse-path-status message status-struct)
  (foreign-call "ikrt_smtp_reverse_path_status" message status-struct))

(define-syntax-rule (smtp-message-reset-status message)
  (foreign-call "ikrt_smtp_message_reset_status" message))


;;;; recipient management

(define-syntax-rule (smtp-add-recipient message mailbox)
  (foreign-call "ikrt_smtp_add_recipient" message mailbox))

(define-syntax-rule (smtp-enumerate-recipients message callback)
  (foreign-call "ikrt_smtp_enumerate_recipients" message callback))

(define-syntax-rule (smtp-option-require-all-recipients session state)
  (foreign-call "ikrt_smtp_option_require_all_recipients" session state))

(define-syntax-rule (smtp-recipient-status recipient status-struct)
  (foreign-call "ikrt_smtp_recipient_status" recipient status-struct))

(define-syntax-rule (smtp-recipient-check-complete recipient)
  (foreign-call "ikrt_smtp_recipient_check_complete" recipient))

(define-syntax-rule (smtp-recipient-reset-status recipient)
  (foreign-call "ikrt_smtp_recipient_reset_status" recipient))


;;;; headers management

(define-syntax-rule (smtp-set-header message header value1 value2)
  (foreign-call "ikrt_smtp_set_header" message header value1 value2))

(define-syntax-rule (smtp-set-header-option message header option)
  (foreign-call "ikrt_smtp_set_header_option" message header option))

(define-syntax-rule (smtp-set-resent-headers message onoff)
  (foreign-call "ikrt_smtp_set_resent_headers" message onoff))


;;;; application data

(define-syntax-rule (smtp-set-application-data session data-pointer)
  (foreign-call "ikrt_smtp_set_application_data" session data-pointer))

(define-syntax-rule (smtp-get-application-data session)
  (foreign-call "ikrt_smtp_get_application_data" session))

;;; --------------------------------------------------------------------

(define-syntax-rule (smtp-message-set-application-data message data-pointer)
  (foreign-call "ikrt_smtp_message_set_application_data" message data-pointer))

(define-syntax-rule (smtp-message-get-application-data message)
  (foreign-call "ikrt_smtp_message_get_application_data" message))

;;; --------------------------------------------------------------------

(define-syntax-rule (smtp-recipient-set-application-data recipient data-pointer)
  (foreign-call "ikrt_smtp_recipient_set_application_data" recipient data-pointer))

(define-syntax-rule (smtp-recipient-get-application-data recipient)
  (foreign-call "ikrt_smtp_recipient_get_application_data" recipient))


;;;; SMTP Authentication extension

(define-syntax-rule (smtp-auth-set-context session auth-context)
  (foreign-call "ikrt_smtp_auth_set_context" session auth-context))

(define-syntax-rule (smtp-gsasl-set-context session auth-context)
  (foreign-call "ikrt_smtp_gsasl_set_context" session auth-context))


;;;; SMTP StartTLS extension

(define-syntax-rule (smtp-starttls-enable session how)
  (foreign-call "ikrt_smtp_starttls_enable" session how))

(define-syntax-rule (smtp-starttls-set-ctx session ssl-context)
  (foreign-call "ikrt_smtp_starttls_set_ctx" session ssl-context))

(define-syntax-rule (smtp-starttls-set-password-cb c-callback)
  (foreign-call "ikrt_smtp_starttls_set_password_cb" c-callback))


;;;; SMTP Deliver By extension

(define-syntax-rule (smtp-deliverby-set-mode message time by-mode trace)
  (foreign-call "ikrt_smtp_deliverby_set_mode" message time by-mode trace))


;;;; SMTP Deliver Status Notification extension

(define-syntax-rule (smtp-dsn-set-ret message flags)
  (foreign-call "ikrt_smtp_dsn_set_ret" message flags))

(define-syntax-rule (smtp-dsn-set-envid message envid)
  (foreign-call "ikrt_smtp_dsn_set_envid" message envid))

(define-syntax-rule (smtp-dsn-set-notify recipient flags)
  (foreign-call "ikrt_smtp_dsn_set_notify" recipient flags))

(define-syntax-rule (smtp-dsn-set-orcpt recipient address-type address)
  (foreign-call "ikrt_smtp_dsn_set_orcpt" recipient address-type address))


;;;; SMTP Size extension

(define-syntax-rule (smtp-size-set-estimate message size)
  (foreign-call "ikrt_smtp_size_set_estimate" message size))


;; SMTP 8bit-MIME Transport extension

(define-syntax-rule (smtp-8bitmime-set-body message body)
  (foreign-call "ikrt_smtp_8bitmime_set_body" message body))


;;;; SMTP Remote Message Queue Starting (ETRN) extension

(define-syntax-rule (smtp-etrn-add-node session option node)
  (foreign-call "ikrt_smtp_etrn_add_node" session option node))

(define-syntax-rule (smtp-etrn-enumerate-nodes session callback)
  (foreign-call "ikrt_smtp_etrn_enumerate_nodes" session callback))

(define-syntax-rule (smtp-etrn-node-status etrn-node status)
  (foreign-call "ikrt_smtp_etrn_node_status" etrn-node status))

(define-syntax-rule (smtp-etrn-set-application-data etrn-node data-pointer)
  (foreign-call "ikrt_smtp_etrn_set_application_data" etrn-node data-pointer))

(define-syntax-rule (smtp-etrn-get-application-data etrn-node)
  (foreign-call "ikrt_smtp_etrn_get_application_data" etrn-node))


;;;; AUTH module

(define-syntax-rule (auth-client-init)
  (foreign-call "auth_client_init"))

(define-syntax-rule (auth-client-exit)
  (foreign-call "auth_client_exit"))

(define-syntax-rule (auth-create-context)
  (foreign-call "auth_create_context"))

(define-syntax-rule (auth-destroy-context auth-ctx)
  (foreign-call "auth_destroy_context" auth-ctx))

(define-syntax-rule (auth-set-mechanism-flags)
  (foreign-call "auth_set_mechanism_flags"))

(define-syntax-rule (auth-set-mechanism-ssf)
  (foreign-call "auth_set_mechanism_ssf"))

(define-syntax-rule (auth-set-interact-cb)
  (foreign-call "auth_set_interact_cb"))

(define-syntax-rule (auth-client-enabled)
  (foreign-call "auth_client_enabled"))

(define-syntax-rule (auth-set-mechanism)
  (foreign-call "auth_set_mechanism"))

(define-syntax-rule (auth-mechanism-name)
  (foreign-call "auth_mechanism_name"))

(define-syntax-rule (auth-response)
  (foreign-call "auth_response"))

(define-syntax-rule (auth-get-ssf)
  (foreign-call "auth_get_ssf"))

(define-syntax-rule (auth-encode)
  (foreign-call "auth_encode"))

(define-syntax-rule (auth-decode)
  (foreign-call "auth_decode"))

(define-syntax-rule (auth-set-external-id)
  (foreign-call "auth_set_external_id"))


;;;; done

)

;;; end of file
