;; Also look at the following files
;;
;; .mbsyncrc      (sync imap emails)
;; .msmtprc       (send mails)

; add the source shipped with mu to load-path
(add-to-list 'load-path (expand-file-name "/usr/local/Cellar/mu/0.9.16/share/emacs/site-lisp/mu/mu4e"))

; make sure emacs finds applications in /usr/local/bin
(setq exec-path (cons "/usr/local/bin" exec-path))

; require mu4e
(require 'mu4e)

; tell mu4e where my Maildir is
(setq mu4e-maildir "~/Maildir")
; tell mu4e how to sync email
(setq mu4e-get-mail-command "/usr/local/bin/mbsync -a")
; tell mu4e to use w3m for html rendering
(setq mu4e-html2text-command "/usr/local/bin/w3m -T text/html")

; taken from mu4e page to define bookmarks
(add-to-list 'mu4e-bookmarks
            '("size:5M..500M"       "Big messages"     ?b))

; mu4e requires to specify drafts, sent, and trash dirs
; a smarter configuration allows to select directories according to the account (see mu4e page)
; (setq mu4e-drafts-folder "/work/drafts")
; (setq mu4e-sent-folder "/work/sent")
; (setq mu4e-trash-folder "/work/trash")

(setq
  mu4e-sent-folder   "/gmail/sent"       ;; folder for sent messages
  mu4e-drafts-folder "/gmail/drafts"     ;; unfinished messages
  mu4e-trash-folder  "/gmail/trash"      ;; trashed messages
  mu4e-refile-folder "/gmail/archive")   ;; saved messages














; use msmtp
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/local/bin/msmtp")
; tell msmtp to choose the SMTP server according to the from field in the outgoing email
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)
