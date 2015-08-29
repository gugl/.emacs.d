(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/gtd.org"))
(setq org-agenda-files (quote ("~/org")))
(setq org-refile-targets '((org-agenda-files :level . 1)))
;; (define-key global-map "\C-cc" 'org-capture)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done 'time)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i")
        ("T" "Todo with store-link" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "\n* %?\nEntered on %U\n  %i")


        ;;("J" "Journal wit org-journal" plain (function org-journal-get-entry-path)
        ;; "** %?")


        ;;("J" "Journal with store-link" entry (file+datetree "~/org/journal.org")
        ;; "* %?\nEntered on %U\n  %i\n  %a")
        ))



;; org-journal
(setq org-journal-dir "~/org/journal/")
