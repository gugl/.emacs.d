;; Mac OS X keyboard mapping
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'meta)
;; (setq mac-command-modifier 'super)

;; Navigate between windows using Alt-1, Alt-2, Shift-left, shift-up, shift-right
(windmove-default-keybindings)

;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)

;; Tab size
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-with)

;; Copy shell evn variables to the emacs environment (fixes some issues with Emacs.app on Mac OS X)
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))


;; ============================================================================
;; Git
;; ============================================================================

;; (require 'git-gutter)
;; (global-git-gutter-mode t)
;; (git-gutter:linum-setup)


;; ============================================================================
;; Neotree
;; ============================================================================

(require 'neotree)
;; (global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "M-\\") 'neotree-toggle)


;; ============================================================================
;; Auto-complete
;; ============================================================================

;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories
;;	     "~/.emacs.d/.cask/24.4.1/elpa/auto-complete-20150322.813/dict")
;; (ac-config-default)
;;(setq ac-ignore-case nil)

(global-company-mode t)
;; (add-hook 'after-init-hook 'global-company-mode)

(setq company-tooltip-limit 12)                      ; bigger popup window
(setq company-idle-delay .1)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(setq company-dabbrev-downcase nil)                  ; Do not convert to lowercase
(setq company-selection-wrap-around t)               ; continue from top when reaching bottom

(require 'helm-config)

;; ============================================================================
;; Smartparens
;; ============================================================================

(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))


;; ============================================================================
;; Projectile config
;; ============================================================================

(require 'grizzl)
(projectile-global-mode)
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
;; (global-set-key (kbd "s-p") 'projectile-find-file)
;; (global-set-key (kbd "s-t") 'projectile-find-file)
;; Press Meta-p for fuzzy find in project
(global-set-key (kbd "C-c t") 'projectile-find-file)
;; Press Meta-b for fuzzy switch buffer
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)
;; Press Meta-s to save the current buffer
;; (global-set-key (kbd "s-s") 'save-buffer)


;; ============================================================================
;; Cucumber Feature mode
;; ============================================================================

(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(add-to-list 'auto-mode-alist '("\\.feature$" . feature--mode))
(add-hook 'feature-mode-hook
          (lambda ()
            (local-set-key (kbd "M-r") 'feature-verify-scenario-at-pos)
            ))


;; ============================================================================
;; Auto Save on Focus loss (Gnu Emacs >= 24.4)
;; ============================================================================

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)


;; =============================================================================
;; UI
;; =============================================================================

;; (load-theme 'monokai t)

;; Darkula theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-darkula")
;; (load-theme 'Darkula t)

;; (require 'color-theme-sanityinc-tomorrow)

(load-theme 'railscasts t nil)

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/railscasts-theme")
;; (load-theme 'railscasts t nil)

;; (require 'darcula-theme)
(set-frame-font "Menlo-12")

(global-linum-mode t)
;; (setq-default truncate-lines t)

(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format "%%%dd " w) line) 'face 'linum)))

(setq linum-format 'linum-format-func)

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)
