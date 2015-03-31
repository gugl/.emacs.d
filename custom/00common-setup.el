;; Mac OS X keyboard mapping
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;; Navigate between windows using Alt-1, Alt-2, Shift-left, shift-up, shift-right
(windmove-default-keybindings)

;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)

(setq-default indent-tabs-mode nil)

;; ============================================================================
;; Git
;; ============================================================================

(require 'git-gutter)
(global-git-gutter-mode t)
(git-gutter:linum-setup)


;; ============================================================================
;; Neotree
;; ============================================================================

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "M-\\") 'neotree-toggle)


;; ============================================================================
;; Auto-complete
;; ============================================================================

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
	     "~/.emacs.d/.cask/24.4.1/elpa/auto-complete-20150322.813/dict")
(ac-config-default)
(setq ac-ignore-case nil)


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
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
;; Press Meta-p for fuzzy find in project
(global-set-key (kbd "M-p") 'projectile-find-file)
;; Press Meta-b for fuzzy switch buffer
(global-set-key (kbd "M-b") 'projectile-switch-to-buffer)
;; Press Meta-s to save the current buffer
(global-set-key (kbd "M-s") 'save-buffer)


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

(load-theme 'monokai t)
(global-linum-mode t)
(setq-default truncate-lines t)

(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format "%%%dd " w) line) 'face 'linum)))

(setq linum-format 'linum-format-func)
