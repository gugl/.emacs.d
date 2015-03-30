;; Mac OS X keyboard mapping
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;; Navigate between windows using Alt-1, Alt-2, Shift-left, shift-up, shift-right
(windmove-default-keybindings)

;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)

;; Enable line numbers
(global-linum-mode)

;; auto-complete config
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
	     "~/.emacs.d/.cask/24.4.1/elpa/auto-complete-20150322.813/dict")
(ac-config-default)
(setq ac-ignore-case nil)

;; smartparens config
(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))

;; projectile config
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
