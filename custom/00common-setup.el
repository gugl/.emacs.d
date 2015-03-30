;; Navigate between windows using Alt-1, Alt-2, Shift-left, shift-up, shift-right
(windmove-default-keybindings)

;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)


;; auto-complete config
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
	     "~/.emacs.d/.cask/24.4.1/elpa/auto-complete-20150322.813/dict")
(ac-config-default)
(setq ac-ignore-case nil)
