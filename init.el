;; (require 'cask "~/.cask/cask.el")
(require 'cask "~/.emacs.d/.cask/24.5.1/elpa/cask-20151123.528/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/custom")

(load "00common-setup.el")
(load "01ruby.el")
(load "02org-mode.el")
(load "03elixir.el")
(load "04react.el")
(load "05email.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-battery-mode nil)
 '(global-rbenv-mode t)
 '(rbenv-installation-dir "/usr/local/opt/rbenv")
 '(rbenv-show-active-ruby-in-modeline nil)
 '(size-indication-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-active1 ((t (:inherit mode-line :background "OliveDrab3")))))
