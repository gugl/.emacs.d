(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(add-to-list 'load-path "~/.emacs.d/custom")

(load "00common-setup.el")
(load "01ruby.el")
(load "02org-mode.el")
(load "03elixir.el")

