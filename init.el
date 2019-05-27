
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(org-babel-load-file "~/.emacs.d/configuration.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bb749a38c5cb7d13b60fa7fc40db7eced3d00aa93654d150b9627cabd2d9b361" default)))
 '(display-battery-mode nil)
 '(global-rbenv-mode t)
 '(org-stuck-projects
   (quote
    ("+LEVEL=2+PROJECT/-DONE"
     ("TODO" "NEXT" "NEXTACTION")
     nil "")))
 '(org-tags-exclude-from-inheritance (quote ("PROJECT")))
 '(package-selected-packages
   (quote
    (drag-stuff terraform-mode ledger-mode multiple-cursors chruby minitest wgrep rubocop haml-mode ruby-test-mode elm-mode treemacs-projectile treemacs eyebrowse all-the-icons-ivy all-the-icons-dired w3m hydra projectile elixir-yasnippets spaceline-all-the-icons spacemacs-theme spaceline flycheck-credo elixir-format elixir-mode flycheck-mix flycheck-dialyxir leuven-theme github-modern-theme calibre-mode undo-tree org2blog vlf php-mode markdown-mode ob-elixir dockerfile-mode ruby-end tidy git-timemachine avy counsel-osx-app counsel-projectile counsel ivy-hydra swiper cider use-package guide-key mu4e-maildirs-extension dashboard page-break-lines chess vimish-fold notmuch dumb-jump annotate calfw yaml-mode web-mode twittering-mode tabbar smartparens slim-mode scss-mode sass-mode robe rinari react-snippets rbenv rainbow-mode railscasts-theme projectile-rails pallet org-journal neotree monokai-theme magit-gitflow highlight-symbol highlight-indentation helm grizzl goto-gem flycheck feature-mode expand-region exec-path-from-shell engine-mode discover-my-major discover-js2-refactor diminish diff-hl darcula-theme color-theme-sanityinc-tomorrow coffee-mode bundler auto-complete alchemist ag)))
 '(rbenv-installation-dir "/usr/local/opt/rbenv")
 '(rbenv-show-active-ruby-in-modeline nil)
 '(size-indication-mode nil))

;;;;;;;;; changed on 1st july to try spaceline
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(powerline-active1 ((t (:inherit mode-line :background "OliveDrab3")))))

;;;;;;;;;; end of try

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
