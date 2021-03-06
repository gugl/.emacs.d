(setq enh-ruby-program "/usr/local/opt/rbenv/shims/ruby")
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

;; Setting rbenv path
(setq rbenv-installation-dir "/usr/local/opt/rbenv")
(require 'rbenv)
(global-rbenv-mode)

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
 
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
 
(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)
 
(require 'cl) ; If you don't have it already
 
(defun* get-closest-gemfile-root (&optional (file "Gemfile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (loop 
     for d = default-directory then (expand-file-name ".." d)
     if (file-exists-p (expand-file-name file d))
     return d
     if (equal d root)
     return nil)))
 
(require 'compile)

(setq compilation-scroll-output "first-error")

(defun rspec-compile-all ()
  (interactive)
  (compile (format (concat "cd " (get-closest-gemfile-root) ";bundle exec rspec")) t))

(defun rspec-compile-file ()
  (interactive)
  ()
  (save-buffer)
  (compile (format "cd %s;bundle exec rspec %s"
                   (get-closest-gemfile-root)
                   (file-relative-name (buffer-file-name) (get-closest-gemfile-root))
                   ) t))

(defun rspec-compile-on-line ()
  (interactive)
  (compile (format "cd %s;rspec %s -l %s"
                   (get-closest-gemfile-root)
                   (file-relative-name (buffer-file-name) (get-closest-gemfile-root))
                   (line-number-at-pos)
                   ) t))
 
(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-s-r") 'rspec-compile-on-line)
            (local-set-key (kbd "C-c r") 'rspec-compile-on-line)
            (local-set-key (kbd "s-r") 'rspec-compile-file)
            ))

(add-hook 'projectile-mode-hook 'projectile-rails-on)

; (define-key projectile-rails-mode-map (kbd "s-RET") 'projectile-rails-goto-file-at-point)
