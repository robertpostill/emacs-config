(setq inhibit-startup-screen +1)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; ido mode - what would I do without it?
(setq ido-enable-flex-matching t)
(ido-mode +1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
; (ido-yes-or-no-mode +1)

;; Highlight parentheses.
(show-paren-mode 1)

;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; winner mode for working with windows
(winner-mode 1)

;; desktops, so I can have a stack of buffers open at any one time
(desktop-save-mode 1)

;; ace-window, window movement made nice
(global-set-key (kbd "M-p") 'ace-window)

;; get the path from the shell on OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; LISP hacking
;; Setup shamelessly ripped from 
;; http://zeekat.nl/articles/making-emacs-work-for-me.html
 (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "/usr/local/bin/sbcl")

(defun my/->string (str)
  (cond
   ((stringp str) str)
   ((symbolp str) (symbol-name str))))

(defun my/->mode-hook (name)
  "Turn mode name into hook symbol"
  (intern (replace-regexp-in-string "\\(-mode\\)?\\(-hook\\)?$"
                                    "-mode-hook"
                                    (my/->string name))))
(defun my/->mode (name)
  "Turn mode name into mode symbol"
  (intern (replace-regexp-in-string "\\(-mode\\)?$"
                                    "-mode"
                                    (my/->string name))))

(defun my/turn-on (&rest mode-list)
  "Turn on the given (minor) modes."
  (dolist (m mode-list)
    (funcall (my/->mode m) +1)))

(setq my/lisps
      '(emacs-lisp lisp clojure))

(defun my/general-lisp-hooks ()
  (my/turn-on 'paredit
              'rainbow-delimiters
              'highlight-parentheses))

(dolist (mode (mapcar 'my/->mode-hook my/lisps))
  (add-hook mode
            'my/general-lisp-hooks))

(dolist (exp '("Cask"))
  (add-to-list 'auto-mode-alist
               (cons exp 'emacs-lisp-mode)))

;; Ruby 
(dolist (exp '("Gemfile" "Rakefile\\'" "\\.rake\\'"))
  (add-to-list 'auto-mode-alist
               (cons exp 'ruby-mode)))

;; projectile and rails
(projectile-global-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(setq projectile-rails-expand-snippet nil)

;; yasnippets
(yas-global-mode 1)
(add-to-list 'yas/root-directory "~/.emacs.d/yasnippet-snippets")

;;OCaml
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist)) 
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)

(load-theme 'solarized-dark t)


(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(utop-command "opam config exec \"utop -emacs\""))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
