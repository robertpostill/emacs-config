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

;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; winner mode for working with windows
(winner-mode 1)

;; desktops, so I can have a stack of buffers open at any one time
(desktop-save-mode 1)

;; ace-window, window movement made nice
(global-set-key (kbd "M-p") 'ace-window)

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


(load-theme 'solarized-dark t)


(server-start)
