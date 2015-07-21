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
(ido-yes-or-no-mode 1)
(ido-ubiquitous-mode 1)

;; Highlight parentheses.
(show-paren-mode 1)

;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; winner mode for working with windows
(winner-mode 1)

;; desktops, so I can have a stack of buffers open at any one time
(desktop-save-mode 1)

;; Paradox github access
(setq paradox-github-token "d51edf216d388880997772ff917fcb225ce8cc8c")

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
;; Gimme most of slime's extras
(setq slime-contribs '(slime-fancy)) 

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

;;; get yasnippets support in rspec-mode
(eval-after-load 'rspec-mode
 '(rspec-install-snippets))


;;OCaml

;;; Setup environment variables using opam
(dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
  (setenv (car var) (cadr var)))

;;; Update the emacs path
(setq exec-path (append (parse-colon-path (getenv "PATH"))
                        (list exec-directory)))

;;; Update the emacs load path
(add-to-list 'load-path (expand-file-name "../../share/emacs/site-lisp"
                                          (getenv "OCAML_TOPLEVEL_PATH")))

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

;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
;;(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; Load merlin-mode
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

;; So you can do it on a mac, where `C-<up>` and `C-<down>` are used
;; by spaces.
(define-key merlin-mode-map
  (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
(define-key merlin-mode-map
  (kbd "C-c <down>") 'merlin-type-enclosing-go-down)
; (set-face-background 'merlin-type-face "#88FF44")

;; ocp-indent 
(setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
;(load-file (concat opam-share "/typerex/ocp-indent/ocp-indent.el"))

;; Look and feel
;;; Theme
(load-theme 'solarized-dark t)

(tool-bar-mode -1)
(menu-bar-mode -1)


(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(utop-command "opam config exec \"utop -emacs\""))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Magit
;; Calm down magit, I have read your instructions :) 
(setq magit-last-seen-setup-instructions "1.4.0")

(global-set-key (kbd "C-x g") 'magit-status)

;; For jekyll
(setq org-publish-project-alist
    '(("com-grinning-cat-posts"
       ;; Path to your org files.
       :base-directory "~/software/robertpostill.github.io/org"
       :base-extension "org"

       ;; Path to your Jekyll project.
       :publishing-directory "~/software/robertpostill.github.io/_posts"
       :recursive t
       :publishing-function org-html-publish-to-html
       :headline-levels 4 
       :html-extension "html"
       :body-only t ;; Only export section between <body> </body>
       :table-of-contents nil)

      ("com-grinning-cat-static"
       :base-directory "~/software/robertpostill.github.io/org" 
       :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg"
       :publishing-directory "~/software/robertpostill.github.io"
       :recursive t
       :publishing-function org-publish-attachment
       :table-of-contents nil)

      ("grinning-cat" :components ("com-grinning-cat-posts" "com-grinning-cat-static"))))
