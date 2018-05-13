
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-startup-screen +1)

(require 'cask)
(cask-initialize)
(pallet-mode t)

;; ido mode - what would I do without it?
(setq ido-enable-flex-matching t)
(ido-mode +1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(ido-yes-or-no-mode 1)
(ido-ubiquitous-mode 1)

;; Midnight mode for clearing old buffers
(require 'midnight)
(midnight-delay-set 'midnight-delay 0)

;; yasnippets
(yas-global-mode 1)
(add-to-list 'yas/root-directory "~/.emacs.d/yasnippet-snippets")

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

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(setq js2-highlight-level 3)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(js2r-add-keybindings-with-prefix "C-c C-m")
;; use web-mode for .jsx files
;;(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;     '(javascript-jshint)))

;; use eslint with web-mode for jsx files
;; (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)

;; customize flycheck temp file prefix
;; (setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
;; (setq-default flycheck-disabled-checkers
;; 	      (append flycheck-disabled-checkers
;; 		      '(json-jsonlist)))

;; Ruby
(dolist (exp '("Gemfile" "Rakefile\\'" "\\.rake\\'"))
  (add-to-list 'auto-mode-alist
               (cons exp 'ruby-mode)))

;; projectile and rails
(projectile-global-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(setq projectile-rails-expand-snippet nil)
(rvm-use-default)

;;; get yasnippets support in rspec-mode
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(add-hook 'ruby-mode-hook
	  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'ruby-mode-hook 'rubocop-mode)

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

;; get some help with keybindings
(which-key-mode)

;; make ssh-agent a thing
(keychain-refresh-environment)

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
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(js-indent-level 2)
 '(magit-commit-arguments nil)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (toml toml-mode flycheck-rust rust-mode which-key keychain-environment polymode vagrant vagrant-tramp vlf terraform-mode geiser racket-mode scribble-mode magit magithub yard-mode yaml-mode wrap-region web-mode utop tuareg solarized-theme smex scss-mode sass-mode rvm ruby-tools ruby-refactor ruby-hash-syntax ruby-guard ruby-factory ruby-end ruby-electric ruby-compilation rubocop rspec-mode rainbow-delimiters projectile-rails popwin paredit-menu paradox pallet ox-gfm org2jekyll midje-mode markdown-mode magit-gh-pulls json-mode js2-refactor ido-yes-or-no ido-ubiquitous highlight-parentheses graphviz-dot-mode gitignore-mode gitconfig-mode flycheck-ocaml flycheck-cask flx-ido feature-mode expand-region exercism exec-path-from-shell ess-smart-underscore ess-R-object-popup ess-R-data-view ensime emacsql-psql elm-yasnippets elm-mode ecb drag-stuff dash-at-point cucumber-goto-step coverage counsel common-lisp-snippets coffee-mode clojurescript-mode clojure-snippets clojure-env cljdoc clj-refactor cider-spy cider-profile cider-decompile chef-mode buttercup bundler bookmark+ ansible ag ack-and-a-half ace-window ac-slime ac-nrepl ac-js2 ac-etags ac-cider ac-R 4clojure)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(utop-command "opam config exec \"utop -emacs\"")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
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

;; Customised keybindings
;; Mac OS style font control
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

(put 'upcase-region 'disabled nil)

