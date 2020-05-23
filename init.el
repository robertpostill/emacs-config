;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

;;; Commentary:
;; Robert Postill's Emacs config
;;
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;; Code:

(package-initialize)

(setq inhibit-startup-screen +1)
(global-visual-line-mode 1)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(pallet-mode t)

;; Ivy mode configuration
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq magit-completing-read-function 'ivy-completing-read)
(setq projectile-completion-system 'ivy)

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
(global-set-key (kbd "M-o") 'ace-window)

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
  (my/turn-on 'lispy
              'rainbow-delimiters
              'highlight-parentheses))

(dolist (mode (mapcar 'my/->mode-hook my/lisps))
  (add-hook mode
            'my/general-lisp-hooks)
  (setq indent-tab-mode nil))

(dolist (exp '("\\.lsp"))
  (add-to-list 'auto-mode-alist
               (cons exp 'lisp-mode)))


(dolist (exp '("Cask"))
  (add-to-list 'auto-mode-alist
               (cons exp 'emacs-lisp-mode)))

;; Javascript
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(setq js2-highlight-level 3)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(js2r-add-keybindings-with-prefix "C-c C-m")
;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("src\\/.*\\.js\\'" . rjsx-mode))

(add-hook 'rjsx-mode-hook 'prettier-js-mode)

;;;  JSdoc, optimaised for work
(setq js-doc-mail-address "robertp@ynomia.io"
      js-doc-author (format "Robert Postill <%s>" js-doc-mail-address)
      js-doc-url "ynomia.io"
      js-doc-license "Proprietary")

(add-hook 'js2-mode-hook
	  #'(lambda ()
	      (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
	      (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)

(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

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

;; Python
(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
   Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (infer-indentation-style)))

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

;; For jekyll
(require 'org)
(require 'org2jekyll)
(add-hook 'org-mode-hook 'turn-on-flyspell)


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
   (quote
    ("#3b6b40f432d6" "#07b9463c4d36" "#47a3341e358a" "#1d873c3f56d5" "#2d86441c3361" "#43b7362d3199" "#061d417f59d7")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#5b7300" . 20)
     ("#007d76" . 30)
     ("#0061a8" . 50)
     ("#866300" . 60)
     ("#992700" . 70)
     ("#a00559" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(js-indent-level 2)
 '(magit-commit-arguments nil)
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4")))
 '(org2jekyll-blog-author "robertpostill")
 '(org2jekyll-jekyll-directory (expand-file-name "~/software/robertpostill.github.io/"))
 '(org2jekyll-jekyll-drafts-dir "")
 '(org2jekyll-jekyll-posts-dir "_posts/")
 '(org2jekyll-source-directory
   (expand-file-name "~/software/robertpostill.github.io/org/"))

 '(org-publish-project-alist
   (\`
    (("default" :base-directory
      (\,
       (org2jekyll-input-directory))
      :base-extension "org" :publishing-directory
      (\,
       (org2jekyll-output-directory))
      :publishing-function org-html-publish-to-html :headline-levels 4 :section-numbers nil :with-toc nil :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>" :html-preamble t :recursive t :make-index t :html-extension "html" :body-only t)
     ("post" :base-directory
      (\,
       (org2jekyll-input-directory))
      :base-extension "org" :publishing-directory
      (\,
       (org2jekyll-output-directory org2jekyll-jekyll-posts-dir))
      :publishing-function org-html-publish-to-html :headline-levels 4 :section-numbers nil :with-toc nil :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>" :html-preamble t :recursive t :make-index t :html-extension "html" :body-only t)
     ("images" :base-directory
      (\,
       (org2jekyll-input-directory "img"))
      :base-extension "jpg\\|gif\\|png" :publishing-directory
      (\,
       (org2jekyll-output-directory "img"))
      :publishing-function org-publish-attachment :recursive t)
     ("js" :base-directory
      (\,
       (org2jekyll-input-directory "js"))
      :base-extension "js" :publishing-directory
      (\,
       (org2jekyll-output-directory "js"))
      :publishing-function org-publish-attachment :recursive t)
     ("css" :base-directory
      (\,
       (org2jekyll-input-directory "css"))
      :base-extension "css\\|el" :publishing-directory
      (\,
       (org2jekyll-output-directory "css"))
      :publishing-function org-publish-attachment :recursive t)
     ("web" :components
      ("images" "js" "css")))))
 '(package-selected-packages
   (quote
    (htmlize yasnippet-snippets yard-mode wrap-region which-key web-mode vlf vagrant-tramp vagrant utop typing typescript-mode tuareg toml-mode toml terraform-mode systemd solarized-theme smex scss-mode scribble-mode sass-mode rvm rust-mode ruby-tools ruby-refactor ruby-hash-syntax ruby-factory ruby-end ruby-electric ruby-compilation rubocop rspec-mode rjsx-mode reason-mode rainbow-delimiters racket-mode python-test pytest py-autopep8 pug-mode projectile-rails prettier-js popwin polymode pivotal-tracker paredit-menu paradox pallet ox-gfm org2jekyll org-pivotal nvm mqtt-mode midje-mode magithub magit-gitflow magit-gh-pulls lispy keychain-environment js2-highlight-vars js-doc ivy-yasnippet ivy-hydra indium ido-ubiquitous highlight-parentheses graphviz-dot-mode golint go-snippets go-projectile go-gopath gitignore-mode gitconfig-mode geiser flycheck-rust flycheck-ocaml flycheck-golangci-lint flycheck-cask feature-mode expand-region exec-path-from-shell ess-smart-underscore ess-R-object-popup ess-R-data-view erlang ensime emacsql-psql elpy elm-yasnippets elm-mode ecb drag-stuff dotenv-mode dockerfile-mode docker-compose-mode docker dash-at-point cucumber-goto-step csv-mode csv crontab-mode coverage counsel-tramp common-lisp-snippets coffee-mode clojure-snippets clj-refactor cider-decompile chef-mode buttercup bundler blacken ansible ag ac-slime ac-js2 ac-etags ac-cider 4clojure))))

'(package-selected-packages
  (quote
   (csv-mode csv blacken typescript-mode typing org-pivotal pivotal-tracker go-guru go-projectile go-rename golint yasnippet-snippets flycheck-golangci-lint go-gopath go-mode go-snippets js-doc py-autopep8 python-test elpy indium crontab-mode systemd pytest pug-mode prettier-js magit-gitflow dotenv-mode js2-highlight-vars js2-mode rjsx-mode nvm reason-mode counsel-tramp docker-compose-mode docker-tramp erlang lispy mqtt-mode ivy-hydra ivy-yasnippet docker dockerfile-mode toml toml-mode flycheck-rust rust-mode which-key keychain-environment polymode vagrant vagrant-tramp vlf terraform-mode geiser racket-mode scribble-mode magit magithub yard-mode yaml-mode wrap-region web-mode utop tuareg solarized-theme smex scss-mode sass-mode rvm ruby-tools ruby-refactor ruby-hash-syntax ruby-guard ruby-factory ruby-end ruby-electric ruby-compilation rubocop rspec-mode rainbow-delimiters projectile-rails popwin paredit-menu paradox pallet ox-gfm org2jekyll midje-mode markdown-mode magit-gh-pulls json-mode js2-refactor ido-ubiquitous highlight-parentheses graphviz-dot-mode gitignore-mode gitconfig-mode flycheck-ocaml flycheck-cask feature-mode expand-region exercism exec-path-from-shell ess-smart-underscore ess-R-object-popup ess-R-data-view ensime emacsql-psql elm-yasnippets elm-mode ecb drag-stuff dash-at-point cucumber-goto-step coverage counsel common-lisp-snippets coffee-mode clojurescript-mode clojure-snippets clojure-env cljdoc clj-refactor cider-spy cider-profile cider-decompile chef-mode buttercup bundler bookmark+ ansible ag ack-and-a-half ace-window ac-slime ac-nrepl ac-js2 ac-etags ac-cider ac-R 4clojure)))
'(pos-tip-background-color "#073642")
'(pos-tip-foreground-color "#93a1a1")
'(pug-tab-width 2)
'(show-paren-mode t)
'(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
'(term-default-bg-color "#002b36")
'(term-default-fg-color "#839496")
'(tool-bar-mode nil)
'(utop-command "opam config exec \"utop -emacs\"")
'(vc-annotate-background nil)
'(vc-annotate-color-map
  (quote
   ((20 . "#dc322f")
    (40 . "#cb4366eb20b4")
    (60 . "#c1167942154f")
    (80 . "#b58900")
    (100 . "#a6ae8f7c0000")
    (120 . "#9ed892380000")
    (140 . "#96be94cf0000")
    (160 . "#8e5397440000")
    (180 . "#859900")
    (200 . "#77679bfc4635")
    (220 . "#6d449d465bfd")
    (240 . "#5fc09ea47092")
    (260 . "#4c68a01784aa")
    (280 . "#2aa198")
    (300 . "#303498e7affc")
    (320 . "#2fa1947cbb9b")
    (340 . "#2c879008c736")
    (360 . "#268bd2"))))
'(vc-annotate-very-old-color nil)
'(weechat-color-list
  (quote
   (unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83")))
'(xterm-color-names
  ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
'(xterm-color-names-bright
  ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"])

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

;; Customised keybindings
;; Mac OS style font control
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(set-face-attribute 'default nil :font "Menlo-14" )

;; Set projectile to something that isn't the mac OS print command
(define-key projectile-mode-map (kbd "s-.") 'projectile-command-map)

(put 'upcase-region 'disabled nil)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; Go Support
(add-hook 'before-save-hook #'gofmt-before-save)

(provide 'init)

;;; init.el ends here
