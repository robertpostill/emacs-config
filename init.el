;;; init.el --- Initialisation file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs Startup File --- initialisation for Emacs
;; Robert Postill's Emacs config
;;

;;; Code:
;; things that don't require packages
(setq-default default-directory "~") ; don't make me traipse all the way from / :)
(setq inhibit-startup-screen +1) ; straight to a buffer
(global-visual-line-mode 1) ; word wrapping
(show-paren-mode 1) ; highlight parentheses
(winner-mode 1) ; working with windows
(desktop-save-mode 1) ; so I can have a stack of buffers open at any one time
(tool-bar-mode -1)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(set-face-attribute 'default nil :font "Menlo-14" )
(put 'downcase-region 'disabled nil) ; yes I really would like to downcase things
(put 'upcase-region 'disabled nil) ; yes I really want to upcase regions.
(setq-default indent-tabs-mode nil) ; don't use tabs unless your major mode demands it
(fset 'yes-or-no-p 'y-or-n-p) ;; Use y/n instead of yes/no
;; suppress the error message when using dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))
;; custom code used by packages
;;; I pinched this straight from https://github.com/jwiegley/dot-emacs/blob/master/init.el
(eval-and-compile
  (defun emacs-path (path)
    (expand-file-name path user-emacs-directory)))

;; setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)
(use-package el-patch
  :straight t)

;; load the shell context on *nixes
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :straight t
  :config
  (exec-path-from-shell-initialize))

;; solarized dark theme
(use-package solarized-theme
	     :straight t
	     :config
	     (load-theme 'solarized-dark t))

;; Yasnippet snippets
(use-package yasnippet
  :straight t
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (yas-load-directory (emacs-path "snippets"))
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :straight t
  :after (yasnippet))

;; Midnight mode for clearing old buffers
(use-package midnight
  :straight t
  :config
  (midnight-delay-set 'midnight-delay 0))

;; window movement made nice
(use-package ace-window
  :straight t
  :bind
  ("C-\"" . 'ace-window))

;; subtrees, visualised
(use-package dired-subtree
  :straight t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))
; better region selection
(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

;; the best way to git
(use-package magit
	     :straight t
	     :init
	     (setq magit-last-seen-setup-instructions "1.4.0")
	     :bind
	     ("C-x g" . 'magit-status)
             :hook
             (('git-commit-setup . 'git-commit-turn-on-flyspell)))
;; (use-package forge
;;   :straight t
;;   :after magit)

;; get some help with keybindings
(use-package which-key
	     :straight t
	     :config
	     (which-key-mode))

;; make ssh-agent a thing
(use-package keychain-environment
	     :straight t
	     :config
	     (keychain-refresh-environment))

;; CSS indent
(setq-default css-indent-offset 2)

(use-package ag
  :straight t)

(use-package free-keys
  :straight t)

(use-package flx
  :straight t)

;; ivy and friends
(use-package ivy
  :straight t
  :init
  :config
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (counsel-find-file . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  (("\C-s" . 'swiper)
   ("C-c C-r" . 'ivy-resume)
   ("<f6>" .'ivy-resume)
   ("M-x" . 'counsel-M-x)
   ("C-c g" . 'counsel-git)
   ("C-c j" . 'counsel-git-grep)
   ("C-c k" . 'counsel-ag)
   ("C-x l" .'counsel-locate)))

(use-package swiper
  :straight t
  :after ivy
  :bind ("C-M-s" . swiper)
  :bind (:map swiper-map
              ("M-y" . yank)
              ("M-%" . swiper-query-replace)
              ("C-." . swiper-avy)
              ("M-c" . swiper-mc))
  :bind (:map isearch-mode-map
              ("C-o" . swiper-from-isearch)))

(use-package counsel
  :straight t
  :after ivy
  :demand t
  :diminish
  :custom (counsel-find-file-ignore-regexp
           (concat "\\(\\`\\.[^.]\\|"
                   (regexp-opt completion-ignored-extensions)
                   "\\'\\)"))
  :bind (("C-*"     . counsel-org-agenda-headlines)
         ("C-x C-f" . counsel-find-file)
         ("C-c e l" . counsel-find-library)
         ("C-c e q" . counsel-set-variable)
         ("C-h f"   . counsel-describe-function)
         ("C-x r b" . counsel-bookmark)
         ("M-x"     . counsel-M-x)
         ;; ("M-y"     . counsel-yank-pop)

         ("M-s f" . counsel-file-jump)
         ;; ("M-s g" . counsel-rg)
         ("M-s j" . counsel-dired-jump))
  :commands counsel-minibuffer-history
  :init
  (bind-key "C-r" #'counsel-minibuffer-history minibuffer-local-map))

(use-package emacsql-sqlite3
  :straight t)

(use-package tree-sitter
  :straight t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package eglot
  :straight t
  :config
  (add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))

;; Org usage
(use-package org
  :straight t
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))
  :hook
  (('org-mode-hook . 'turn-on-flyspell)))

(setq org-roam-database-connector 'sqlite3)
(setq org-directory (concat (getenv "HOME") "/Dropbox/org-roam/"))
(use-package org-roam
  :straight t
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n r" . org-roam-node-random)		    
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))
(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; terraform development
(use-package terraform-mode
  :straight t
  :hook (terraform-mode . terraform-format-on-save-mode))

;; docker
(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile[a-zA-Z.-]*\\'")

;; .env files
(use-package dotenv-mode
  :straight t
  :mode "\\.env\\..*\\'")


;; YAML becuase yuck
(use-package yaml-mode
  :straight t
  :mode "\\.ya?ml\\'")

;; Ruby/Rails
(use-package haml-mode
  :straight t)

;; Javascript
(setq js-indent-level 2)
(use-package js-mode
  :hook ((js-mode . 'js2-minor-mode)))
(use-package js2-mode
  :straight t)

;; Typescript
(use-package typescript-mode
  :after tree-sitter
  :straight t
  :mode "\\.ts\\'"
  :config (setq-default typescript-indent-level 2))

(use-package prettier-js
  :straight t
  ;:hook ((js-mode . 'prettier-js-mode))
  )
;;(use-package tree-sitter-jsdoc
;;  :straight (:host github :repo "tree-sitter/tree-sitter-jsdoc"))
;;(use-package jsdoc
;;  :straight (:host github :repo "isamert/jsdoc.el"))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(use-package company
  :straight t
  :init (global-company-mode)
  :hook ((graphviz-dot-mode . company-mode)))

(use-package tide
  :straight t
  :init (setq-default typescript-indent-level 2)
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (typescript-mode . prettier-js-mode)
	 (before-save . tide-format-before-save)))


;; Use web-mode for tsx files
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package web-mode
  :straight t
  :init (setq web-mode-markup-indent-offset 2)
  :init (setq web-mode-code-indent-offset 2))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(use-package tsi
  :after tree-sitter
  :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; ReasonML
(use-package reason-mode
  :straight t)
(use-package merlin
  :straight t)

;; Markdown
(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Projectile
(use-package projectile
  :straight t
  :config (projectile-mode +1)
          (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
	  (setq projectile-project-search-path '("~/software/")))
;; org notes on projects
(use-package org-project-capture
  :bind (("C-c n p" . org-project-capture-project-todo-completing-read))
  :straight (:host github :repo "colonelpanic8/org-project-capture")
  :config
  (progn
    (setq org-project-capture-backend
          (make-instance 'org-project-capture-projectile-backend))  ; Replace with your backend of choice
    (setq org-project-capture-projects-file "~/org/projects.org")
    (org-project-capture-single-file)))
  
;; LISP editing
(use-package lispy-mnemonic
  :straight t)
(use-package lispy
  :straight t
  :hook (lispy-mode . lispy-mnemonic-mode))

;; Racket
(use-package racket-mode
  :straight t
  :hook ((racket-mode . racket-xp-mode)
         (racket-mode . (lambda () (lispy-mode 1)))))
(use-package scribble-mode
  :straight t)

;; drawing with dot
(use-package graphviz-dot-mode
  :straight t
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package asdf
  :straight (:host github :repo "tabfugnic/asdf.el")
  :after (asdf-enable))

(use-package rust-mode
  :straight t
  :init
  (setq rust-mode-treesitter-derive t)
  :hook ((rust-mode . eglot-ensure)))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" default))
 '(ispell-program-name "aspell"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
