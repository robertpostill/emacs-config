;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

;;; Commentary:
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
(menu-bar-mode -1)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(set-face-attribute 'default nil :font "Menlo-14" )
(put 'downcase-region 'disabled nil) ; yes I really would like to downcase things
(put 'upcase-region 'disabled nil) ; yes I really want to upcase regions.
(setq-default indent-tabs-mode nil) ; don't use tabs unless your major mode demands it
(fset 'yes-or-no-p 'y-or-n-p) ;; Use y/n instead of yes/no
;; supress the error message when using dired
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
  ("M-o" . 'ace-window))

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
	     ("C-x g" . 'magit-status))
(use-package forge
  :straight t
  :after magit)

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

(use-package flx
  :straight t)

;; ivy and firends
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


;; For jekyll
(use-package org
  :straight t)
(use-package org2jekyll
  :straight t
  :after org
  :hook
  (('org-mode-hook . 'turn-on-flyspell)
   ('org-mode-hook . #'org2jekyll-mode))
  :custom
  (org2jekyll-blog-author "robertpostill")
  (org2jekyll-source-directory (expand-file-name "~/software/robertpostill.github.io/org/"))
  (org2jekyll-jekyll-directory (expand-file-name "~/software/robertpostill.github.io/"))
  (org2jekyll-jekyll-drafts-dir "")
  (org2jekyll-jekyll-posts-dir "_posts/")
  (org-publish-project-alist
   `(("default"
      :base-directory ,(org2jekyll-input-directory)
      :base-extension "org"
      :publishing-directory ,(org2jekyll-output-directory)
      :publishing-function org-html-publish-to-html
      :headline-levels 4
      :section-numbers nil
      :with-toc nil
      :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
      :html-preamble t
      :recursive t
      :make-index t
      :html-extension "html"
      :body-only t)
     ("post"
      :base-directory ,(org2jekyll-input-directory)
      :base-extension "org"
      :publishing-directory ,(org2jekyll-output-directory org2jekyll-jekyll-posts-dir)
      :publishing-function org-html-publish-to-html
      :headline-levels 4
      :section-numbers nil
      :with-toc nil
      :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
      :html-preamble t
      :recursive t
      :make-index t
      :html-extension "html"
      :body-only t)
     ("images"
      :base-directory ,(org2jekyll-input-directory "img")
      :base-extension "jpg\\|gif\\|png"
      :publishing-directory ,(org2jekyll-output-directory "img")
      :publishing-function org-publish-attachment
      :recursive t)
     ("js"
      :base-directory ,(org2jekyll-input-directory "js")
      :base-extension "js"
      :publishing-directory ,(org2jekyll-output-directory "js")
      :publishing-function org-publish-attachment
      :recursive t)
     ("css"
      :base-directory ,(org2jekyll-input-directory "css")
      :base-extension "css\\|el"
      :publishing-directory ,(org2jekyll-output-directory "css")
      :publishing-function org-publish-attachment
      :recursive t)
     ("web" :components ("images" "js" "css")))))

;; terraform developemnt
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

;; Javascript
(setq js-indent-level 2)

;; Typescript
(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :config (setq-default typescript-indent-level 2))

(use-package prettier-js
  :straight t)

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(use-package company
  :straight t
  :init (global-company-mode))

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

;; Racket
(use-package racket-mode
  :straight t
  :hook (racket-mode . racket-xp-mode))
(use-package scribble-mode
  :straight t)

(provide 'init)
;;; init.el ends here
