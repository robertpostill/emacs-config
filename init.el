;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

;;; Commentary:
;; Robert Postill's Emacs config
;;

;; things that don't require packages
(setq inhibit-startup-screen +1) ;straight to a buffer
(global-visual-line-mode 1) ; word wrapping
(show-paren-mode 1) ; highlight parentheses
(winner-mode 1) ; working with windows
(desktop-save-mode 1) ; so I can have a stack of buffers open at any one time
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(set-face-attribute 'default nil :font "Menlo-14" )

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
  :config
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

;; ivy and firends
(use-package ivy
  :straight t
  :init
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
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

;; terraform developemnt
(use-package terraform-mode
  :straight t)
