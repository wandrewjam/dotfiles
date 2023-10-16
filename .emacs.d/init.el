;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set default font and window size ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'default-frame-alist '(font . "SF Mono-14"))
(add-to-list 'default-frame-alist '(width . 90))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn off a bunch of UI elements ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 5)

(setq-default inhibit-startup-screen t)
(setq-default package-selected-packages '(ess magit))
(setq-default frame-resize-pixelwise t)

;;;;;;;;;;;;;;;
;; Use Melpa ;;
;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display line numbering in most modes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode customize-mode help-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

;; Redefines the `display-line-numbers--turn-on' function
(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
	      (member major-mode display-line-numbers-exempt-modes))
    (setq display-line-numbers-type 'relative)
    (display-line-numbers-mode)))

(global-display-line-numbers-mode)

;; Add column numbering
(column-number-mode)

(use-package ivy
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; Set theme
(use-package doom-themes
  :config
  (load-theme 'doom-challenger-deep t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Developer")
    (setq projectile-project-search-path '("~/Developer")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-mode-line "Projectile"))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package eglot)
;; (use-package lsp-mode
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :commands (lsp lsp-deferred)
;;   :hook
;;   ((python-mode . lsp)
;;    (lsp-mode . lsp-enable-which-key-integration))
;;   :config
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-tramp-connection "pylsp")
;;     :major-modes '(python-mode)
;;     :remote? t
;;     :server-id 'pylsp-remote))
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-tramp-connection "pylsp")
;;     :major-modes '(python-mode)
;;     :remote? t
;;     :server-id 'stupid-name)))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; Unbind ess-smart-S-assign-key
(setq-default ess-smart-S-assign-key nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" default))
 '(package-selected-packages
   '(eglot doom-themes ivy-rich which-key rainbow-delimiters counsel ivy auctex markdown-mode csv-mode ess magit)))

;; Source from .emacs_local, when it exists
(let ((custom "~/.emacs_local"))
  (when (file-exists-p custom)
    (load-file custom)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
