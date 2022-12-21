;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set default font and window size ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'default-frame-alist '(font . "SF Mono-14"))
(add-to-list 'default-frame-alist '(width . 90))
(add-to-list 'default-frame-alist '(height . 47))
(tool-bar-mode 0)
(setq-default inhibit-startup-screen t)
(setq-default package-selected-packages '(ess magit))
(setq-default frame-resize-pixelwise t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display line numbering in most modes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode customize-mode)
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

;; Unbind ess-smart-S-assign-key
(setq-default ess-smart-S-assign-key nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set autosave and backup locations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
;; (setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;;;;;;;;;;;;;;;;;;;;;;
;; Use Melpa-Stable ;;
;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(auctex markdown-mode csv-mode ess magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Source from .emacs_local, when it exists
(let ((custom "~/.emacs_local"))
  (when (file-exists-p custom)
    (load-file custom)))
