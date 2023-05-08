;;; init.el --- early init  -*- lexical-binding: t -*-
(set-default-coding-systems 'utf-8)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package)
(progn
	(setq package-check-signature nil)
	(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
	(package-initialize))
;; (package-refresh-contents)

(add-to-list 'load-path "~/.emacs.d/")
(require 'modes)
(require 'keys)
(require 'look)

(add-to-list 'load-path "~/.emacs.d/local/")
(require 'resize-frame)
(require 'hamed-keyboard)
(require 'marginalia)

(setq make-backup-files nil
			create-lockfiles nil
			confirm-nonexistent-file-or-buffer nil)

(setq after-focus-change-function '(lambda () (save-some-buffers t)))

(defun date ()
	(interactive)
	(insert (format-time-string "%d-%m-%Y")))

(defun insert-chosen-path ()
  "Insert the user's chosen path into the buffer."
  (interactive)
  (let ((default-path (concat default-directory "")))
    (setq chosen-path (read-directory-name "Choose a directory: " default-path))
    (when (string= chosen-path "")
      (setq chosen-path default-path))
    (insert chosen-path)))

(fido-vertical-mode)
(setq echo-keystrokes 0.02)
(setq minibuffer-prompt-properties '(read-only nil
																							 cursor-intangible t
																							 face minibuffer-prompt))

(winner-mode)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-subword-mode 1)									; camelCase

(require 'aggressive-indent)
(progn (global-aggressive-indent-mode 1)
			 (setq-default tab-width 2))

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

(electric-pair-mode 1)									;Parentheses
(setq show-paren-delay 0)
(show-paren-mode)


(setq gc-cons-threshold (* 1024 1024 8))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "outline" :slant normal :weight regular :height 105 :width normal))))
 '(highlight-indent-guides-character-face ((t (:foreground "#e665e665e665" :slant normal :weight heavy :height 115 :width normal :foundry "outline" :family "Iosevka")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
	 '(((:application eshell)
			eshell-connection-default-profile)))
 '(connection-local-profile-alist
	 '((eshell-connection-default-profile
			(eshell-path-env-list))))
 '(eglot-ignored-server-capabilities
	 '(:documentFormattingProvider :documentRangeFormattingProvider :documentOnTypeFormattingProvider :inlayHintProvider))
 '(package-selected-packages
	 '(kotlin-mode marginalia pdf-tools consult yaml-mode flutter dart-mode undo-fu pulsar highlight-indent-guides expand-region corfu cape ahk-mode aggressive-indent))
 '(warning-suppress-types '((initialization))))
(put 'erase-buffer 'disabled nil)
