;;; init.el --- early init  -*- lexical-binding: t -*-
(set-default-coding-systems 'utf-8)
(split-window-horizontally)

(require 'package)
(progn
	(setq package-check-signature nil)
	(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
	(package-initialize))
;; (package-refresh-contents)

(add-to-list 'load-path "~/.emacs.d/local/")
;; -------------------------------------------------------------------

;; keys
(require 'hamed-keyboard)
(global-unset-key [mouse-2])

(define-key key-translation-map (kbd "M-g") (kbd "C-g"))
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-j") 'backward-char)

(require 'icomplete)
(progn
  (define-key icomplete-minibuffer-map (kbd "M-k")
							'icomplete-forward-completions)
	(define-key icomplete-minibuffer-map (kbd "M-i")
							'icomplete-backward-completions))

(require 'corfu)
(global-set-key (kbd "M-k") 'corfu-next)
(global-set-key (kbd "M-i") 'corfu-previous)

(global-set-key (kbd "M-b") 'consult-buffer) ; minibuffer

(require 'dired)
(require 'dired-x)
(progn
	(define-key dired-mode-map "l" 'dired-find-alternate-file)
	(define-key dired-mode-map "k" 'dired-next-line)
	(define-key dired-mode-map "i" 'dired-previous-line)
	(define-key dired-mode-map "j" (lambda ()
																	 (interactive)
																	 (find-alternate-file "..")))
	(define-key dired-mode-map (kbd "M-<return>") 'dired-find-file-other-window))


(require 'mouse)
(progn
	(setq mouse-wheel-progressive-speed nil)
	(setq scroll-preserve-screen-position t
				scroll-conservatively 101
				history-length 1000
				scroll-margin 2))
;; -------------------------------------------------------------------

;; change defluts
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files										nil
			create-lockfiles										nil
			confirm-nonexistent-file-or-buffer	nil)

;; AutoSave
(setq after-focus-change-function '(lambda () (save-some-buffers t)))




;; -------------------------------------------------------------------

;; funcs
(defun date ()
	(interactive)
	(insert (format-time-string "%d-%m-%Y")))

(defun insert-chosen-path ()
  "Insert the user's chosen path into the buffer."
  (interactive)
  (let ((default-path (concat default-directory "")))
    (setq chosen-path
					(read-directory-name "Choose a directory: " default-path))
    (when (string= chosen-path "")
      (setq chosen-path default-path))
    (insert chosen-path)))
;; -------------------------------------------------------------------

;; code editing
(fido-vertical-mode)
(setq echo-keystrokes 0.02)
(setq minibuffer-prompt-properties '( read-only nil
																			cursor-intangible t
																			face minibuffer-prompt))

(winner-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-subword-mode 1)									; camelCase
(setq-default tab-width 2)

(electric-pair-mode 1)									;Parentheses
(setq show-paren-delay 0)
(show-paren-mode)

(global-hl-line-mode 1)
(global-visual-line-mode 0)
(pulsar-global-mode)

(require 'treesit)
(treesit-available-p)

;; treesit-language-source-alist
;; (setq treesit-language-source-alist
;;    '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
;; 		 (dart "https://github.com/UserNobody14/tree-sitter-dart")
;;      (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(require 'cape)
(progn
	(add-to-list 'completion-at-point-functions #'cape-dabbrev)
	(add-to-list 'completion-at-point-functions #'cape-file))

(require 'corfu)
(progn
	(global-corfu-mode)
	(setq corfu-auto t))

(progn
	(setq eldoc-idle-delay 2))

(marginalia-mode)

;; eglot
;; (setq eglot-stay-out-of '(flymake))
(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))


;; C 
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))


;; -------------------------------------------------------------------
;; look

(setq-default word-wrap t)
(toggle-truncate-lines -1)

(setq visible-bell nil)
(fringe-mode '(1 . 4))
(set-frame-parameter (selected-frame) 'internal-border-width 1)

(add-to-list 'default-frame-alist '(font . "Iosevka-10.5"))

(defun hamed-set-margins ()
  (setq left-fringe-width 15
				right-fringe-width 15))

(dolist (hook '(text-mode-hook
								eshell-mode-hook
								shell-mode-hook
								org-mode-hook
								help-mode-hook
								info-mode-hook))
  (add-hook hook 'hamed-set-margins))

;; -------------------------------------------------------------------


(progn
	(add-hook 'dired-mode-hook (lambda ()
															 (dired-hide-details-mode)))
	(setq dired-listing-switches "-aBhl  --group-directories-first")
	(put 'dired-find-alternate-file 'disabled nil))



;; text non-code
;; main file for todo, other swtich using a key
(setq ispell-dictionary "en_US")
(setenv "LANG" "en_US")
;; (setenv "DICPATH" "C:/Hunspell")
;; (setq ispell-hunspell-dict-paths-alist
;;       '(("en_US" "C:/Hunspell/en_US-large.dic" "C:/Hunspell/en_US-large.aff")))

(eval-after-load "flyspell"
	'(progn
		 (define-key flyspell-mouse-map [down-mouse-1] #'flyspell-correct-word)))

(require 'org)
(progn
	(setq org-hide-emphasis-markers t))


;; -------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist '((fullscreen . maximized)))
 '(package-selected-packages
	 '(osm markdown-mode consult csv-mode eglot cape corfu dart-mode expand-region flutter kotlin-mode marginalia pulsar undo-fu yaml-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-inlay-hint-face ((t nil))))
