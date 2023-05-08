;;; general
(setq w32-pass-apps-to-system nil)

(define-key key-translation-map (kbd "M-g") (kbd "C-g"))
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-j") 'backward-char)

(require 'corfu)
(progn
	(global-set-key (kbd "M-k") 'corfu-next)
	(global-set-key (kbd "M-i") 'corfu-previous))

;; minibuffer
(require 'icomplete)
(progn
	(define-key icomplete-minibuffer-map (kbd "M-k") 'icomplete-forward-completions)
	(define-key icomplete-minibuffer-map (kbd "M-i") 'icomplete-backward-completions))


(global-set-key (kbd "M-b") 'consult-buffer)

(require 'mouse)
(progn
	(setq mouse-wheel-progressive-speed nil)
	(setq scroll-preserve-screen-position t
				scroll-conservatively 101
				history-length 1000
				scroll-margin 10))


;;; mode specific
(require 'dired )
(require 'dired-x)
(progn
	(define-key dired-mode-map "l" 'dired-find-alternate-file)
	(define-key dired-mode-map "k" 'dired-next-line)
	(define-key dired-mode-map "i" 'dired-previous-line)
	(define-key dired-mode-map "j" (lambda ()
																	 (interactive)
																	 (find-alternate-file "..")))
	(define-key dired-mode-map (kbd "M-<return>") 'dired-find-file-other-window))


(provide 'keys)
