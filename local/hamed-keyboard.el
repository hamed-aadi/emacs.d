;;;;

;;; FUNCTIONS 
(defun endline-indented ()
	(interactive)
	(end-of-line)
	(newline-and-indent))

(defun startline-indented ()
	(interactive)
	(previous-line)
	(end-of-line)
	(newline-and-indent))

;; FIXME: don't work on first open file
(defun isearch-at-point (&optional start end) 
  (interactive "r")
	(if (region-active-p)
			(progn
				(let ((string (buffer-substring-no-properties start end)))
					(deactivate-mark)
					(isearch-resume string nil nil t string nil)))
    (call-interactively 'isearch-forward-regexp)))

(defun delete-to-endline ()
	(interactive)
	(delete-region (point) (line-end-position)))

;; 
;;; hmd-mode
(require 'rect)

(defvar hmd-mode-map (make-keymap))
(defvar hmd-mode-cursor 'box)
(defvar hmd-mode-insert-cursor 'bar)

(defun hmd-mode--toggle-cursor ()
	"Toggles the cursor depending on status of hmd-mode."
	(if hmd-mode
			(setq cursor-type hmd-mode-cursor)
		(setq cursor-type hmd-mode-insert-cursor)))


(defun hmd-mode--minibuffer-setup-hook ()
	(setq overriding-terminal-local-map nil))

;;;###autoload
(define-minor-mode hmd-mode
	"A modal editing mode for emacs."
	:lighter "hmd command"
	:keymap hmd-mode-map
	:after-hook
	(progn
		(hmd-mode--toggle-cursor)
		(if hmd-mode
				(setq-local hmd-mode-exit-map
										(set-transient-map hmd-mode-map t))
			(when (boundp 'hmd-mode-exit-map)
				(funcall hmd-mode-exit-map)))))


(add-hook 'buffer-list-update-hook          'hmd-mode--toggle-cursor)
(add-hook 'window-configuration-change-hook 'hmd-mode--toggle-cursor)
(add-hook 'minibuffer-setup-hook            'hmd-mode--minibuffer-setup-hook)

(defun hmd-mode-disable ()
	(interactive)
	(when hmd-mode
		(hmd-mode -1)))

(global-set-key (kbd "M-SPC") 'hmd-mode)

;; Move
(define-key hmd-mode-map (kbd "j")		'backward-char)
(define-key hmd-mode-map (kbd "l")		'forward-char)
(define-key hmd-mode-map (kbd "k")		'next-line)
(define-key hmd-mode-map (kbd "i")		'previous-line)

(define-key hmd-mode-map (kbd "o")		'forward-paragraph)
(define-key hmd-mode-map (kbd "u")		'backward-paragraph)
(define-key hmd-mode-map (kbd ";")		'forward-word )
(define-key hmd-mode-map (kbd "h")		'backward-word)

;; Go
(define-key hmd-mode-map (kbd "g g")	'goto-line)
(define-key hmd-mode-map (kbd "g i")	'beginning-of-buffer)
(define-key hmd-mode-map (kbd "g k")	'end-of-buffer)
(define-key hmd-mode-map (kbd "g l")	'end-of-line)
(define-key hmd-mode-map (kbd "g j")	'back-to-indentation)
(define-key hmd-mode-map (kbd "g o")	'forward-section)
(define-key hmd-mode-map (kbd "g u")	'backward-section)

;; Basic
(define-key hmd-mode-map (kbd "v")			'yank)
(define-key hmd-mode-map (kbd "SPC v")	'consult-yank-from-kill-ring)

(define-key hmd-mode-map (kbd "SPC s")	'save-buffer)

(define-key hmd-mode-map (kbd "c")			'kill-ring-save)
(define-key hmd-mode-map (kbd "x")			'kill-region)

(define-key hmd-mode-map (kbd "s")			'endline-indented)
(global-set-key (kbd "M-s")							'startline-indented)

;; mark
(require 'expand-region)
(define-key hmd-mode-map (kbd "m") 'er/expand-region)
(define-key hmd-mode-map (kbd "SPC m") 'set-mark-command)

;; find
(define-key hmd-mode-map (kbd "f f") 'isearch-at-point)
(define-key isearch-mode-map (kbd "M-i") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "M-k") 'isearch-repeat-forward)

;; replace
(global-set-key (kbd "M-r") 'overwrite-mode)
(define-key hmd-mode-map (kbd "f r") 'query-replace)

;; jump
(define-key hmd-mode-map (kbd "SPC f") 'find-file)
(define-key hmd-mode-map (kbd "SPC SPC f") 'find-file-other-window)

(define-key hmd-mode-map (kbd "SPC j") 'bookmark-jump)
(define-key hmd-mode-map (kbd "SPC SPC j") 'bookmark-jump-other-window)

(global-set-key (kbd "M-f") 'consult-imenu)


;; undo/redo
(define-key hmd-mode-map (kbd "y") 'undo-fu-only-undo)
(define-key hmd-mode-map (kbd "t") 'undo-fu-only-redo)
(define-key hmd-mode-map (kbd "SPC y") 'winner-undo)
(define-key hmd-mode-map (kbd "SPC t") 'winner-redo)

;; buffer/window
(define-key hmd-mode-map (kbd "e")			'other-window)
(define-key hmd-mode-map (kbd "b")			'mode-line-other-buffer)
(define-key hmd-mode-map (kbd "SPC k")	'kill-current-buffer)
(define-key hmd-mode-map (kbd "SPC b")	'ibuffer-jump)
(define-key hmd-mode-map (kbd "SPC 3")	'split-window-horizontally)
(define-key hmd-mode-map (kbd "SPC 2")	'split-window-vertically)
(define-key hmd-mode-map (kbd "SPC 1")	'delete-other-windows)
(define-key hmd-mode-map (kbd "SPC 0")	'delete-window)

;; Delete
(define-key hmd-mode-map (kbd "w")   'kill-whole-line)
;; FIXME: delete from /n+empty to /nempty "^[ \t]*\n"
(define-key hmd-mode-map (kbd "d d") 'delete-region)
(define-key hmd-mode-map (kbd "d l") 'kill-word)
(define-key hmd-mode-map (kbd "d j") 'backward-kill-word)
;; FIXME: meke same as d k
(define-key hmd-mode-map (kbd "d h") 'backward-kill-sentence)
(define-key hmd-mode-map (kbd "d ;") 'delete-to-endline)

(define-key hmd-mode-map (kbd "d k")
						(lambda ()	(interactive)
							(progn
								(next-line) (kill-whole-line) (previous-line))))

(define-key hmd-mode-map (kbd "d i") (lambda () (interactive)
																			 (progn
																				 (previous-line) (kill-whole-line))))
;; Comment
(define-key hmd-mode-map (kbd "z l") 'comment-line)
(define-key hmd-mode-map (kbd "z r") 'comment-region)
(define-key hmd-mode-map (kbd "z y") 'uncomment-region)
(define-key hmd-mode-map (kbd "z t") 'commented-title)

;; misc 
(define-key key-translation-map  (kbd "SPC h") (kbd "C-h"))


;; (define-key hmd-mode-map (kbd " ") ')
(provide 'hamed-keyboard)
