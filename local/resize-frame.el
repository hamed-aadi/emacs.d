;;; resize-frame.el --- A minor mode to resize frames easily.  -*- lexical-binding: t; -*-

;; Keywords: frames, tools, convenience
;; License: WTFPL


(defvar resize-frame-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<down>") 'enlarge-window)
    (define-key map (kbd "<up>") 'shrink-window)
    (define-key map (kbd "<right>") 'enlarge-window-horizontally)
    (define-key map (kbd "<left>") 'shrink-window-horizontally)
    (set-char-table-range (nth 1 map) t 'resize-frame-done)
    (define-key map (kbd "C-p") 'enlarge-window)
    (define-key map (kbd "C-n") 'shrink-window)
    (define-key map (kbd "C-f") 'enlarge-window-horizontally)
    (define-key map (kbd "C-b") 'shrink-window-horizontally)
    map))

(define-minor-mode resize-frame
  "A simple minor mode to resize-frame.
C-c C-c to apply."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " ResizeFrame"
  ;; The minor mode bindings.
  :keymap resize-frame-map
  :global t
  (if (<= (length (window-list)) 1)
      (progn (setq resize-frame nil)
             (message "need more frams to resize."))
      (message "resize frames with arrow keys.")))

(defun resize-frame-done ()
  (interactive)
  (setq resize-frame nil)
  (message "Done."))

(global-set-key (kbd "ESC `") 'resize-frame)

(provide 'resize-frame)
;;; resize-frame.el ends here
