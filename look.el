
(global-hl-line-mode 1)
(global-visual-line-mode t)
(pulsar-global-mode)
(setq visible-bell t)

(set-fontset-font "fontset-default" nil 
									(font-spec :size 20 :name "Symbola"))

(setq frame-title-format '("Emacs - %b"))

(fringe-mode '(1 . 4))
(set-face-attribute 'fringe nil :background "#fff")
(set-frame-parameter (selected-frame) 'internal-border-width 1)

(defun line-render (left right)
  (let* ((available-width (- (window-total-width) (length left) )))
    (format (format "%%s%%%ds" available-width) left right)))

(setq-default mode-line-format
							'((:eval
								 (line-render
									(format-mode-line
									 (list (propertize
													(cond ((and buffer-file-name (buffer-modified-p)) " ** ")
																(t                                          " -- "))
													'face '( :foreground "black"
																	 :weight black))
												 (propertize " %b " 'face '(:weight regular))
												 "| %m"
												 (propertize " " 'display '(raise +0.1))
												 (propertize " " 'display '(raise -0.1))))
									(format-mode-line '(" %l:%c " ))))))

(defun hamed-set-margins ()
  (setq left-fringe-width 15
				right-fringe-width 15))

(dolist (hook '(text-mode-hook
								eshell-mode-hook
								org-mode-hook
								help-mode-hook
								info-mode-hook))
  (add-hook hook 'hamed-set-margins))

(provide 'look)
