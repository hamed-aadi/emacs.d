(require 'ibuffer)
(progn
	(setq ibuffer-show-empty-filter-groups nil
				ibuffer-expert t)

	(setq ibuffer-saved-filter-groups
				(quote (("default"
								 ("dired" (mode . dired-mode))
								 ("emacs lisp" (mode . emacs-lisp-mode))
								 ("term" (or
													(mode . eshell-mode)
													(mode . shell-mode)))
								 ("text" (filename . "text"))
								 ("emacs" (or
													 (name . "^\\*scratch\\*$")
													 (name . "^\\*Messages\\*$")))
								 ("help" (or
													(name . "^\\*Help\\*$")
													(mode . info)))))))

	(add-hook 'ibuffer-mode-hook
						(lambda ()
							(ibuffer-switch-to-saved-filter-groups "default"))))


(require 'dired )
(require 'dired-x)
(progn
	(add-hook 'dired-mode-hook (lambda ()
															 (dired-hide-details-mode)))
	(setq dired-listing-switches "-aBhl  --group-directories-first")
	(put 'dired-find-alternate-file 'disabled nil))


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

(require 'org)
(progn
	(setq org-hide-emphasis-markers t))


(provide 'modes)
