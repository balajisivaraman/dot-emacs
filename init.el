;; Benchmarking Emacs Startup
(defconst emacs-start-time (current-time))

(when (window-system)
  (let ((elapsed-time (float-time (time-subtract (current-time)
						 emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed-time))

  (add-hook 'after-init-hook
	    `(lambda ()
	       (let ((elapsed-time (float-time (time-subtract (current-time)
							      emacs-start-time))))
		 (message "Loading %s...done (%.3fs)" ,load-file-name elapsed-time)))
	    t))
