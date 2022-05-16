(setq package-enable-at-startup nil
      gc-cons-threshold most-positive-fixnum)

(setq-default mode-line-format nil)

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq file-name-handler-alist default-file-name-handler-alist)) 100)
