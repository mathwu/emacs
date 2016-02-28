;(require 'cl)
;(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("popkit" . "http://elpa.popkit.org/packages/")))
(package-initialize)
 
;; automatically installed package
(when (not package-archive-contents)
  (package-refresh-contents))
 
;; setting default package to be installed
(defvar my-default-packages '(dash
			      chinese-fonts-setup
<<<<<<< HEAD
			      magit
=======
>>>>>>> testing
			      auctex
			      yasnippet
			      auto-complete
			      org
			      zenburn-theme
			      ))
(dolist (p my-default-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'init-elpa)
