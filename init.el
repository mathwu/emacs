;; coding information
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq user-full-name "Senlin Wu")
(setq user-mail-address "wusenlin@outlook.com")

;;----------------------------------------------------------------------------
;; basic definitions
;;----------------------------------------------------------------------------
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win* (eq system-type 'windows-nt))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;;----------------------------------------------------------------------------
;; set directories
;;----------------------------------------------------------------------------
(when *is-a-win*
  (add-to-list 'load-path "C:/emacs/share/emacs/site-lisp")
  (setq command-line-default-directory "D:/work")
  (setq default-directory "D:/work")
  (defvar base-path "D:/emacs"))

(when *is-a-mac*
  (setq path "/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin")
  (setenv "PATH" path)
;  (setq command-line-default-directory "~/work/")
  (setq default-directory "~/work/")
  (add-to-list 'load-path "/opt/local/share/emacs/site-lisp/")
  (defvar base-path "~/.emacs.d"))

(when *is-a-linux*
  (setq path "/home/wusenlin/bin:/usr/local/bin:/usr/bin:/bin:/usr/bin/X11:/usr/games")
  (setenv "PATH" path)
  (setq command-line-default-directory "~/work/")
  (setq default-directory "~/work/")
  (defvar base-path "~/.emacs.d"))


(defvar lisp-path (concat base-path "/lisp"))
(defvar plugin-path (concat base-path "/plugins"))
(defvar elpa-path (concat base-path "/elpa"))
(defvar org-path (concat base-path "/org"))
(defvar my-snippet-path (concat base-path "/snippets"))
(defvar theme-path (concat base-path "/themes"))
(defvar cfs-profiles-directory (concat base-path "/chinese-fonts-setup"))

(add-to-list 'load-path lisp-path)
(add-to-list 'load-path plugin-path)
(add-to-list 'custom-theme-load-path theme-path)
(add-to-list 'load-path theme-path)
(add-to-list 'load-path my-snippet-path)
(add-to-list 'load-path cfs-profiles-directory)

(setq load-path (cons (concat org-path "/lisp") load-path))
(setq load-path (cons (concat org-path "/contrib/lisp") load-path))

;(add-to-list 'load-path (concat org-path "lisp"))
;(add-to-list 'load-path (concat org-path "/contrib/lisp"))

(setq package-user-dir elpa-path)
(require 'init-elpa)

;;----------------------------------------------------------------------------
;; font settings
;;----------------------------------------------------------------------------
(require 'chinese-fonts-setup)

(setq cfs-profiles
    '("mac" "win" "linux"))
(when *is-a-mac*
  (custom-set-variables
   '(cfs--current-profile-name "mac" t)
   '(cfs--fontsize-steps (quote (7 4 4)) t)))

(when *is-a-win*
  (custom-set-variables
   '(cfs--current-profile-name "win" t)
   '(cfs--fontsize-steps (quote (4 4 4)) t)))

 (when *is-a-linux*
 (custom-set-variables
    '(cfs--current-profile-name "linux" t)
    '(cfs--fontsize-steps (quote (8 4 3)) t)))



;(global-font-lock-mode t) 
;(setq font-lock-maximum-decoration t)

;;----------------------------------------------------------------------------
;; themes
;;----------------------------------------------------------------------------

(load-theme 'zenburn t)

;;----------------------------------------------------------------------------
;; style of the buffer
;;----------------------------------------------------------------------------

(set-scroll-bar-mode nil)
(setq column-number-mode t)
(setq line-number-mode t)
(global-linum-mode 1)
(setq gnus-inhibit-startup-message t) 
(setq initial-frame-alist '((width . 120) (height . 60)))
(setq track-eol t)
(display-time-mode t)
(setq display-time-24hr-format t)

;;----------------------------------------------------------------------------
;; spell check
;;----------------------------------------------------------------------------
(when *spell-check-support-enabled*
  (setq-default ispell-program-name "aspell")
  (setq-default ispell-local-dictionary "american")
  (global-set-key (kbd "") 'ispell-complete-word))

;;----------------------------------------------------------------------------
;; CDLaTeX
;;----------------------------------------------------------------------------

(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)

;;----------------------------------------------------------------------------
;; LaTeX
;;----------------------------------------------------------------------------

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(require 'tex-mik)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode

(setq reftex-plug-into-AUCTeX t)
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)


(add-hook 'LaTeX-mode-hook 
	  (lambda()
;	    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
	    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))	    
	    (setq TeX-command-default "XeLaTeX")
	    (setq TeX-save-query  nil )
	    (setq TeX-show-compilation t)))

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)

(when *is-a-mac*
    (add-hook 'LaTeX-mode-hook
	      (lambda()
		(add-to-list 'TeX-expand-list
			     '("%q" skim-make-url))))
    (defun skim-make-url () (concat
			     (TeX-current-line)
			     " "
			     (expand-file-name (funcall file (TeX-output-extension) t)
					       (file-name-directory (TeX-master-file)))
			     " "
			     (buffer-file-name)))
    (setq TeX-view-program-list
	  ;; we need Skim installtion here
	  '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline  %n %o %b")))
    (setq TeX-view-program-selection '((output-pdf "Skim"))))


(when *is-a-win*
  (require 'sumatra-forward)
  (custom-set-variables
   '(TeX-engine (quote xetex))
   '(TeX-source-correlate-method (quote synctex))
   '(TeX-view-predicate-list nil)
   '(TeX-view-program-list (quote (("sumatrapdf" "D:/emacs/sumatrapdf.exe %o"))))
   '(TeX-view-program-selection (quote ((output-pdf "sumatrapdf") (output-html "start"))))
   '(preview-auto-cache-preamble nil)
   '(preview-scale-function 1.3)
   '(preview-transparent-color nil)))

(when *is-a-linux*
  (add-hook 'LaTeX-mode-hook '(lambda ()
				(add-to-list 'TeX-expand-list
					     '("%u" Okular-make-url))))
  (defun Okular-make-url () (concat
			     "file://"
			     (expand-file-name (funcall file (TeX-output-extension) t)
					       (file-name-directory (TeX-master-file)))
			     "#src:"
			     (TeX-current-line)
			     (expand-file-name (TeX-master-directory))
			     "./"
			     (TeX-current-file-name-master-relative)))
  (setq TeX-view-program-list
      '(("Okular"
	 ("okular --unique %u"))))
  (setq TeX-view-program-selection '((output-pdf "Okular"))))




;;----------------------------------------------------------------------------
;; more hooks
;;----------------------------------------------------------------------------

(dolist (hook '(text-mode-hook LaTeX-mode-hook tex-mode-hook bibtex-mode-hook org-mode-hook))
  (add-hook hook
            (lambda ()
	      (auto-fill-mode 1)
	      (visual-line-mode 1)
	      (outline-minor-mode 1)
;	      (flyspell-mode 1)
	      (yas-minor-mode 1)
                    (local-set-key (kbd "C-c C-v") 'flyspell-goto-next-error))))


(dolist (hook '(LaTeX-mode-hook tex-mode-hook bibtex-mode-hook org-mode-hook))
  (add-hook hook
            (lambda ()
;	      (outline-minor-mode 1)
	      (cdlatex-mode 1))))

;;----------------------------------------------------------------------------
;; yasnippets
;;----------------------------------------------------------------------------
;(require 'cl-lib)
(require 'yasnippet)
(setq yas-snippet-dirs
      '(my-snippet-path            ;; personal snippets
	))
(yas-global-mode 1)
(yas-reload-all)

;;----------------------------------------------------------------------------
;; asymptote
;;----------------------------------------------------------------------------
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))




;;----------------------------------------------------------------------------
;; org-mode
;;----------------------------------------------------------------------------

(require 'org-install)
;(require 'org-latex)
(require 'ox-latex)
;; The following lines are always needed. Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) ; not needed since Emacs 22.2
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                               "xelatex -interaction nonstopmode %f"))
(add-to-list 'org-latex-classes
	     '("ctexbook"
	      "\\documentclass[fontset=none,UTF8,a4paper,zihao=-4]{ctexbook}"
	      ("\\part{%s}" . "\\part*{%s}")
	      ("\\chapter{%s}" . "\\chapter*{%s}")
	      ("\\section{%s}" . "\\section*{%s}")
	      ("\\subsection{%s}" . "\\subsection*{%s}")
	      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

;;----------------------------------------------------------------------------
;;markdown mode
;;----------------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;----------------------------------------------------------------------------
;; magit
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-x g") 'magit-status)


;; start emacs in server mode so that skim can talk to it
(require 'server)
(unless (server-running-p)
  (server-start))



;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
