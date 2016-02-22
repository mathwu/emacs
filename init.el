;; coding information
(prefer-coding-system 'utf-8)

;;----------------------------------------------------------------------------
;; basic definitions
;;----------------------------------------------------------------------------
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win* (eq system-type 'windows-nt))

;;----------------------------------------------------------------------------
;; set directories
;;----------------------------------------------------------------------------
(when *is-a-win*
  (add-to-list 'load-path "C:/emacs/share/emacs/site-lisp")
  (setq command-line-default-directory "D:/OneDrive/work")
  (setq default-directory "D:/OneDrive/work")
  (setq package-user-dir "D:/OneDrive/emacs/elpa")
  (defvar base-path "D:/emacs"))

(when *is-a-mac*
  (setq path "/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin")
  (setenv "PATH" path)
  (setq command-line-default-directory "~/OneDrive/")
  (setq default-directory "~/Onedrive/")
  (add-to-list 'load-path "/opt/local/share/emacs/site-lisp/")
  (setq package-user-dir "~/Onedrive/emacs/elpa")
  (defvar base-path "~/.emacs.d"))

(defvar lisp-path (concat base-path "/lisp"))
(defvar plugin-path (concat base-path "/plugins"))
(defvar elpa-path (concat base-path "/elpa"))
(defvar org-path (concat plugin-path "/org-mode"))
(defvar my-snippet-path (concat base-path "/snippets"))
(defvar theme-path (concat base-path "/themes"))

(add-to-list 'load-path lisp-path)
(add-to-list 'load-path plugin-path)
(add-to-list 'custom-theme-load-path theme-path)
(add-to-list 'load-path theme-path)
(add-to-list 'load-path my-snippet-path)

(setq package-user-dir elpa-path)
(require 'init-elpa)

;;----------------------------------------------------------------------------
;; font settings
;;----------------------------------------------------------------------------
(when *is-a-mac*
  ;; English font
  (set-face-attribute 'default nil :font "Dejavu Sans Mono 18")
  ;; Chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
		      charset
		      (font-spec :family "Hiragion Sans" :size 20))))

(when *is-a-win*
  ;; English font
  (set-face-attribute 'default nil :font "Consolas 18")
  ;; Chinses font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
		      charset
		      (font-spec :family "Microsoft YaHei" :size 20))))

(global-font-lock-mode t) 
(setq font-lock-maximum-decoration t)

;;----------------------------------------------------------------------------
;; themes
;;----------------------------------------------------------------------------
(setq solarized-termcolors 256)
(setq solarized-broken-srgb t)
(load-theme 'solarized t)
;(load-theme 'zenburn t)
;(load-theme 'dracula t)

(set-frame-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)

;(require 'color-theme-sanityinc-tomorrow)
;(load-theme 'sanityinc-tomorrow-night t)

;;----------------------------------------------------------------------------
;; style of the buffer
;;----------------------------------------------------------------------------

(set-scroll-bar-mode nil)
(setq column-number-mode t)
(setq line-number-mode t)
(global-linum-mode 1)
(setq gnus-inhibit-startup-message t) 
(setq initial-frame-alist '((width . 60) (height . 30)))
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
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
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
	      (cdlatex-mode 1)
	      (reftex-mode 1))))

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

;; start emacs in server mode so that skim can talk to it
(require 'server)
(unless (server-running-p)
  (server-start))


;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
