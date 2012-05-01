(push (expand-file-name "~/.emacs.d") load-path)
(setq backup-directory-alist '(("." . "~/.emacs-backups"))) ; stop leaving backup~ turds scattered everywhere

(custom-set-variables ; stop leaving backup ~ & # turds scattered everywhere
  '(auto-save-file-name-transforms '((".*" "~/.emacs-backups/\\1" t))))

(prefer-coding-system 'utf-8-unix)

(require 'mediawiki)
(require 'smex)
(tool-bar-mode 0)
(show-paren-mode t)
(global-auto-revert-mode 1)

;;this breaks tramp right now: ^m on the end of every line; disable it.
(setq vc-handled-backends (delq 'Git vc-handled-backends))

;; never deploy overwrite-mode
(add-hook 'post-command-hook
	  (lambda () (if overwrite-mode (overwrite-mode 0))))

;;restore the emacs session on startup.
(desktop-save-mode 1)

(require 'uniquify) 
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; Show line-number in the mode line
(line-number-mode 1)
;; Show column-number in the mode line
(column-number-mode 1)

(pc-selection-mode t)

(require 'redo)
(global-set-key (read-kbd-macro "C-z") 'undo)
(global-set-key (read-kbd-macro "C-S-Z") 'redo)
(global-set-key (read-kbd-macro "<C-tab>") 'other-window)

;; Replace yes or no with y or n
(defalias 'old-yes-or-no-p (function yes-or-no-p))
(defalias 'yes-or-no-p (function y-or-n-p))

;;start emacs listening for other "open file" requests from windows
(server-start)

;;; ask before quitting
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))
 
(when window-system 
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

(add-hook 'xml-mode-hook
	  (lambda ()
	    (setq sgml-indent-data 'T)))

;; some keys for org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;;ibuffer goodness
(require 'ibuffer) 
(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'color-theme)
(color-theme-initialize)
(color-theme-calm-forest)

(require 'js2-mode)
(require 'php-mode)

(custom-set-variables
 '(html-helper-mode-global-JSP-not-ASP nil nil (html-helper-mode))
 '(html-helper-mode-uses-visual-basic t nil (html-helper-mode)))

(autoload 'asp-html-helper-mode "html-helper-mode" "Yay ASP" t)

(setq auto-mode-alist
      (append '(("\.xul$" . xml-mode)
		("\.rdf$" . xml-mode)
		("\.config$" . xml-mode)
		("\.css$" . css-mode)
		("\.build$" . xml-mode)
		("\.wdproj$" . xml-mode)
		("\.rhtml$" . html-mode)
		("\.include$" . xml-mode)
		("\\.org$" . org-mode)
		("\.rhtml$" . html-mode)
		("\\.cfc$" . html-mode)
		("\\.cfm$" . html-mode)
		("\\.tal$" . html-mode)
		("\\.php" . php-mode)
		("\\.js$" . js2-mode)
		("\\.asp$" . asp-html-helper-mode)) 
	      auto-mode-alist))

(push (cons "\\.\\(lisp\\|asd\\|sh\\|py\\|cl\\)\\'" 'utf-8-unix) auto-coding-alist)
(setq default-buffer-file-coding-system 'utf-8-unix)

(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)
