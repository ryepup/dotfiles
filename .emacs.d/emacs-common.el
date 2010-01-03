(push (expand-file-name "~/.emacs.d") load-path)

(tool-bar-mode 0)
(show-paren-mode t)
(global-auto-revert-mode 1)

;;restore the emacs session on startup.
(desktop-save-mode 1)

;; does directory completion stuff
(ido-mode 1)

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

;; Replace Stupid yes or no with y or n
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

(setq auto-mode-alist
      (append '(("\.xul$" . xml-mode)
		("\.rdf$" . xml-mode)
		("\.config$" . xml-mode)
		("\.css$" . css-mode)
		("\.build$" . xml-mode)
		("\.wdproj$" . xml-mode)
		("\.rhtml$" . html-mode)
		("\.include$" . xml-mode)
		("\\.org$" . org-mode)) 
	      auto-mode-alist))

(add-hook 'xml-mode-hook
	  (lambda ()
	    (setq sgml-indent-data 'T)))