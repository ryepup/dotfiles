;;;; LISP/SLIME SETUP

(setf slime-net-coding-system 'utf-8-unix)
(setf slime-enable-evaluate-in-emacs t)

(defun scratch-lisp-file ()
  "Insert a template (with DEFPACKAGE and IN-PACKAGE forms) into
  the current buffer."
  (interactive)
  (goto-char 0)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (package (file-name-sans-extension file)))
    (insert ";;;; " file "\n")
    (insert "\n(defpackage #:" package "\n  (:use #:cl))\n\n")
    (insert "(in-package #:" package ")\n\n")))

(defvar hs-toggle-all-last-val nil)
(defun hs-toggle-all ()
  (interactive)
  (make-local-variable 'hs-toggle-all-last-val)
  ;; only modify buffer local copy
  (setq hs-toggle-all-last-val (not hs-toggle-all-last-val))
  (if hs-toggle-all-last-val
      (hs-hide-all)
      (hs-show-all)))

;; Customize post mode a bit.
(defun my-lisp-mode-hook ()
  (hs-minor-mode 1)
  (local-set-key (kbd "C-+" ) 'hs-toggle-hiding)
  (local-set-key (kbd "C-M-+" ) 'hs-toggle-all))

(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)