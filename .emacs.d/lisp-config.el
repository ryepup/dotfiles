;;;; LISP/SLIME SETUP

(push (cons "\\.\\(lisp\\|asd\\)\\'" 'utf-8-unix) 
      auto-coding-alist)

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

(defun slime-sakimet ()
  "Start Slime without an inferior lisp."
  (interactive)
  (shell-command "ssh -o ServerAliveInterval=300 -L 4005:127.0.0.1:4213 -L4313:127.0.0.1:4313 -L 8213:127.0.0.1:8213  sakimet bash --login start-swank.sh &")
  (sleep-for 10)
  (slime-connect "localhost" 4005))


