(push (expand-file-name "~/.emacs.d") load-path)

;(require 'edit-server)
;(edit-server-start)


;; some keys for org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)


;(require 'zencoding-mode)
;(add-hook 'sgml-mode-hook 'zencoding-mode)
