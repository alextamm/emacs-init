;; Proxy settings are specific to site
(if (file-readable-p "~/.emacs.d/site-local.el")
    (load-file "~/.emacs.d/site-local.el"))

(load-file "~/.emacs.d/user-prefs.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minibuffer-prompt ((t nil))))

