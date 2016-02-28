(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(require 'multiple-cursors)
(require 'auto-complete)

(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(setq inhibit-splash-screen 1)
(setq inhibit-startup-echo-area-message "alex")
(setq transient-mark-mode t)

(column-number-mode 1)
(line-number-mode 1)

;; (define-key global-map "\e[1;2P" [S-f1])
;; (define-key function-key-map "\e[1;2Q" [S-f2])
;; (define-key function-key-map "\e[1;2R" [S-f3])
;; (define-key function-key-map "\e[1;2S" [S-f4])


;; No menu in terminal
(cond ((string= 'nil window-system)
       (menu-bar-mode 0)
       ))
(set-scroll-bar-mode 'right)

;; This is for editing blocks and things:
(cua-selection-mode t)
(setq cua-auto-tabify-rectangles nil)	;; Don't tabify after rectangle commands
(transient-mark-mode 1)			;; No region when it is not highlighted
(setq cua-keep-region-after-copy t)	;; Standard Windows behaviour
(setq cua-enable-cua-keys nil)


(add-to-list 'load-path "~/.emacs.d/plugins")
;;(require 'yasnippet)

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

(defun my-c++-indent-setup ()
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil))

;;(add-hook 'fundamental-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key "\C-i" 'self-insert-command)
;; 	    ))
;; 
;;(add-hook 'text-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key "\C-i" 'self-insert-command)
;; 	    ))

(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key "\C-i" 'org-cycle)
	    ))

(add-hook 'c++-mode-hook
          (lambda ()
            (my-c++-indent-setup)
            (local-set-key "\r" 'newline-and-indent)
            (local-set-key "\C-j" 'newline)
            )
          )

(add-hook 'c-mode-hook
          (lambda ()
            (my-c++-indent-setup)
            (local-set-key "\r" 'newline-and-indent)
            (local-set-key "\C-j" 'newline)
            )
          )

;; A function for regular search and replace-tasks
;; If you need to bind to a key, for example.
;; Template for replacing tabs to eight spaces and vice versa
;; Made by Fingis at Frantic
;; (defun schwartz ()
;;   (interactive)
;;   (if (and transient-mark-mode
;;            mark-active)
;;       (save-excursion
;; 	(save-restriction
;; 	  (narrow-to-region (region-beginning) (region-end))
;; 	  (goto-char (point-min))
;; 	  (replace-string "foo" "bar")
;; 	  )
;; 	)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (replace-string "foo" "BAR")
;;       )
;;     )
;;   )

(defun forward-paren ()
  (interactive)
  (search-forward-regexp "\\((\\|{\\|\\[\\)")
  )

(defun backward-paren ()
  (interactive)
  (search-backward-regexp "\\()\\|}\\|\\]\\)")
  )

(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
          (lambda ()
            (setq cperl-indent-level 4)
            (setq indent-tabs-mode nil)
;;           (setq cperl-electric-parens t)
;;           (setq cperl-electric-keywords t)
            (setq cperl-merge-trailing-else nil)
            (local-set-key "\r" 'newline-and-indent)
            (local-set-key "\C-j" 'newline)
            (abbrev-mode)
            ;;(setq cperl-hairy t)
            )
          )


(setq hippie-expand-try-functions-list '(
					 try-expand-dabbrev
					 yas/hippie-try-expand
					 )
)

;;(global-set-key (kbd "<return>") (kbd "<return>"))
(global-set-key "\C-n" 'forward-paren)
(global-set-key "\C-p" 'backward-paren)

(fset 'yes-or-no-p 'y-or-n-p)


(defun My-smart-home () "Odd home to beginning of line, even home to beginning of text/code."
  (interactive)
  ;; the "^" is supposed to do something smart with shift-selection-mode
  ;; however, the shift-selection mode doesn't seem terribly smart
  ;;(interactive "^")
  (if (and (eq last-command 'My-smart-home)
	   (/= (line-beginning-position) (point)))
      (beginning-of-line)
    (beginning-of-line-text))
  )

(global-set-key [home] 'My-smart-home)
(global-set-key "\C-b" 'cua-set-rectangle-mark)
(global-set-key (kbd "S-<delete>") 'kill-whole-line)
(global-set-key [S-f1] 'cperl-mode)
(global-set-key [f1] 'find-file)
