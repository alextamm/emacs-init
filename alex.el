(add-to-list 'load-path "~/bin/emacs-plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")

;; we want the latest and greates cperl-mode
;;(load-file "~/bin/emacs-plugins/cperl-mode.el")

(require 'linum)
(if (string= 'nil window-system)
    (setq linum-format "%2d:") (setq linum-format "%2d"))

(require 'code-mode)
(require 'yasnippet) ;; not yasnippet-bundle
(yas/global-mode 1)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

(defun reload ()
  (interactive)
  (load-file "~/.emacs")
  )

(set 'alex-hilited nil)

(defun alex-highlight ()
  (interactive)
  (let ((tohilight (read-from-minibuffer "Highlight string: ")))
    (unhighlight-regexp alex-hilited)
    (highlight-phrase tohilight (quote hi-pink))
    (set 'alex-hilited tohilight)
    )
  )

(defun alex-unhighlight ()
  (interactive)
  (unhighlight-regexp alex-hilited)
  (set 'alex-hilited nil)
  )

(defun latin ()
  (interactive)
  (set-buffer-file-coding-system 'latin-1)
  (message "File will be save in Latin-1")
  )

(defun utf ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8)
  (message "File will be save in UTF-8")
  )

(defun perl-tidy ()
  (interactive)
  (if (and transient-mark-mode
	   mark-active) 
      (shell-command-on-region
       (region-beginning) (region-end)
       "perltidy" nil t
       )
    (shell-command-on-region
     (point-min) (point-max)
     "perltidy" nil t
     ) 
    )
  )


(defun normaltabs()
  (interactive)
  (global-set-key (kbd "TAB") 'self-insert-command)
  )


(defun copy-rectangle ()
  (interactive)
  (kill-rectangle (region-beginning) (region-end))
  (exchange-point-and-mark)
  (yank-rectangle)
  )

(defun My-smart-home ()
  "Odd home to beginning of line, even home to beginning of text/code."
  (interactive)
  ;; the "^" is supposed to do something smart with shift-selection-mode
  ;; however, the shift-selection mode doesn't seem terribly smart
  ;;(interactive "^")
  (if (and (eq last-command 'My-smart-home)
           (/= (line-beginning-position) (point)))
      (beginning-of-line)
    (beginning-of-line-text))
  )

(define-abbrev-table 'cperl-mode-abbrev-table '(
						("perl" "#!/usr/bin/perl\n\nuse strict;\nuse warnings;\n" nil 1)
						))


(defalias 'perl-mode 'cperl-mode)

(defun alex-programming ()
  (local-set-key "\r" 'newline-and-indent)
  (local-set-key "\C-j" 'newline)
  (setq indent-tabs-mode nil)
  )


(add-hook 'c-mode-common-hook
	  '(lambda () 
	     (setq c-basic-offset 4
		   tab-width 8)
	     (setq-default indent-tabs-mode nil)
	     )
	  )

(add-hook 'c-mode-hook
	  (lambda ()
	    (alex-programming)
	    )
	  )

(add-hook 'c++-mode-hook
	  (lambda ()
	    (alex-programming)
	    )
	  )

(add-hook 'cperl-mode-hook
          (lambda ()
	    (alex-programming)
	    (setq cperl-close-paren-offset -4)
	    (setq cperl-continued-statement-offset 4)
	    (setq cperl-indent-level 4)
	    (setq cperl-indent-parens-as-block t)
	    (setq cperl-tab-always-indent t)
	    (setq cperl-electric-keywords t) 
	    (setq cperl-merge-trailing-else nil)
	    )
          )

(add-hook 'html-mode-hook
          (lambda()
            (global-set-key (kbd "C-c .") 'sgml-close-tag)
            (setq sgml-basic-offset 8)
            (setq indent-tabs-mode t)
	    )
	  )

(add-hook 'ruby-mode-hook (lambda() (alex-programming)))
(add-hook 'emacs-lisp-mode-hook (lambda() (alex-programming)))

;; Key bindings
;; (global-set-key (quote [S-backspace]) (quote kill-whole-line))
(global-set-key (quote [delete]) (quote delete-char))
(global-set-key (quote [S-delete]) (quote kill-whole-line))
(global-set-key (kbd "C-d") 'duplicate-line)
(global-set-key (kbd "C-b") 'cua-set-rectangle-mark)
(global-set-key (kbd "C-n") 'linum-mode)
(global-set-key (kbd "C-M-g") 'cua-toggle-global-mark)
(global-set-key (kbd "C-x r w") 'copy-rectangle)
(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [S-f1] 'cperl-mode)
(global-set-key [S-f2] 'cperl-mode)
(global-set-key [home] 'My-smart-home)
;;(global-set-key [S-f4] 'alex-unhighlight)
