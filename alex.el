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

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

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

(defun alex-append (start end)
  (interactive "r")
  (if (and transient-mark-mode mark-active)
      (let ((to-append (read-from-minibuffer "Append what: ")))
        (save-excursion
          (progn
            (goto-char end)
            (end-of-line)
            (if (> (point) end)
                (progn
                  (setq last-line (- (line-number-at-pos) 1))
                  (setq at-the-end nil)
                  )
              (progn
                ;; we are at the end of the region
                (setq last-line (line-number-at-pos))
                (setq at-the-end t)
                )
              )
            (goto-char start)
            (setq stop-here nil)
            (while (and (not stop-here)
                        (<= (line-number-at-pos) last-line))
              (end-of-line)
              (insert to-append)
              (if (eobp) (setq stop-here t) (next-line))
              )
            ) ;; progn
          ) ;; save-excursion
        (if at-the-end
            (end-of-line))
        ;;(message start)
        )
    (progn (message "No region selected") )
    )
  )

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

;; (defun cua-resize-rectangle-right (n)
;;     "Resize rectangle to the right."
;;     (interactive "p")
;;     (let ((resized (> n 0)))
;;       (while (> n 0)
;;         (setq n (1- n))
;;         (cond
;;          ((cua--rectangle-right-side)
;;           (cua--rectangle-right (1+ (cua--rectangle-right)))
;;           (move-to-column (cua--rectangle-right)))
;;          (t
;;           (cua--rectangle-left (1+ (cua--rectangle-left)))
;;           (move-to-column (cua--rectangle-right)))))
;;       (if resized
;;           (cua--rectangle-resized))
;;       )
;;     (if (= (current-column) (cua--rectangle-right))
;;         (move-to-column (+ (cua--rectangle-right) 1))
;;       )
;;     )

;; (defun cua-resize-rectangle-left (n)
;;     "Resize rectangle to the left."
;;     (interactive "p")
;;     (let (resized)
;;       (while (> n 0)
;;         (setq n (1- n))
;;         (if (or (= (cua--rectangle-right) 0)
;;                 (and (not (cua--rectangle-right-side))
;;                      (= (cua--rectangle-left) 0)))
;;             (setq n 0)
;;           (cond
;;            ((cua--rectangle-right-side)
;;             (cua--rectangle-right (1- (cua--rectangle-right)))
;;             (move-to-column (cua--rectangle-right)))
;;            (t
;;             (cua--rectangle-left (1- (cua--rectangle-left)))
;;             (move-to-column (cua--rectangle-right))))
;;           (setq resized t)))
;;       (if resized
;;           (cua--rectangle-resized)))
;;     (if (= (current-column) (cua--rectangle-right))
;;         (move-to-column (+ (cua--rectangle-right) 1))
;;       )
;;     )


;; This we need to do sometime...
;; (defun alex-delete-last-char ()
;;   "Delete the last char in the rectangle"
;;   (interactive)
;; )  

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

(defun mylisp () 
  (alex-programming)
  (global-set-key [f12] 'eval-last-sexp)
  (yas/minor-mode -1)
  (yas/global-mode -1)
)
(add-hook 'lisp-mode-hook (lambda() (mylisp)))
(add-hook 'lisp-interaction-mode-hook (lambda() (mylisp)))
(add-hook 'emacs-lisp-mode-hook (lambda() (mylisp)))

;; Key bindings
;; (global-set-key (quote [S-backspace]) (quote kill-whole-line))
(global-set-key (quote [delete]) (quote delete-char))
(global-set-key (quote [S-delete]) (quote kill-whole-line))
(global-set-key (kbd "C-d") 'duplicate-line)
(global-set-key (kbd "C-b") 'alex-cua-mods)
(global-set-key (kbd "C-n") 'linum-mode)
(global-set-key (kbd "C-M-g") 'cua-toggle-global-mark)
(global-set-key (kbd "C-x r w") 'copy-rectangle)
(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [f1] 'find-file)
(global-set-key [S-f1] 'cperl-mode)
(global-set-key [home] 'My-smart-home)
;;(global-set-key [S-f4] 'alex-unhighlight)
(global-set-key [f11] 'alex-append)

;; Now we have to do some magic
;; since cua-set-rectangle-mark calls
;; cua--init-rectangles, and this screws up my defuns,
;; we have to make sure my defuns are in place

(defun alex-cua-mods ()
  (interactive)
  (cua-set-rectangle-mark)
  (defun cua-insert-char-rectangle (&optional ch)
    (interactive)
    (if buffer-read-only
        (ding)
      (cua--indent-rectangle (or ch (aref (this-single-command-keys) 0)))
      (if (and (= (current-column) (cua--rectangle-right))
               (not (= (cua--rectangle-left) (cua--rectangle-right))))
          (cua-resize-rectangle-right 1))
      (cua--keep-active))
    t)
  (defun cua--rectangle-resized ()
    ;; Refresh state after resizing rectangle
    (setq cua--buffer-and-point-before-command nil)
    (cua--rectangle-insert-col 0)
    (cua--rectangle-set-corners)
    (cua--keep-active)
    (if (= (current-column) (cua--rectangle-right))
        (move-to-column (+ (cua--rectangle-right) 1))
      )
    )
  (global-set-key (kbd "C-b") 'cua-set-rectangle-mark)
  )
(setq mouse-yank-at-point t)
(setq mouse-autoselect-window t)
