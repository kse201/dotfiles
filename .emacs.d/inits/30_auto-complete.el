;;; @ auto-complete
(when (require 'auto-complete nil t)
  (require 'popup nil t)
  (require 'fuzzy nil t)
  (setq popup-use-optimized-column-computation nil)
  (setq ac-ignore-case 'smart)               ; 大文字が含まれている場合のみ、大文字/小文字を区別する
  ;;  (ac-set-trigger-key "TAB")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (setq ac-use-menu-map t)
  (setq ac-auto-start t)
  (global-auto-complete-mode 1)
  (setq ac-modes (cons 'js-mode ac-modes))
  ;; emacs-lisp-modeでEmacs Lispシンボルも補完してくれるようにする
  (defun emacs-lisp-ac-setup ()
    (setq ac-sources '(ac-source-words-in-same-mode-buffers
                       ac-source-symbols)))
  (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-ac-setup)
  (when (require 'auto-complete-config nil t)
    (add-to-list 'ac-dictionary-directories "~/.emacd.d/ac-dict")
    (setq ac-ignore-case t)
    (ac-config-default)                     ; デフォルト設定
    (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
    (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
    (auto-complete-mode t)))
