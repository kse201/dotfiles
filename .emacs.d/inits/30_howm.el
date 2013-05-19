;; @ howm
(when (require 'howm-mode nil t)
  (mapc
   (lambda (f)
     (autoload f
       "howm" "Hitori Otegaru Wiki Modoki" t))
   '(howm-menu howm-list-all howm-list-recent
               howm-list-grep howm-create
               howm-keyword-to-kill-ring))
  
  ;; howmメモの保存先
  (setq howm-directory (concat user-emacs-directory "~/Dropbox/document/memo/howm"))
  ;; howm-memoを1日1ファイルに
  (setq howm-file-name-format "%Y/%m/%Y/-%m-%d.howm")
  ;; howm-menuの言語を日本語に
  (setq howm-menu-lang 'ja)
  (define-key global-map (kbd "C-x C-, C-, ") 'howm-menu)
  ;; 保存と同時に閉じる
  (defun howm-save-buffer-and-kill ()
    "howmメモを保存と同時に閉じる"
    (interactive)
    (when (and (buffer-file-name)
               (string-match "\\.howm" (buffer-name)))
      (save-buffer)
      (kill-buffer)))
  (define-key howm-mode-map (kbd "C-c C-c") 'howm-save-buffer-and-kill))

