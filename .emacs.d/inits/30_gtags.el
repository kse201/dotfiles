;;; @ gtags
(when (and
       (locate-library "gtags")
       (require 'gtags nil t))
  (setq gtags-suggested-key-mapping t)
  (global-set-key "\M-t" 'gtags-find-tag)     ;関数の定義元へ
  (global-set-key "\M-r" 'gtags-find-rtag)    ;関数の参照先へ
  (global-set-key "\M-s" 'gtags-find-symbol)  ;変数の定義元/参照先へ
  ;;(global-set-key "\M-p" 'gtags-find-pattern)
  (global-set-key "\M-f" 'gtags-find-file)    ;ファイルにジャンプ
  (global-set-key [?\C-,] 'gtags-pop-stack)   ;前のバッファに戻る
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (gtags-mode 1)
               (gtags-make-complete-list)))
  ;; update GTAGS
  ;;http://qiita.com/items/d9e686d2f2a092321e34
  (defun update-gtags (&optional prefix)
    "automatically Update gtags "
    (interactive "P")
    (let ((rootdir (gtags-get-rootpath))
          (args (if prefix "-v" "-iv")))
      (when rootdir
        (let* ((default-directory rootdir)
               (buffer (get-buffer-create "*update GTAGS*")))
          (save-excursion
            (set-buffer buffer)
            (erase-buffer)
            (let ((result (process-file "gtags" nil buffer nil args)))
              (if (= 0 result)
                  (message "GTAGS successfully updated.")
                (message "update GTAGS error with exit status %d" result))))))))

  (defun gtags-parse-file2 ()
    "Listup the methods of Current-Buffer."
    (interactive)
    (if (gtags-get-rootpath)
        (let*
            ((root (gtags-get-rootpath))
             (path (buffer-file-name))
             (gtags-path-style 'root)
             (gtags-rootdir root))
          (if (string-match (regexp-quote root) path)
              (gtags-goto-tag
               (replace-match "" t nil path)
               "f" nil)
            ;; delegate to gtags-parse-file
            (gtags-parse-file)))
      ;; delegate to gtags-parse-file
      (gtags-parse-file)))
  )
