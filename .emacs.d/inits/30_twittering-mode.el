;;; @ twittering-mode
(when (require 'twittering-mode nil t)
  ;; 起動時パスワード認証 *要 gpgコマンド
  (setq twittering-use-master-password t)
  ;; パスワード暗号ファイル保存先変更 (デフォはホームディレクトリ)
  ;(setq twittering-private-info-file "~/.emacs.d/twittering-mode.gpg")

  ;; 表示する書式 区切り線いれたら見やすい
  (setq twittering-status-format "%i @%s %p [%@]%r %R %f%L:\n %T\n-------------------------------------------")
  ;; アイコンを表示する
  (setq twittering-icon-mode t)
  ;; アイコンサイズを変更する *48以外を希望する場合 要 imagemagickコマンド
  ;(setq twittering-convert-fix-size 40)
  ;; 更新の頻度(秒)
  (setq twittering-timer-interval 40)
  ;; ツイート取得数
  (setq twittering-number-of-tweets-on-retrieval 50)
  ;; o で次のURLをブラウザでオープン
  (add-hook 'twittering-mode-hook
            (lambda ()
              (local-set-key (kbd "o")
                             (lambda ()
                               (interactive)
                               (twittering-goto-next-uri)
                               (execute-kbd-macro (kbd "C-m"))))))
  (add-to-list 'exec-path "/usr/local/bin")
  ;; 起動時に開くタイムラインの設定
  ;; http://christina04.blog.fc2.com/blog-entry-175.html
  (setq twittering-initial-timeline-spec-string
        '(":home"
          ":retweeted_to_me"
          "nim03027/real"))
  ;;; bitly
  (setq twittering-tinyurl-service 'bit.ly)
  (setq twittering-bitly-login "o_2qpahq4o2g")
  (setq twittering-bitly-api-key "R_569bbc2f545c7bc590b6d3b35c0554e3")
  ;; 実行キー追加 デフォルトは[f4]キー
  ;;(global-set-key (kbd "C-c t u") 'twittering-tinyurl-replace-at-point)

  ;;http://qiita.com/items/8e0007dc76f5f14cb2aa
  ;;;;; 新たなタイムラインを表示する前に，現在のタイムラインのバッファを消去
  (defadvice twittering-visit-timeline (before kill-buffer-before-visit-timeline activate)
             "Kill TL buffer before visit new TL."
             (twittering-kill-buffer))


  ;;; f / g キーでアクセスするタイムラインリスト
  (setq twittering-initial-timeline-spec-string
        '(":mentions"
          ":home"))

  ;;; 次のタイムラインリストを表示
  (defun twittering-kill-and-switch-to-next-timeline ()
    (interactive)
    (when (twittering-buffer-p)
      (let* ((buffer-list twittering-initial-timeline-spec-string)
             (following-buffers (cdr (member (buffer-name (current-buffer)) buffer-list)))
             (next (if following-buffers
                     (car following-buffers)
                     (car buffer-list))))
        (unless (eq (current-buffer) next)
          (twittering-visit-timeline next)))))

  ;;; 前のタイムラインリストを表示
  (defun twittering-kill-and-switch-to-previous-timeline ()
    (interactive)
    (when (twittering-buffer-p)
      (let* ((buffer-list (reverse twittering-initial-timeline-spec-string))
             (preceding-buffers (cdr (member (buffer-name (current-buffer)) buffer-list)))
             (previous (if preceding-buffers
                         (car preceding-buffers)
                         (car buffer-list))))
        (unless (eq (current-buffer) previous)
          (twittering-visit-timeline previous)))))

  ;;; キーの設定
  (defun twittering-mode-hooks ()
    (define-key twittering-mode-map (kbd "f") 'twittering-kill-and-switch-to-next-timeline)
    (define-key twittering-mode-map (kbd "b") 'twittering-kill-and-switch-to-previous-timeline))

  (add-hook 'twittering-mode-hook 'twittering-mode-hooks)
  )
