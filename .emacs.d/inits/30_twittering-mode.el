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
  )
