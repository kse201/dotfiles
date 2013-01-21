;; @ anything
(when (require 'anything nil t)
  (setq
   anything-idle-delay 0.3
   anything-input-idle-delay 0.2
   anything-candidate-number-limit 10000
   anything-quick-update t
   anything-enable-shortcuts 'alphabet)
  
  (when (require 'anything-config nil t)
    (setq anything-su-or-sudo "sudo")
    ;; anything関連キーバインド
    (define-many-keys global-map
      '(( "M-y" . anything-show-kill-ring)
        ( "C-;" . anything-M-x)
        ( "C-x C-b" . anything-for-files)
        ( "C-x C-f" . anything-find-files)
        ("C-x g" . anything-imenu)
        ("TAB" . anything-lisp-completion-at-point-or-indent)
        ("C-x C-o" . other-window)))
    (define-many-keys anything-map
      '(("C-z" . nil)
        ("C-w" . anything-execute-persistent-action)
        ("C-o" . nil)
        ("C-M-n" . anything-next-source)
        ("C-M-p" . anything-previous-source)))

    (require 'anything-match-plugin nil t)
    (defun anything-custom-filelist ()
      (interactive)
      (anything-other-buffer
       (append
        '(anything-c-source-ffap-line
          anything-c-source-ffap-guesser
          anything-c-source-buffers+
          )
        (anything-c-sources-git-project-for)
        '(anything-c-source-recentf
          anything-c-source-bookmarks
          anything-c-source-file-cache
          anything-c-source-filelist
          ))
       "*anything file list*"))
    )

  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))
  (require 'anything-show-completion nil t)
  ;; ajc-java-complete.el
  (when (require 'ajc-java-complete-config nil t)
    (add-hook 'java-mode-hook 'ajc-java-complete-mode))

  (when (require 'anything-c-moccur nil t)
    (setq
     anything-c-moccur-anything-idle-delay 0.1
     anything-c-moccur-higligt-info-line-flag t ; バッファの情報をハイライトする
     anything-c-moccur-enable-auto-look-flag t  ;選択中の候補の位置を他のwindowに表示する
     anything-c-moccur-enable-initial-pattern nil) ;起動時にポイントの位置の単語を初期パターンにしない
    ;; C-M-o にanything-c-moccur-occur-by-moccurを割り当てる
    (global-set-key (kbd "C-M-o") 'anything-c-moccur-occur-by-moccur)

    ;; http://d.hatena.ne.jp/kitokitoki/20100626/p1
    (defvar anything-cycle-task-count 0)

    ;; fixme-mode.el と ik:anything-cycle-pattern を参考にした
    (defun anything-cycle-task ()
          ;; 全バッファからTODO等を一覧表示する
      (interactive)
      (let ((los '("\\_<todo\\_>"
                   "\\_<kludge\\_>"
                   "\\_<fixme\\_>"
                   "\\_<bug\\_>"
                   "\\_<todo\\_>\\|\\_<fixme\\_>\\|\\_<bug\\_>\\|\\_<kludge\\_>")))
        (if (eq this-command real-last-command)
            (incf anything-cycle-task-count)
          (setq anything-cycle-task-count 0))
        (when (>= anything-cycle-task-count (length los))
          (setq anything-cycle-task-count 0))
        (delete-minibuffer-contents)
        (let ((sep (nth anything-cycle-task-count los)))
          (insert sep))))
    ;; anythign-c-moccur-buffer-list 中に T で切り替え
    (define-key anything-c-moccur-anything-map (kbd "T") 'anything-cycle-task)
    )
  ;; @ anything-auto-install
  (require 'anything-auto-install nil t)
  
  ;; @ anything-gtags
  (when (and (require 'anything-exuberant-ctags nil t)
             (require 'anything-gtags nil t))
    ;; anything-gtags-for-tags のソースを定義
    (setq anything-for-tags
          (list anything-c-source-imenu
                anything-c-source-gtags-select
                ;; etagsを利用する場合ははコメントを外す
                ;; anything-c-source-etags-select
                anything-c-source-exuberant-ctags-select
                ))
    ;; anything-for-tagsコマンドを作製
    (defun anything-for-tags ()
      "Preconfigured `anything' for anything-for-tags."
      (interactive)
      (anything anything-for-tags
                (thing-at-point 'symbol)
                nil nil nil "*anything for tags*"))
    ;; M-tにanything-for-tagsを割り当て
    (global-set-key (kbd "M-t") 'anything-for-tags))

  (when (require 'anything-complete nil t)
    (anything-lisp-complete-symbol-set-timer 150))

  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install))
  
  ;; anything でPDFを閲覧
  (setq abks:books-dir "/home/foo/bar/pdf-files") ; PDFファイルのあるディレクトリ（★必須）
  (setq abks:open-command "acroread") ; LinuxのAdobeReaderを使う (default)
  
  ;; for evince setting (default)
  (setq abks:cache-pixel "600")
  (setq abks:mkcover-cmd-pdf-postfix nil)
  (setq abks:mkcover-cmd '("evince-thumbnailer" "-s" size pdf jpeg))

  ;; for ImageMagick and GhostScript setting
  ;; (setq abks:cache-pixel "600x600")
  ;; (setq abks:mkcover-cmd-pdf-postfix "[0]")
  ;; (setq abks:mkcover-cmd '("convert" "-resize" size pdf jpeg))
  
  (global-set-key (kbd "M-8") 'anything-books-command) ; キーバインド
  ;; Emacs でPDFを読む
  (defadvice abks:open-file (around my-abks:open-file activate)
    (if (require 'doc-view  nil t)
        (find-file (ad-get-arg 0))
      ad-do-it))
  (add-hook 'view-mode-hook
            (lambda ()
              (when (eql major-mode 'doc-view-mode)
                (define-key view-mode-map "-" nil)
                (define-key view-mode-map "n" nil)
                (define-key view-mode-map "p" nil))))
  (setq anything-for-document-source
        (list anything-c-source-man-pages
              anything-c-source-info-cl
              anything-c-source-info-pages
              anything-c-source-info-elisp
              anything-c-source-apropos-emacs-commands
              anything-c-source-apropos-emacs-functions
              anything-c-source-apropos-emacs-variables))
  ;; anything-for-documentコマンドを作成
  (defun anything-for-document ()
    "Preconficured `anything' for anything-for-document."
    (interactive)
    (anything anything-for-document-source
              (thing-at-point 'symbol) nil nil nil
              "*anything for document*"))
  ;; man パスを設定
  (setq woman-manpath '("/usr/share/man"
                        "/usr/loca/share/man"
                        "/usr/local/share/man/ja"
                        "/usr/local/Cellar/"))
  (setq woman-cache-filename "~/.emacs.d/.wmncach.el") ;キャッシュ生成
  )

(add-to-list 'anything-sources 'anything-c-source-emacs-commands)

