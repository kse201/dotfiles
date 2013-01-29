;;; 追加elisp関連
(require 'cl)
;; @ package
(when
    (if (string-match "^23\." emacs-version)
        (require 'package-23 nil t)
      (require 'package-24 nil t))
  
  ;; バッケージリポジトリにMarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))
(require 'melpa)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; 起動時にEmacsWikiのページ名を補完候補に加える
            (auto-install-update-emacswiki-package-name t)
            (auto-install-compatibility-setup)))

                                        ;(add-to-list 'load-path "~/src/emacswikipages/" t)

;; 履歴を次回Emacs起動時にも保存する
(require 'saveplace)
(savehist-mode t)

;; 最近使ったファイルに加えないファイルを正規表現で指定する
(setq recentf-exclude '("/TAGS$" "/var/tmp/"))

;; http://d.hatena.ne.jp/tomoya/20110217/1297928222
(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1)
  (require 'recentf-ext))

(when (require 'auto-async-byte-compile nil t)
  ;; 自動バイトコンパイルを無効にするファイル名の正規表現
  (setq auto-async-byte-compile-exclude-files-regexp "init.el")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;;; --------------------------------------------------
;;; @ emacs-lisp

;;; @ autoinsert
(auto-insert-mode t)
(setq auto-insert-directory "~/.emacs.d/templete")
(define-auto-insert "\\.rb$" "template.rb")

;;; @ minor-mode-hack
;;; マイナーモード衝突問題を解決する
(when (require 'minor-mode-hack nil t))

;;; http://e-arrows.sakura.ne.jp/2010/02/vim-to-emacs.html
;; org-mode
;; Emacsでメモ・TODO管理
(when  (require 'org-install nil t)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cr" 'org-remember)
  (setq org-startup-truncated nil)
  (setq org-return-follows-link t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (org-remember-insinuate)
  (setq org-directory "~/Dropbox/documents/memo/")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-agenda-files (concat org-directory "notes.org"))
  (setq org-remember-templates
        '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
          ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
          ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas"))))

;;; @ color-moccur 検索結果のリストアップ
(when (require 'color-moccur nil t)
  (global-set-key (kbd "M-o") 'occur-by-moccur)
  ;; スペース区切りでAND検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索のとき除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  ;; MMigemoを利用できる環境であればMigemoを使う
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t ))
    (setq cmigemo-command "cmigemo") ; cmigemoを使う
    (setq moccur-use-migemo t)))

;;; @ grep結果バッファでのカーソル移動でダイナミックにファイルを開いてくれる
(when (require 'color-grep nil t)
  (setq color-grep-sync-kill-buffer t)
  ;; M-x grep-findでPerlのackコマンドを使うよう変更
  (setq grep-find-command "ack --nocolor --nogroup "))

;;; @ undohist 編集履歴の記憶
(when (require 'undohist nil t)
  (undohist-initialize))

;;; @ undo-tree モードの設定
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

(when (require 'auto-save-buffers nil t)
  (run-with-idle-timer 5 t 'auto-save-buffers))


;;; @ screen-lines 物理行で移動
(when (require 'screen-lines)
  (add-hook 'text-mode-hook 'turn-on-screen-lines-mode))

;;; @ text-adjust 日本語の文章を整形する
(when (require 'text-adjust nil t))

;;; @ multi-shell
(when (require 'multi-shell nil t)
  (setq multi-shell-command "/bin/zsh"))

;; @ yasnippet
(when (require 'yasnippet-config nil t)
  (yas/setup "~/.emacs.d/elisp/yasnippet-0.6.1c"))

(when (require 'redo+ nil t)
  (global-set-key (kbd  "C-.") 'redo))

;; http://d.hatena.ne.jp/sandai/20120303/p1
;; カーソル付近にあるEmacs Lispの関数や変数のヘルプをエコーエリアに表示
;; http://www.emacswiki.org/emacs/eldoc-extension.el
(when (require 'eldoc nil t)
  (require 'eldoc-extension nil t)
  (defun elisp-mode-hooks ()
    ;;  "lisp-mode-hooks"
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-area-use-multiline-p t)
    (turn-on-eldoc-mode))
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-minor-mode-string "")
  (add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks))

;;;  @ C-eldoc
;; C言語の関数や変数のヘルプをエコーエリアに表示
(when (require 'c-eldoc nil t)
  (add-hook 'c-mode-hook
            (lambda ()
              (set (make-local-variable 'eldoc-idle-delay) 0.2)
              (set (make-local-variable 'eldoc-minor-mode-string) "")
              (c-turn-on-eldoc-mode))))

(require 'hideshow nil t)

;; @ e2wm
;; http://d.hatena.ne.jp/kiwanami/20100528/1275038929
(when (require 'e2wm nil t)
  (global-set-key (kbd "M-+") 'e2wm:start-management)
  (setq e2wm:c-code-recipe
        '(| (:left-max-size 12)
            (- (:upper-size-ratio 0.7)
               files history)
            (- (:upper-size-ratio 0.7)
               (| (:right-max-size 12)
                  main imenu)
               sub)))
  (setq e2wm:c-code-winfo
        '((:name main)
          (:name files :plugin files)
          (:name history :plugin history-list)
          (:name sub :buffer "*info*" :default-hide t)
          (:name imenu :plugin imenu :default-hide nil))))

;; @ w3m
(when (require 'w3m nil t)
  (require 'w3m-load nil t)
  (setq w3m-use-cookies t)              ;クッキーを使う
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-key-binding 'info)
                                        ;(global-set-key (kbd "C-x C-b") 'bs-show)
  ;; URIらしき部分を選択してC-xmするとemacs-w3m起動
  (global-set-key "\C-xm" 'browse-url-at-point)
  ;; 検索の設定 M-x w3m-search
  (autoload 'w3m-search "w3m-search" "Search QUERY using SEARCH-ENGINE." t)
  ;; 検索をGoogle(日本語サイト)でおこなう
  (setq w3m-search-default-engine "google")
  ;; C-csを押下するとどのBufferからでも検索を開始
  (global-set-key "\C-cs" 'w3m-search)
  (autoload 'w3m-weather "w3m-weather" "Display weather report." t)
  (autoload 'w3m-antenna "w3m-antenna" "Report chenge of WEB sites." t)
  (autoload 'w3m-namazu "w3m-namazu" "Search files with Namazu." t)
                                        ;formに入力可能とする。今は不要かもしれない
  (setq w3m-use-form t)
                                        ;うまく起動しない場合以下を設定してみるとよい
                                        ;(setq w3m-command "/usr/local/bin/w3m")
                                        ;初期起動時に表示する画面
  (setq w3m-home-page "http://www.google.co.jp")
  (setq w3m-default-display-inline-images t)       ;画像を表示する

  ;; http://mugijiru.seesaa.net/article/258382587.html
  (defun w3m-play-movie ()
    (interactive)
    (let ((url (w3m-anchor)))
      (cond ((string-match "^http:\\/\\/www\\.youtube\\.com" url)
             (message (concat "loading from youtube... " url))
             (call-process "play-youtube" nil nil nil url))
            ((string-match (concat "^http.*" (regexp-opt '(".mpg" ".wmv" ".avi" ".flv")) "$") url)
             (call-process "mplayer" nil nil nil "-fs" url))
            (t
             (message "not movie."))))))

;;; ruby
(require 'ruby-electric nil t)          ; 括弧の自動挿入
(when (require 'ruby-block nil t)       ; end に対応する行のハイライト
  (setq ruby-block-highlight nil))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")





;; http://tcnksm.sakura.ne.jp/blog/2012/05/07/emacs-%E3%81%A7-ruby-%E3%81%AE%E5%85%A5%E5%8A%9B%E8%87%AA%E5%8B%95%E8%A3%9C%E5%AE%8C%E3%81%A8%E3%83%AA%E3%83%95%E3%82%A1%E3%83%AC%E3%83%B3%E3%82%B9%E3%81%AE%E8%A1%A8%E7%A4%BA/
((when require 'rsense nil t)
(setq rsense-home "~/.emacs.d/opt/rsense-0.3")
(add-to-list 'load-path (concat rsense-home "/etc"))
(add-hook 'ruby-mode-hook
          '(lambda ()
             ;; .や::を入力直後から補完開始
             (add-to-list 'ac-sources 'ac-source-rsense-method)
             (add-to-list 'ac-sources 'ac-source-rsense-constant)
             ;; C-x .で補完出来るようキーを設定
             (define-key ruby-mode-map (kbd "C-x .") 'ac-complete-rsense)))

;; ruby-mode-hook用の関数を定義
(defun ruby-mode-hooks ()
  (inf-ruby-keys)
  (ruby-electric-mode t)
  (ruby-block-mode t))
(add-hook 'ruby-mode-hook 'ruby-mode-hooks) ; ruby-mode-hookに追加
)
;; 再帰的にgrep
;; http://www.clear-code.com/blog/2011/2/16.html
(require 'grep nil t)
(setq grep-command-before-query "grep -nH -r -e ")
(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
             (concat grep-command-before-query
                     (shell-quote-argument (grep-tag-default)))))
        (cons (if buffer-file-name
                  (concat grep-command-before-target
                          " *."
                          (file-name-extension buffer-file-name))
                (concat grep-command-before-target " ."))
              (+ (length grep-command-before-target) 1)))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
                         (+ (length grep-command-before-query) 1)))

;; diredを便利に
(require 'dired-x nil t)
;; diredから"r"でファイル名をインライン編集する
(require 'wdired nil t)
(define-key dired-mode-map "r" 'wdir3ed-change-to-wdired-mode)

;; ファイル名が重複していたらディレクトリ名を追加
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

(when (require 'navi2ch nil t)
  ;; レスをすべて表示する
  (setq navi2ch-article-exist-message-range '(1 . 1000)) ;既存スレ
  (setq navi2ch-article-new-message-range '(1000 . 1))   ;新スレ
  ;; Boardモードのレス数欄にレスの増加数を表示する
  (setq  navi-board-insert-subject-with-diff t)
  ;; Boardモードのレス数欄にレスの未読数を表示する
  (setq navi2ch-board-insert-subject-with-unread t)
  ;; 板一覧のカテゴリをデフォルトですべて開いて表示する
  (setq navi2ch-list-init-open-category nil)
  ;; スレをexpire(削除)しない
  (setq navi2ch-board-expire-date nil)
  ;; 履歴の行数を制限しない
  (setq navi2ch-history-max-line nil))

;;; @ paredit.el
(when (require 'paredit nil t)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'c-mode-hook 'enable-paredit-mode))

;;; @ color-theme (emacs 23)
(if (string-match "^23\." emacs-version)
    (when (require 'color-theme nil t)
      (color-theme-xemacs)
      ))

;;; @ newsticker
(when (require 'newsticker nil t)
  (autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
  (autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t))

 ;;; @ rainbow-delimiters
(when (require 'rainbow-delimiters nil t)
  (global-rainbow-delimiters-mode t)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode))

;;; @ smartchr.el
;; http://tech.kayac.com/archive/emacs-tips-smartchr.html
(require 'smartchr nil t)
(global-set-key (kbd "=") (smartchr '(" = "  " == " "=")))

;;; @ C/C++
(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; センテンスの終了である ';' を入力したら、自動改行+インデント
             (c-toggle-auto-hungry-state 1)
             ;; RET キーで自動改行+インデント
             (define-key c-mode-base-map "\C-m" 'newline-and-indent)))

  ;;; - リスト11 kill-lineで行が連結したときにインデントを減らす
(defadvice kill-line (before kill-line-and-fixup activate)
  (when (and (not (bolp)) (eolp))
    (forward-char)
    (fixup-whitespace)
    (backward-char)))

;;; @ bongo
;;; http://pastelwill.jp/wiki/doku.php?id=emacs#bongo_itunes_の代わりに_emacs_を使う
(add-to-load-path "elisp/bongo/")
(when (require 'bongo-mplayer nil t)
  (autoload 'bongo "bongo"
    "Start Bongo by switching to a Bongo buffer." t)
  (defun load-path-setter (path-list target-path)
    (dolist (x path-list) (add-to-list target-path x)))
  (load-path-setter
   '("/usr/local/Cellar/mplayer/1.1/bin"
     "/Applications/VLC.app/Contents/MacOS")
   'exec-path)
  (setq bongo-enabled-backends '(mplayer)))

  ;;; 日本語マニュアル
(add-to-list 'Info-directory-list "~/.emacs.d/info")

;;; @ popwin
(setq pop-up-windows t)
(when (require 'popwin nil t)
  (setq anything-samewindow nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-height 0.4)
  (setq popwin:popup-window-position 'bottom)
  (push '("anything" :regexp t :height 40) popwin:special-display-config ) ;anything
  (push '("*Completions*" ) popwin:special-display-config )
  (push '("*complilation*" :noselect t :stick t ) popwin:special-display-config )
  (push '(dired-mode :position top) popwin:special-display-config ) ;dired
  (push '("*Backtrace*" :noselect t) popwin:special-display-config )
  (push '(fundamental-mode  :noselect t) popwin:special-display-config )
  (push '(typeset-mode :noselect t) popwin:special-display-config )
  (push '(" *auto-async-byte-compile*"  :position bottom :noselect t :height 0.1 :stick nil) popwin:special-display-config )
  (push '("*YaTeX-typesetting*" :position bottom :noselect t) popwin:special-display-config )
  (push '("*VC-log*" :position bottom) popwin:special-display-config )
  )

;;; @ egg
;;(when (executable-find "git")
;;  (require 'egg nil t))

;;; @ time-stamp
(when (require 'time-stamp nil t)
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-active t)
  (setq time-stamp-start "Last Change: " )
  (setq time-stamp-format "%02d-%3b-%04y.")
  (setq time-stamp-end " \\|$"))

;;; @ foreign-regexp
(when (require 'foreign-regexp nil t)
  (custom-set-variables
   ;; 正規表現、perlかrubyを選択
   '(foreign-regexp/regexp-type 'perl) ;; Choose by your preference.
   '(reb-re-syntax 'foreign-regexp)) ;; Tell re-builder to use foreign regexp.
  )

;; ブロックの折畳みと展開
;; http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/fold-dwim.el
(when (require 'fold-dwim nil t)
  (require 'hideshow nil t)
  ;; 機能を利用するメジャーモード一覧
  (let ((hook))
    (dolist (hook
             '(emacs-lisp-mode-hook
               c-mode-common-hook
               python-mode-hook
               php-mode-hook
               ruby-mode-hook
               js2-mode-hook
               css-mode-hook
               apples-mode-hook))
      (add-hook hook 'hs-minor-mode))))
