;; init.el
;; Date:

;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;; auto-installによってインストールされるEmacs Lispをロードパスに加える
;; デフォルトは、~/.emacs.d/auto-install/
(add-to-list 'load-path "~/.emacs.d/auto-install/")
(add-to-list 'load-path "~/.emacs.d/elisp/")

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (require 'auto-install nil t)
              (setq auto-install-directory "~/.emacs.d/elisp/")
              
              ;; install-elisp.el互換モードにする
              (auto-install-compatibility-setup)
              
              ;; proxy setting
              ;; 参考: http://e-arrows.sakura.ne.jp/2010/12/emacs-anywhere.html
              (defun machine-ip-address (dev)
                "Return IP address of a network device."
                (let ((info (network-interface-info dev)))
                  (if info
                      (format-network-address (car info) t))))
              
              (defvar *network-interface-names* '("en1" "wlan0")
                "Candidates for the network devices.")
              
              (defun officep ()
                "Am I in the office? If I am in the office, my IP address must start with '10.0.100.'."
                6    (let ((ip (some #'machine-ip-address *network-interface-names*)))
                       (and ip
                            (eq 0 (string-match "^172\\.16\\.1\\." ip)))))
              
              (if (officep)
                  (setq url-proxy-services '(("http" . "172.16.1.1:3128")))
                (setq w3m-command-arguments
                      (nconc w3m-command-arguments
                             '("-o" "http_proxy=http://172.16.1.1:3128/")))
                )
              ;; 起動時にEmacsWikiのページ名を補完候補に加える
              (auto-install-update-emacswiki-package-name t)
              (auto-install-cmpatibility-setup))

            ;; package.el
            (when (require 'package nil t) 
              ;; バッケージリポジトリにMarmaladeと開発者運営のELPAを追加
              (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) 
              (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
              (package-initialize))))

(add-to-list 'load-path "~/src/emacswikipages/" t)

;; 設定ファイル編集 
(defun edit-init.el ()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )
(global-set-key (kbd "C-x C-i") 'edit-init.el)
;; ediff関連のバッファを1つのフレームにまとめる
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; 現在行に色をつける
(global-hl-line-mode 1)
;; 履歴を次回Emacs起動時にも保存する
(savehist-mode 1)
(require 'saveplace )
;; <C-h>を後退に割り当てる
(global-set-key (kbd "C-h") 'delete-backward-char)
;; モードラインに時刻を表示する
(display-time)
;; カーソルの点滅を止める
(blink-cursor-mode 0)
;; 評価した結果を全部憑依
(setq eval-comment-region nil)
;; 行番号・桁番号を表示する
(line-number-mode 1)
;; リージョンに色をつける
(transient-mark-mode 1)
;; GCを減らして軽くする (デフォルトの10倍)
(setq gc-cons-threshold (* gc-cons-threshold))
;; ログの記録行数を増やす
(setq message-log-max 10000)
;; スクロールバーを右側に
(set-scroll-bar-mode 'right)
;; 現在の関数名をモードラインに
(which-function-mode 1)

;; zsh を使う
(setq shell-file-name "/bin/zsh")
(setq enable-recursice-minibuffers t)
;; ダイアログボックスを使わないようにする
(setq use-dialog-box nil)
(defalias 'message-box 'message)
;; 履歴をたくさん保存する
(setq history-length 10000)
;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.1)
;; 大きいファイルを開こうとしたときに警告を発生させる
;; デフォルトは10MBなので25MBに拡張する
(setq large-file-worning-threshold (* 25 1024 1024))
;; ミニバッファで入力を取り消しても履歴に残す
;; 誤って取り消して入力が失われるのを防ぐため
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))
;; yesは面倒なのでyで十分
(defalias 'yes-or-no-p 'y-or-n-p)
;; もろもろ非表示
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

;; 最近のファイル500個個を保存する
(setq recentf-max-saved-items 500)

;; 最近使ったファイルに加えないファイルを正規表現で指定する
(setq recentf-exclude '("/TAGS$" "/var/tmp/"))
(require 'recentf-ext)
;; This was installed by package-install.el.
;; This provides support for the package system and
;; interfacing with ELPA, the package archive.
;; Move this code earlier if you want to reference
;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(require 'auto-async-byte-compile)
;; 自動バイトコンパイルを無効にするファイル名の正規表現
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
(setq auto-async-byte-compile-exclude-files-regexp "^_")
;; *.elを保存時、自動バイトコンパイル
                                        ;(add-hook 'after-save-hook              
                                        ;         (lambda ()
                                        ;          (let ((file (buffer-file-name)))
                                        ;           (when (string-match ".*\\.el$" file)
                                        ;            (byte-compile-file file))))

;; recentf-ext.el
                                        ;(global-set-key (kbd "C-;") 'recentf-open-files )

;; キーバインド
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-?") 'help-for-help)
(global-set-key (kbd "C-z") 'undo)                 ; undo
;(global-set-key (kbd "C-c i") 'indent-region)       ; インデント
;(global-set-key (kbd "C-c C-i") 'dabbrev-expand)   ; 補完
;(global-set-key (kbd "C-c ;") 'comment-region)      ; コメントアウト
;(global-set-key (kbd "C-c :") 'uncomment-region)   ; コメント解除
(global-set-key "\C-\\" nil) ; \C-\の日本語入力の設定を無効にする
(global-set-key "\C-c" 'other-frame)         ; フレーム
(global-set-key (kbd "C-m") 'newline-and-indent)
;(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
(global-set-key (kbd "C-x C-o") 'my-other-window)
(global-set-key (kbd "M-a") 'mark-whole-buffer)
(global-set-key (kbd "M-y") 'backward-kill-word) ; 一つ前の単語削除
(global-set-key (kbd "C-x o") 'browse-url-at-point) ;ブラウザ起動
(global-set-key (kbd "C-x C-g") 'goto-line) ; 指定行へ移動

;; Localeに合わせた環境の設定
(set-locale-environment nil)
;; 対応する括弧を光らせる
(require 'paren)
(show-paren-mode 1)
;; 画面内に収まらない時は括弧内も光らせる
(setq show-paren-style 'mixed)
(make-face 'paren-mismatch)
(set-face-foreground 'paren-mismatch "white")
(set-face-background 'paren-mismatch "lightcoral")
(setq show-paren-face  'paren-match)
(setq show-paren-mismatch-face 'paren-mismatch)
;; 強力な補完機能を使う
;; (partial-completion-mode 1)
;; 最終更新日の自動挿入
(require 'time-stamp)
                                        ;画像ファイルを表示する
(auto-image-file-mode t)
;; 自動でファイルを挿入する
(auto-insert-mode t)
;; C-x,bでバッファリストをミニバッファに表示する
(iswitchb-mode 1)
;; C-x b でbuffersを選ぶとき便利
(if (string-match "23" emacs-version)
    (iswitchb-default-keybindings))

(defface my-hl-line-face
  ;; 背景がdarkならば背景色を紺に
  '((((class clolor) (background dark))
     (:background "NavyBlue" t))
    ;; 背景がlightならば背景色を緑に
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; Shift + 矢印で範囲選択
(if (string-match "23" emacs-version)
    (pc-selection-mode))
;; 選択範囲に色をつけて見た目をわかりやすく
(transient-mark-mode 1)
;; フォント設定
(set-face-attribute
 'default nil 
 :family "Ricty"
 :height 140)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec 
  :family "Ricty"))

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")
;; 行番号表示
(global-linum-mode t)
;; TABの表示幅 4
(setq-default tab-width 4)
;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)
;; テーマ読み込み設定
(when (require 'color-theme nil t)
  (color-theme-initialize))

;; 対応括弧のハイライト
(setq show-paren-delay 0) ; 表示までの秒数 
(show-paren-mode t)
(setq shwo-paren-style 'expression)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "red")

;; anything
(when (require 'anything nil t)
  (setq
   anything-idle-delay 0.3
   anything-input-idle-delay 0.2
   anything-candidate-number-limit 10000
   anything-quick-update t
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    (setq anything-su-or-sudo "sudo")
    (global-set-key (kbd "M-y") 'anything-show-kill-ring)
    (require 'anything-match-plugin nil t))

  (when (and ( executable-find "cmigemo")
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
     anything-c-moccur-enable-initial-pattern t) ;起動時にポイントの位置の単語を初期パターンにする
    ;; C-M-o にanything-c-moccur-occur-by-moccurを割り当てる
    (global-set-key (kbd "C-M-o") 'anything-c-moccur-occur-by-moccur))
  (when (require 'auto-install nil t)
    (require 'anything-auto-install))
  (when (require 'anything-complete nil t)
    (anything-lisp-complete-symbol-set-timer 150))
  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install)))


(add-to-list 'anything-sources 'anything-c-source-emacs-commands)
(global-set-key (kbd "C-; C-;") 'anything-M-x)
(global-set-key (kbd "C-; C-b") 'anything-buffers-list) ;; バッファ一覧

;; 日本語マニュアル
(add-to-list 'Info-directory-list "~/.emacs.d/info")

;; リージョン内の行数と文字数をモードラインに表示する (範囲指定字のみ)
;; http://d.hatena.nejp/sonota88/20110224/
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ))
(add-to-list 'default-mode-line-format 
             '(:eval (count-lines-and-chars)))

(when (eq system-type 'darwin)
  (setq mac-command-key-is-meta nil)    ;コマンドキーをメタにしない
  (setq mac-potion-modifier 'meta)      ; Optionをメタに
  ;;  (setq mac-command-modifier 'super)    ; コマンドをSuperに
  (setq mac-pass-control-to-system t))   ; コントロールキーをMacではなくEmacsに渡す

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(eval-when-compile
  (require 'cl))

;; auto-complete
;; 補完候補を自動ポップアップ
(when (require 'auto-complete nil t)
  (global-auto-complete-mode t)
  (setq ac-modes (cons 'js-mode ac-modes)))

(when (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacd.d/ac-dict")
  (ac-config-default)                     ; デフォルト設定 
  (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
  (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
  (ac-mode)
  )

;; open-junk-file.el
(require 'open-junk-file)
(setq open-junk-file-formant "~/junk/%Y/%m-%d-%H%M%S.")

;; color-moccur 検索結果のリストアップ
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
    (setq moccur-use-migemo t)))

;; undohist 編集履歴の記憶
(when (require 'undohist nil t)
  (undohist-initialize))

;; バッテリー残量表示
(display-battery-mode t)
;; ファイルサイズを表示
(size-indication-mode t)

;; backup autosave
(setq make-backup-files nil)
(setq auto-save-default t)
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" , (expand-file-name "~/.emacs.d/backups/") t)))
;; オートセーブ生成までの秒間隔
(setq auto-save-timeout 15)
;; オートセーブ生成までのタイプ間隔
(setq auto-save-interval 60)

;; gtags-modeのキーバインドを有効化する
(setq gtags-suggested-key-mapping t)
(require 'gtags nil t)

;; multi-term
;;(when (require 'multi-term nil t)       
;;  (setq multi-term-program "/bin/zsh"))

;; multi-shell
(when (require 'multi-shell nil t)
  (setq multi-shell-command "/bin/zsh"))
;; カーソル場所を保存
(setq-default save-place t)

;; 最終行に必ず１行挿入する
(setq require-final-newline t)

;; バッファの最後でnewlineで新規行を追加するのを禁止
(setq next-line-add-newlines nil)

;; 終了時にautosavefileを消す
(setq delete-auto-save-files t)

;; 保管時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; 部分一致の補間機能を使う
(if (string-match "23" emacs-version)
    (partial-completion-mode t))

;; emacs-lisp mode
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   ;; スペースでインデント
   (setq indent-tabs-mode nil)))

;; powerline.el {{{
(when (require 'powerline nil t)
  (defun arrow-right-xpm (color1 color2)
    "Return an XPM right arrow string representing."
    (format "/* XPM */
            static char * arrow_right[] = {
            \"12 18 2 1\",
            \".	c %s\",
            \" 	c %s\",
            \".           \",
            \"..          \",
            \"...         \",
            \"....        \",
            \".....       \",
            \"......      \",
            \".......     \",
            \"........    \",
            \".........   \",
            \".........   \",
            \"........    \",
            \".......     \",
            \"......      \",
            \".....       \",
            \"....        \",
            \"...         \",
            \"..          \",
            \".           \"};"  color1 color2))

  (defun arrow-left-xpm (color1 color2)
    "Return an XPM right arrow string representing."
    (format "/* XPM */
                      static char * arrow_right[] = {
                      \"12 18 2 1\",
                      \".	c %s\",
                      \" 	c %s\",
                      \"           .\",
                      \"          ..\",
                      \"         ...\",
                      \"        ....\",
                      \"       .....\",
                      \"      ......\",
                      \"     .......\",
                      \"    ........\",
                      \"   .........\",
                      \"   .........\",
                      \"    ........\",
                      \"     .......\",
                      \"      ......\",
                      \"       .....\",
                      \"        ....\",
                      \"         ...\",
                      \"          ..\",
                      \"           .\"};"  color2 color1))


  (defconst color1 "#999")
  (defconst color2 "#555")

  (defvar arrow-right-1 (create-image (arrow-right-xpm color1 color2) 'xpm t :ascent 'center))
  (defvar arrow-right-2 (create-image (arrow-right-xpm color2 "None") 'xpm t :ascent 'center))
  (defvar arrow-left-1  (create-image (arrow-left-xpm color2 color1) 'xpm t :ascent 'center))
  (defvar arrow-left-2  (create-image (arrow-left-xpm "None" color2) 'xpm t :ascent 'center))

  (setq-default mode-line-format
                (list  '(:eval (concat (propertize " %b " 'face 'mode-line-color-1)
                                       (propertize " " 'display arrow-right-1)))
                       '(:eval (concat (propertize " %m " 'face 'mode-line-color-2)
                                       (propertize " " 'display arrow-right-2)))

                       ;; Justify right by filling with spaces to right fringe - 16
                       ;; (16 should be computed rahter than hardcoded)
                       '(:eval (propertize " " 'display '((space :align-to (- right-fringe 17)))))

                       '(:eval (concat (propertize " " 'display arrow-left-2)
                                       (propertize " %p " 'face 'mode-line-color-2)))
                       '(:eval (concat (propertize " " 'display arrow-left-1)
                                       (propertize "%4l:%2c  " 'face 'mode-line-color-1)))
                       )) 

  (make-face 'mode-line-color-1)
  (set-face-attribute 'mode-line-color-1 nil
                      :foreground "#fff"
                      :background color1)

  (make-face 'mode-line-color-2)
  (set-face-attribute 'mode-line-color-2 nil
                      :foreground "#fff"
                      :background color2)

  (set-face-attribute 'mode-line nil
                      :foreground "#fff"
                      :background "#000"
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#fff"
                      :background "#000"))
;; }}}

;; M-g でM-x goto-line
(global-set-key (kbd "M-g") 'goto-line)

;; 同一ファイル名のバッファ名にはディレクトを表示
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; C-x C-f時に、カーソル付近のファイル名orURLをミニバッファに持ってくる
(ffap-bindings)

;; yasnippet.el
(when (require 'yasnippet-config nil t)
  (yas/setup "~/.emacs.d/elisp/yasnippet-0.6.1c"))

(require 'summarye)

;; よそのwindowにカーソルを表示しない
(setq cursor-in-non-selected-windows nil)
;; 画像ファイルを表示
(auto-image-file-mode)
;; 無駄な空行を可視化
(setq-default indicate-empty-lines t)
;; isearchのハイライトの反応を良くする
(setq isearch-lazy-highlight-initial-delay 0)
;; line-space
(setq-default line-spacing 1)
;; 縦分割ウィンドウを移動時に ウィンドウの横幅比率を変化
(defun my-other-window ()
  ""
  (interactive)
  (other-window 1)
  (let (( max-width (truncate (* (screen-width) 0.65))))
    (if (< (window-width) max-width)
        (enlarge-window-horizontally (- max-width (window-width))))))

;; 文字コード
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
;; ファイル名
(when (eq system-type 'darwin)          ; Mac のファイル名設定
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
(when (eq system-type 'w32)             ; Windowsのファイル名設定
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

(when (require 'redo+ nil t)
  (global-set-key (kbd  "C-.") 'redo))

;; menu-tree.el
;; メニュー日本語化
;; http://www11.atwiki.jp/s-irie/pages/13.html
(require 'menu-tree nil t)

;; C-kで行全体を削除

;; elispにて変数・関数
(defun elisp-mode-hooks ()
  ;;  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (require 'eldoc-extension nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)
(prin1-to-string emacs-lisp-mode-hook)

(require 'hideshow)

(require 'goto-chg)
(global-set-key [f5] 'backupgoto-last-change)
(global-set-key [S-f5] 'goto-last-cha-rverse)

;; 参考:http://www23.atwiki.jp/selflearn/pages/41.html#id_5448ea00
(when (locate-library "gtags")
  (require 'gtags)
  (global-set-key (kbd "M-t") 'gtags-find-tags)
  (global-set-key (kbd "M-r") 'gtags-find-rtags)
  (global-set-key (kbd "M-s") 'gtags-find-symbol)
  (global-set-key (kbd "M-p") 'gtags-find-pattern)
  (global-set-key (kbd "M-f") 'gtags-find-file)
  (global-set-key [?\C-,] 'gtags-pop-stack)
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (gtags-mode 1)
               (gtags-make-complete-list))))

(load "color-moccur")
(setq dmoccur-recursive-search t)
(setq moccur-grep-default-word-near-point t)
(setq moccur-split-word t)
(setq dmoccur-exclusion-mask (append '("\\~$" "\\.svn\\/\*" "\\.o$"
                                       "GPATH" "GRTAGS" "GSYMS" "GTAGS")
                                     dmoccur-exclusion-mask))
;; grep結果バッファでのカーソル移動でダイナミックにファイルを開いてくれる
(when (require 'color-grep)
  (setq color-grep-sync-kill-buffer t))

(global-set-key [f12] 'speedbar)

;; ファイルを開いた時に以前編集していた場所に移動
(load "saveplace")
(setq-default save-place t)

;; ignore byte-complie warnings
(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  obsolete
                                  noruntime
                                  cl-funcitons
                                  interactive-only))

;; e2wm.el
;; http://d.hatena.ne.jp/kiwanami/20100528/1275038929
(require 'e2wm nil t)

;; Emacs のCommands Hisotryを再起動語も使用する
;; http://qiita.com/items/4b489c0abbb39a5dcc45
(setq desktop-globals-to-save '(extended-command-history))
(setq desktop-files-not-to-save "")
(desktop-save-mode 1)

;; window移動
;; http://d.hatena.ne.jp/tomoya/20120512/1336832436 
(windmove-default-keybindings 'super)   ;Mac用
                                        ; (windmove-default-keybindings 'meta)
                                        ; (windmove-default-keybindings) 引数なしの場合はShift

;; 良い感じにウィンドウ分割
(global-set-key (kbd "C-x C-w") 'good-split-window)

(defun good-split-window ()
  (interactive)
  (if (< (window-width) (window-height))
      (split-window-vertically)
    (split-window-horizontally)
    ))

;; ウィンドウ操作の履歴をundo/redo
;; C-c <left> / C-c <right> 
(when (fboundp 'winner-mode)
  (winner-mode t))

;; C-a でインデントで飛ばした行頭に移動
;; http://e-arrows.sakura.ne.jp/2010/02/vim-to-emacs.html
(defun my-beginning-of-indented-line (current-point)
  (interactive "d")
  (if (string-match
       "^[ \t]+$"
       (save-excursion
         (buffer-substring-no-properties
          (progn (beginning-of-line) (point))
          current-point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key (kbd "C-a") 'my-beginning-of-indented-line)

;; *scratch*を消さない
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))

(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))
;; beepを消す
(defun my-bell-function ()
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                                keyboard-quit mwheel-scroll down up next-line previous-line
                                backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

(require 'w3m)
(require 'w3m-load)
(setq w3m-use-cookies t)
(setq browse-url-browser-function 'w3m-browse-url)
(setq w3m-key-binding 'info)

(global-set-key (kbd "C-x C-b") 'bs-show)

;; ruby
(require 'ruby-electric nil t)          ; 括弧の自動挿入
(when (require 'ruby-block nil t)       ; end に対応する行のハイライト
  (setq ruby-block-highlight nil t))  
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

;; ruby-mode-hook用の関数を定義
(defun ruby-mode-hooks ()
  (inf-ruby-keys)
  (ruby-electric-mode t)
  (ruby-block-mode t))
(add-hook 'ruby-mode-hook 'ruby-mode-hooks) ; ruby-mode-hookに追加
