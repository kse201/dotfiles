;; auto-instlalによってインストールされるEmacs Lispをロードパスに加える
;; デフォルトは、~/.emacs.d/auto-install/
(add-to-list 'load-path "~/.emacs.d/auto-install/")
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  ;; proxy setting
  (setq url-proxy-services '(("http" . "172.16.1.1:3128")))
  (auto-install-compatibility-setup))
;; 起動時にEmacsWikiのページ名を補完候補に加える
(auto-install-update-emacswiki-package-name t)
;; install-elisp.el互換モードにする
(auto-install-compatibility-setup)
;; ediff関連のバッファを1つのフレームにまとめる
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; 現在行に色をつける
(global-hl-line-mode 1)
;; 履歴を次回Emacs起動時にも保存する
(savehist-mode 1)
(require 'saveplace)
;; <C-h>を後退に割り当てる
(global-set-key (kbd "C-h") 'delete-backward-char)
;; モードラインに時刻を表示する
(display-time)
;; 行番号・桁番号を表示する
(line-number-mode 1)
;; リージョンに色をつける
(transient-mark-mode 1)
;; GCを減らして軽くする (デフォルトの10倍)
(setq gc-cons-threshold (* gc-cons-threshold))
;; ログの記録行数を増やす
(setq message-log-max 10000)
;; ミニバッファを再帰的に呼ぶ出せるようにする
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
(defadvice abort-recursive-edit (before minibuffer-sace activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))
;; yesは面倒なのでyで十分
(defalias 'yes-or-no-p 'y-or-n-p)
;; ツールバーとスクロールバーを消す
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
;; 最近のファイル500個個を保存する
(setq recentf-max-saved-items 500)
;; 最近使ったファイルに加えないファイルを正規表現で指定する
(setq recentf-exclude '("/TAGS$" "/var/tmp/"))
(require 'recentf-ext)
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(require 'auto-async-byte-compile)
;; 自動バイトコンパイルを無効にするファイル名の正規表現
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
(setq auto-async-byte-compile-exclude-files-regexp "^_")
;; recentf-ext.el
(define-key global-map (kbd "C-;") 'recentf-open-files )

;; キーバインド
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\M-?" 'help-for-help)
(define-key global-map "\C-z" 'undo)                 ; undo
(define-key global-map "\C-ci" 'indent-region)       ; インデント
(define-key global-map "\C-c\C-i" 'dabbrev-expand)   ; 補完
(define-key global-map "\C-c;" 'comment-region)      ; コメントアウト
(define-key global-map "\C-c:" 'uncomment-region)    ; コメント解除
(define-key global-map "\C-o" 'toggle-input-method)  ; 日本語入力切替
(define-key global-map "\C-\\" nil) ; \C-\の日本語入力の設定を無効にする
(define-key global-map "\C-c " 'other-frame)         ; フレーム
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines
(define-key global-map (kbd "C-t") 'other-window)

;; Localeに合わせた環境の設定
(set-locale-environment nil)
;; 対応する括弧を光らせる
(show-paren-mode 1)
;; 強力な補完機能を使う
(partial-completion-mode 1)
;; 最終更新日の自動挿入
(require 'time-stamp)
;画像ファイルを表示する
(auto-image-file-mode t)
;; 自動でファイルを挿入する
(auto-insert-mode t)
;; C-x,bでバッファリストをミニバッファに表示する
(iswitchb-mode 1)

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
(pc-selection-mode)
;; 選択範囲に色をつけて見た目をわかりやすく
(trancsient-mark-mode 1)
(set-face-attribute 'default nil 
		    :family "Ricty"
		    :height 140)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec 
  :family "Ricty")
 )

(when  (require 'package nil t)
  ;; バッケージリポジトリにMarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives '("marmalade" "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" "http://tromey.com/elpa/"))
  (package-initialize))

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
;; paranのスタイル 
(setq shwo-paren-style 'expression)
;; フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "red")
