;;; @ surface
;;; OS 別設定
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-frame-position (selected-frame) 0 0)
         (setq term-setup-hook 'jbr-init)
         (setq window-setup-hook 'jbr-init))
        ((eq ws 'ns)
         ;; for MacBook Air(Late2010) 11inch display
         (set-frame-position (selected-frame) 0 0)
         (set-frame-size (selected-frame) 95 47))))
;; スクロール時のカーソル位置の維持
(setq scroll-preserve-screen-position nil)
;; 画面スクロール時の重複行数
(setq next-screen-context-lines 1)
;; Scroll window on a line-by-line basis
(setq scroll-conservatively 1000)
(setq scroll-step 1)
(setq scroll-margin 0) ; default=0
(setq yank-excluded-properties t)
;;; 見た目関連
;; 選択範囲に色をつけて見た目をわかりやすく
(transient-mark-mode 1)
;; フォント設定
(set-face-attribute
 'default nil
 :family "Ricty"
 :height 160)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec
  :family "Ricty"))

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")
;; 行番号表示
(global-linum-mode t)
;; 行番号のフォーマット
(set-face-attribute 'linum nil :foreground "red" :height 1)
(setq linum-format "%2d")
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))
;; TABの表示幅 4
(setq-default tab-width 4)

;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)

;; 対応括弧のハイライト
(show-paren-mode nil)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "red")
(setq show-paren-delay 0.125) ; 表示までの秒数 emacs24では0だと重い 1x http://suzukima.hatenablog.com/entry/2012/08/16/232210

;; リージョン内の行数と文字数をモードラインに表示する (範囲指定字のみ)
;; http://d.hatena.nejp/sonota88/20110224/
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))


;; バッテリー残量表示
(display-battery-mode t)
;; ファイルサイズを表示
(size-indication-mode t)

;; 行末空白強調
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; 現在行に色をつける
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background  "#98FB98"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode 1)

;; モードラインに時刻を表示する
(display-time)
;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;;; カーソルの位置が何行目かを表示する
(line-number-mode t)
;; スタートアップ時のエコー領域メッセージの非表示
(setq inhibit-startup-echo-area-message -1)
;; カーソルの点滅を止める
(blink-cursor-mode 0)

;; リージョンに色をつける
(transient-mark-mode 1)
;; スクロールバーを右側に
(set-scroll-bar-mode 'right)            ; GUI emacs
;; 現在の関数名をモードラインに
(which-function-mode 1)
(setq custom-theme-load-path "~/.emacs.d/themes")

;;; 画像ファイルを表示
(auto-image-file-mode t)
;; 空白や長すぎる行を視覚化する。
(require 'whitespace)
;; 1行が80桁を超えたら長すぎると判断する。
(setq whitespace-line-column 80)
(setq whitespace-style '(face              ; faceを使って視覚化する。
                         trailing          ; 行末の空白を対象とする。
                         space-before-tab  ; タブの前にあるスペースを対象とする。
                         space-after-tab)) ; タブの後にあるスペースを対象とする。
;; デフォルトで視覚化を有効にする。
(global-whitespace-mode 0)

;; よそのwindowにカーソルを表示しない
(setq cursor-in-non-selected-windows nil)
;; 無駄な空行を可視化
(setq-default indicate-empty-lines t)
;; isearchのハイライトの反応を良くする
(setq isearch-lazy-highlight-initial-delay 0)

;; スクラッチバッファの初期メッセージ消去
(setq initial-scratch-message "Scratch\n========\n\n")

;; 対応する括弧を光らせる
(require 'paren)
;; 画面内に収まらない時は括弧内も光らせる
(setq show-paren-style 'mixed)
(make-face 'paren-mismatch)
(set-face-foreground 'paren-mismatch "white")
(set-face-background 'paren-mismatch "lightcoral")
;; 正規表現見やすく
(set-face-foreground 'font-lock-regexp-grouping-backslash "#66CC99")
(set-face-foreground 'font-lock-regexp-grouping-construct "#9966CC")
(setq show-paren-face  'paren-match)
(setq show-paren-mismatch-face 'paren-mismatch)

;; line-space
(setq-default line-spacing 1)
(global-set-key [f12] 'speedbar)

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


