;;
;; init.el
;;
;; Last Change: 23-Dec-2012.
(eval-when-compile (require 'cl))

;; Language.
(set-language-environment 'Japanese)

;; Coding system.
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(require 'server)
(unless (server-running-p)
  (server-start))
;;; ------------------------------
;;; @ Function
;;; init.el開く
(defun edit-init ()
  "edit init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;;; scratch開く
(defun edit-scratch ()
  "edit *scratch*"
  (interactive)
  (switch-to-buffer "*scratch*"))

;;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  "load-pathを追加する関数を定義"
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; キーバインド登録簡略化
;; http://pod.hatenablog.com/entry/2012/12/10/204538
(defun define-many-keys (key-map key-table)
  "キーバイントの登録簡略化"
  (loop for (key . cmd) in key-table
        do (define-key key-map (read-kbd-macro key) cmd)))

(let ((default-directory "~/.emacs.d/elisp/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "conf" "public_repos" "elpa" "auto-install" "elisp")

;; 起動時にウィンドウ最大化
;; http://www.emacswiki.org/emacs/FullScreen#toc12
(defun jbr-init ()
  "Called from term-setup-hook after the default
  terminal setup is
  done or directly from startup if term-setup-hook not
  used.  The value
  0xF030 is the command for maximizing a window."
  (interactive)
  (w32-send-sys-command #xf030)
  (ecb-redraw-layout)
  (calendar))

;; 良い感じにウィンドウ分割
(defun good-split-window ()
  "良い感じにウィンドウ分割"
  (interactive)
  (if (< (window-width) (* (window-height) 1.5) )
      (split-window-vertically)
    (split-window-horizontally)))

;; 縦分割ウィンドウを移動時に ウィンドウの横幅比率を変化
(defun my-other-window ()
  "Auto resize window when 'other-window"
  (interactive)
  (other-window 1)
  (let (( max-width (truncate (* (screen-width) 0.5))))
    (if (< (window-width) max-width)
        (enlarge-window-horizontally (- max-width (window-width))))))

;;; マーク箇所に移動
(defun move-to-mark ()
  "マーク箇所に移動"
  (interactive)
  (let ((pos (point)))
    (goto-char (mark))
    (push-mark pos)))

;;; ------------------------------------------------------------------
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-frame-position (selected-frame) 0 0)
         (setq term-setup-hook 'jbr-init)
         (setq window-setup-hook 'jbr-init))
        ((eq ws 'ns)
         ;; for MacBook Air(Late2010) 11inch display
         (set-frame-position (selected-frame) 0 0)
         (set-frame-size (selected-frame) 95 47))))

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
    "Am I in the office? If I am in the office, my IP address must start with '172.16.1..'."
    (let ((ip (some #'machine-ip-address *network-interface-names*)))
      (and ip
           (eq 0 (string-match "^172\\.16\\.1\\." ip)))))

  (if (officep)
      (progn
        (setq url-proxy-services '(("http" . "172.16.1.1:3128")))
                                        ;(setq w3m-command-arguments
                                        ;     (nconc w3m-command-arguments
                                        ;     '("-o" "http_proxy=http://172.16.1.1:3128/")))
        )
    (progn
      (setq url-proxy-services nil))))
;; ediff関連のバッファを1つのフレームにまとめる
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; 現在行に色をつける
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
;; 評価した結果を全部憑依
(setq eval-comment-region nil)
;; 行番号・桁番号を表示する
(line-number-mode 1)
;; リージョンに色をつける
(transient-mark-mode 1)
;; GCを減らして軽くする (デフォルトの10倍)
(setq gc-cons-threshold 5242880)
;; ログの記録行数を増やす
(setq message-log-max 10000)
;; スクロールバーを右側に
(set-scroll-bar-mode 'right)            ; GUI emacs
;; 現在の関数名をモードラインに
(which-function-mode 1)
(setq custom-theme-load-path "~/.emacs.d/themes")
;; スクロール時のカーソル位置の維持
(setq scroll-preserve-screen-position nil)
;; 画面スクロール時の重複行数
(setq next-screen-context-lines 1)
;; Scroll window on a line-by-line basis
(setq scroll-conservatively 1000)
(setq scroll-step 1)
(setq scroll-margin 0) ; default=0
(setq yank-excluded-properties t)
;;; 画像ファイルを表示
(auto-image-file-mode t)
                                        ; 空白や長すぎる行を視覚化する。
(require 'whitespace)
;; 1行が80桁を超えたら長すぎると判断する。
(setq whitespace-line-column 80)
(setq whitespace-style '(face              ; faceを使って視覚化する。
                         trailing          ; 行末の空白を対象とする。
                         space-before-tab  ; タブの前にあるスペースを対象とする。
                         space-after-tab)) ; タブの後にあるスペースを対象とする。
;; デフォルトで視覚化を有効にする。
(global-whitespace-mode 0)
;;; 大文字小文字返還
;;; C-x C-u/C-l 大文字小文字 upper / lower
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;;; 関数名をウィンドウの上部に現在の関数名を表示
(which-function-mode 1)
;;; 自動スペルチェック
(setq-default flyspell-mode t)
(setq ispell-dictionary "american")
(setq ispell-program-name "aspell")
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
;; 選択リージョンを使って検索
(defadvice isearch-mode
  (around isearch-mode-default-string
          (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))


;; ------------------------------
;; @ backup
;; backup autosave
;; 変更ファイルのバックアップ
(setq make-backup-files t)
(setq auto-save-default t)
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" , (expand-file-name "~/.emacs.d/backups/") t)))
;; オートセーブ生成までの秒間隔
(setq auto-save-timeout 40)
;; オートセーブ生成までのタイプ間隔
(setq auto-save-interval 200)

;; 変更ファイルの番号つきバックアップ
(setq version-control t)

;; 編集中ファイルのバックアップ
(setq auto-save-list-file-name t)
(setq auto-save-list-file-prefix t)

;; 編集中ファイルのバックアップ先
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; 編集中ファイルのバックアップ間隔（秒）
(setq auto-save-timeout 60)

;; 編集中ファイルのバックアップ間隔（打鍵）
(setq auto-save-interval 500)

;; バックアップ世代数
(setq kept-old-versions 1)
(setq kept-new-versions 2)
;;; 古いバックアップファイルは削除しない
(setq delete-old-versions nil)
;; ブックマーク
(setq bookmark-save-flag 1)
(setq bookmark-sort-flag nil)
(defun bookmark-arrange-latest-top ()
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (cons latest (delq latest bookmark-alist))))
  (bookmark-save))
(add-hook 'bookmark-after-jump-hook 'bookmark-arrange-latest-top)
;;; ------------------------------

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

;; 最近使ったファイルをメニュー表示
(recentf-mode t)
;; 最近使ったファイルの表示数
(setq recentf-max-menu-items 10)
;; 最近のファイル500個個を保存する
(setq recentf-max-saved-items 500)

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; *.elを保存時、自動バイトコンパイル
;;(add-hook 'after-save-hook
;;         (lambda ()
;;          (let ((file (buffer-file-name)))
;;           (when (string-match ".*\\.el$" file)
;;            (byte-compile-file file))))

;; @ recentf-ext
;;(global-set-key (kbd "C-;") 'recentf-open-files )

;; キーバインド
(define-many-keys global-map
  '(("C-h" . delete-backward-char)
    ("<f1>" . help-for-help)
    ("C-c i" . indent-region ); インデント
    ("C-c C-i" . dabbrev-expand ); 補完
    ("C-c );" . comment-region ); コメントアウト
    ("C-c :" . uncomment-region ); コメント解除
    ("C-\\" . nil ); \C-\の日本語入力の設定を無効にする
    ("C-m" . newline-and-indent)
    ("C-c l" . toggle-truncate-lines)
    ("C-x C-o" . my-other-window)
    ("M-a" . mark-whole-buffer)
    ("M-y" . backward-kill-word ); 一つ前の単語削除
    ("C-x o" . browse-url-at-point );ブラウザ起動
    ("C-x C-g" . goto-line ); 指定行へ移動
    ("C-x w h" . windmove-left)
    ("C-x w j" . windmove-down)
    ("C-x w k" . windmove-up)
    ("C-x w l" . windmove-right)
    ("C-x SPC" . good-split-window)
    ("C-c C-@" . move-to-mark)
    ("C-c C-e" . edit-init)
    ("C-x C-c" . helm-M-x)
    ("C-x C-z" . nil)
    ("M-p" . scroll-down)
    ("M-n" . scroll-up)
    ("C-M-h" . delete-horizontal-space)
    ))
(define-key mode-specific-map "c" 'compile)
(defalias 'exit 'save-buffers-kill-emacs)

;; 範囲指定していないとき、C-wで前の単語を削除
;;http://dev.ariel-networks.com/wp/documents/aritcles/emacs/part16
(defadvice kill-region (around kill-word-or-kill-region activate)
  (if (and (interactive-p) transient-mark-mode (not mark-active))
      (backward-kill-word 1)
    ad-do-it))

;; minibuffer用
(define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)

;; Localeに合わせた環境の設定
(set-locale-environment nil)
(show-paren-mode 1)
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
;; 強力な補完機能を使う
;; (partial-completion-mode 1)
;; 画像ファイルを表示する
(auto-image-file-mode t)
;; 自動でファイルを挿入する
(auto-insert-mode t)
;; C-x,bでバッファリストをミニバッファに表示する
(iswitchb-mode 1)
;; C-x b でbuffersを選ぶとき便利
(if (string-match "^23\." emacs-version)
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
(if (string-match "^23\." emacs-version)
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
;; 行番号のフォーマット
;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
(set-face-attribute 'linum nil :height 1)
(setq linum-format "%2d")
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))
;; TABの表示幅 4
(setq-default tab-width 4)
;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)
;; evalした結果全部表示
(setq eval-expression-print-length nil)
;; 行頭のC-k一回で行全体を削除
(setq kill-whole-line t)
;; 対応括弧のハイライト
(setq show-paren-delay 0.125) ; 表示までの秒数 emacs24では0だと重い 1xhttp://suzukima.hatenablog.com/entry/2012/08/16/232210
(show-paren-mode t)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "red")


;; リージョン内の行数と文字数をモードラインに表示する (範囲指定字のみ)
;; http://d.hatena.nejp/sonota88/20110224/
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

(when (eq system-type 'darwin)
  ;;(setq mac-command-key-is-meta nil)    ;コマンドキーをメタにしない
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)      ; Optionをメタに
  ;;  (setq mac-command-modifier 'super)    ; コマンドをSuperに
  (setq mac-pass-control-to-system t))   ; コントロールキーをMacではなくEmacsに渡す

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(eval-when-compile
  (require 'cl))

;; バッテリー残量表示
(display-battery-mode t)
;; ファイルサイズを表示
(size-indication-mode t)

;; 最終行に必ず１行挿入する
(setq require-final-newline t)

;; バッファの最後でnewlineで新規行を追加するのを禁止
(setq next-line-add-newlines nil)

;; 終了時にautosavefileを消す
(setq delete-auto-save-files t)

;; 保管時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; 行末空白強調
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; 部分一致の補間機能を使う
(if (string-match "^23\." emacs-version)
    (partial-completion-mode t))

;; emacs-lisp mode
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   ;; スペースでインデント
   (setq indent-tabs-mode nil)))

;; M-g でM-x goto-line
(global-set-key (kbd "M-g") 'goto-line)

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

;; C-kで行全体を削除

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

;; Emacs のCommands Hisotryを再起動語も使用する
;; http://qiita.com/items/4b489c0abbb39a5dcc45
(setq desktop-globals-to-save '(extended-command-history))
(setq desktop-files-not-to-save "")
(desktop-save-mode 1)

;; window移動
;; http://d.hatena.ne.jp/tomoya/20120512/1336832436
(windmove-default-keybindings 'super)
;;Mac用
;; (windmove-default-keybindings 'meta)
;; (windmove-default-keybindings) 引数なしの場合はShift

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

;; スクラッチバッファの手記メッセージ消去
(setq initial-scratch-message "")
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
(setq ring-bell-function 'ignore)



;; C-Ret で矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; http://qiita.com/items/f0db094fde6640143f42
(if (file-directory-p (expand-file-name "~/bin"))
    (progn
      (add-to-list 'exec-path (expand-file-name "~/bin"))
      (setenv "PATH" (mapconcat 'identity exec-path ":"))))

;; GUIで直接ファイルを開いた場合フレームを作成しない
                                        ;(add-hook 'before-make-frame-hook
                                        ;         (lambda ()
                                        ;          (when (eq tabbar-mode t)
                                        ;           (switch-to-buffer (buffer-name))
                                        ;          (delete-this-frame))))

;; http://qiita.com/items/b836e7792be0a7c65fd4
;; C系統,Pythonにて1行80文字を超えるとハイライト
(add-hook 'c-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))
(add-hook 'c++-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))
(add-hook 'python-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

;; Javaで1行100文字を超えるとハイライト
(add-hook 'java-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

;; time-stamp
(when (require 'time-stamp nil t)
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-active t)
  (setq time-stamp-start "Last Change: ")
  (setq time-stamp-format "%02d-%3b-%04y.")
  (setq time-stamp-end " \\|$"))

;; スクリプト保存時、自動的にchmod+x
;;; ファイル先頭に#!が含まれているとき
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

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

(add-to-list 'load-path "~/src/emacswikipages/" t)


;; 履歴を次回Emacs起動時にも保存する
(require 'saveplace )
(savehist-mode 1)

;; 対応する括弧を光らせる
(require 'paren)

;; 最近使ったファイルに加えないファイルを正規表現で指定する
(setq recentf-exclude '("/TAGS$" "/var/tmp/"))
(require 'recentf-ext)

(when (require 'auto-async-byte-compile nil t)
  ;; 自動バイトコンパイルを無効にするファイル名の正規表現
  (setq auto-async-byte-compile-exclude-files-regexp "init.el")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))


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
        ( "M-x" . anything-M-x)
        ( "C-; C-;" . anything)
        ( "C-s" . anything-occur)
        ( "C-x b" . anything-for-files)
        ( "C-x C-f" . anything-find-files)
        ("C-x g" . anything-imenu)
        ))
    (define-many-keys anything-map
      '(("C-z" . nil)
        ("C-w" . anything-execute-persistent-action)
        ("C-o" . nil)
        ("C-M-n" . anything-next-source)
        ("C-M-p" . anything-previous-source)))

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

;;; @ auto-complete
;; 補完候補を自動ポップアップ
(when (require 'auto-complete nil t)
  (require 'popup)
  (require 'fuzzy)
  (setq popup-use-optimized-column-computation nil)
  (setq ac-ignore-case 'smart)               ; 大文字が含まれている場合のみ、大文字/小文字を区別する
  (ac-set-trigger-key "TAB")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (global-auto-complete-mode 1)
  (setq ac-modes (cons 'js-mode ac-modes)))
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacd.d/ac-dict")
  (setq ac-ignore-case t)
  (ac-config-default)                     ; デフォルト設定
  (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
  (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
  (ac-mode t))

;;; @ open-junk-file
(require 'open-junk-file)
(setq open-junk-file-formant "~/junk/%Y/%m-%d-%H%M%S.")

;;; @ gnus gmail
(when (require 'gnus nil t)
  (load "gnus-setup")
  (require 'gnus-start)
  (require 'gnus-art)
  (require 'auth-source)
  (require 'starttls)
  (require 'nnimap)
  (require 'nnir)

  ;; Username and mail address.
  (setq user-full-name "kse.201"
        user-mail-address "kse.201@gmail.com")
  (add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"
                                                       (nnimap-stream ssl)
                                                       (nnimap-address "imap.gmail.com")
                                                       (nnimap-server-port 993)
                                                       (nnir-search-engine imap)))

  ;; for reading mail by imap.
  (setq gnus-select-method
        '(nnimap "gmail"
                 (nnimap-address "imap.gmail.com")
                 (nnimap-server-port 993)
                 (nnimap-authinfo-file "~/.emacs.d/.authinfo")
                 (nnimap-stream ssl)))

  ;; for sending mail.
  (setq message-send-mail-function 'smtpmail-send-it
        send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                     "kse.201@gmail.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        ;; Cc: and Bcc: to header of message-mode.
        message-default-mail-headers "Cc: \nBcc: \n")

  ;; customize `gnu-summary-line-foramt'.
  (defvar my-gnus-mail-addres-regex "kse\\.201\\(\\+[^@]+\\)?@gmail\\.com"
    "*Regular expression of mail address that indicates for me.")

  ;; from http://emacs.wordpress.com/2007/10/07/gmail-envy/
  ;; and customize it.
  (defun gnus-user-format-function-j (headers)
    "Return a \">\" if variable `my-gnus-mail-addres-regex' matches in To,
CC or Bcc. If not matched, return a \" \"."
    (cond
     ((or (string-match my-gnus-mail-addres-regex
                        (gnus-extra-header 'To headers))
          (string-match my-gnus-mail-addres-regex
                        (gnus-extra-header 'Cc headers))
          (string-match my-gnus-mail-addres-regex
                        (gnus-extra-header 'BCc headers)))
      ">")
     (t
      " ")))
  (setq gnus-summary-line-format "%uj%U%R%I%(%[%-23,23f%]%) %s\n")

  ;; gnu-topic-mode by default
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

  ;; MUA is gnus.
  (setq read-mail-command 'gnus
        mail-user-agent 'gnus-user-agent)

  (setq ;; Do not use mailcrypt.
   gnus-use-mailcrypt nil
   gnus-check-new-newsgroups nil
   gnus-use-cache t
   gnus-cache-directory "~/Mail/cache/"
   gnus-cache-enter-articles '(ticked dormant read unread)
   gnus-cache-remove-articles nil
   gnus-cacheable-groups "^nnimap"
   gnus-posting-styles '((".*" (name "kse.201")))
   ;; Do not split the mail when mail was large.
   mime-edit-split-message nil
   ;; treates wide character
   gnus-use-correct-string-widths t
   ;; Do not ask online or not.
   gnus-agent-go-online t
   ;; do not goto cursor to unread group.
   gnus-group-goto-unread nil
   ;; show also user-agent.
   gnus-visible-headers (concat gnus-visible-headers "\\|^User-Agent")
   ;; extra headers to parse.
   gnus-extra-headers '(To Newsgroups X-Newsreader
                           Content-Type CC User-Agent Gnus-Warning)
   nnmail-extra-headers gnus-extra-headers
   ;; If member of thread that includes new article has old
   ;; article, grab old articles to display thread.
   gnus-fetch-old-headers t)
  
  ;; Display always 500 articles at least in summary buffer.
  (defvar my-gnus-summary-maximum-articles 500
    "*The recent X number of articles that displayed in summary-buffer
by use `gnus-topic-select-group' (RET) in gnus-group-buffer. The default
value is 500. The recent 500 articles are always displayed at least.")
  (setq gnus-alter-articles-to-read-function
        #'(lambda (group articles)
            (let ((active (gnus-active group)))
              (delete-dups
               (append articles
                       (gnus-uncompress-range
                        (cond
                         (my-gnus-summary-maximum-articles
                          ;; show `my-gnus-summary-maximum-articles' messages.
                          (cons (max (car active)
                                     (- (cdr active)
                                        my-gnus-summary-maximum-articles
                                        -1))
                                (cdr active)))
                         (t
                          ;; show always all messages.
                          active))))))))

  ;; Gnus + EasyPG
  (require 'epg-config)
  (require 'gnus-msg)
  (require 'mml2015)
  (setq gnus-message-replysign t
        gnus-message-replyencrypt t
        gnus-message-replysignencrypted t
        mm-verify-option 'always
        mm-decrypt-option 'always
        mml2015-use 'epg
        mml2015-encrypt-to-self t
        mml2015-always-trust nil
        mml2015-cache-passphrase t
        mml2015-passphrase-cache-expiry '36000
        mml2015-sign-with-sender t
        gnus-buttonized-mime-types '("multipart/alternative"
                                     "multipart/encrypted"
                                     "multipart/signed"))

  ;; mail contacts list manager.
  (require 'bbdb)
  ;; take mail address automatically
  (setq bbdb/news-auto-create-p t)
  ;; add address automatically
  (setq bbdb-always-add-addresses t)
  ;; do not use popup
  (setq bbdb-use-pop-up nil)
  ;; my mail address
  (setq bbdb-user-mail-names "kse\\.201@gmail\\.com")
  (bbdb-initialize 'gnus 'message))


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
    (setq moccur-use-migemo t)))

;;; @ grep結果バッファでのカーソル移動でダイナミックにファイルを開いてくれる
(when (require 'color-grep nil t)
  (setq color-grep-sync-kill-buffer t)
  ;; M-x grep-findでPerlのackコマンドを使うよう変更
  (setq grep-find-command "ack --nocolor --nogroup "))

;; undohist 編集履歴の記憶
(when (require 'undohist nil t)
  (undohist-initialize))

;; multi-term
;;(when (require 'multi-term nil t)
;;  (setq multi-term-program "/bin/zsh"))

;;; @ multi-shell
(when (require 'multi-shell nil t)
  (setq multi-shell-command "/bin/zsh"))

;;; @ 同一ファイル名のバッファ名にはディレクトを表示
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; @ yasnippet
(when (require 'yasnippet-config nil t)
  (yas/setup "~/.emacs.d/elisp/yasnippet-0.6.1c"))

(require 'summarye)

;;; @ magit 色変更
(when (require 'magit nil t)
  (set-face-foreground 'magit-diff-add "#b9ca4a") ; 追加した部分を緑に
  (set-face-foreground 'magit-diff-del "#d54e53")  ; 削除した 部分を赤に
  (set-face-background 'magit-item-highlight "#000000") ; 選択項目ハイライトがうっとうしいので背景色と同化
  )

(when (require 'redo+ nil t)
  (global-set-key (kbd  "C-.") 'redo))

;; elispにて変数・関数
(defun elisp-mode-hooks ()
  ;;  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (require 'eldoc-extension nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)
;;(print-to-string emacs-lisp-mode-hook)
;;; *grep*で編集できるようにする
(when (require 'grep-edit nil t)
  (add-hook 'grep-setup-hook
            (lambda ()
              (define-key grep-mode-map
                (kbd "C-c C-c") 'grep-edit-finish-edit))))

;; http://d.hatena.ne.jp/sandai/20120303/p1
;; カーソル付近にあるEmacs Lispの関数や変数のヘルプをエコーエリアに表示
;; http://www.emacswiki.org/emacs/eldoc-extension.el
(when (require 'eldoc-extension nil t)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-minor-mode-string ""))

;;;  @ C-eldoc
;; C言語の関数や変数のヘルプをエコーエリアに表示
(when (require 'c-eldoc nil t)
  (add-hook 'c-mode-hook
            (lambda ()
              (set (make-local-variable 'eldoc-idle-delay) 0.2)
              (set (make-local-variable 'eldoc-minor-mode-string) "")
              (c-turn-on-eldoc-mode))))

(require 'hideshow)

(require 'goto-chg)
(global-set-key [f5] 'backupgoto-last-change)
(global-set-key [S-f5] 'goto-last-cha-rverse)

;; grep結果バッファでのカーソル移動でダイナミックにファイルを開いてくれる
(when (require 'color-grep)
  (setq color-grep-sync-kill-buffer t))

;; @ e2wm
;; http://d.hatena.ne.jp/kiwanami/20100528/1275038929
(require 'e2wm nil t)
(global-set-key (kbd "M-+") 'e2wm:start-management)

;; @ w3m
(when (require 'w3m nil t)
  (require 'w3m-load)
  (setq w3m-use-cookies t)              ;クッキーを使う
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-key-binding 'info)
  (global-set-key (kbd "C-x C-b") 'bs-show)
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
             (message "not movie.")))))
  )

;; ruby
(require 'ruby-electric nil t)          ; 括弧の自動挿入
(when (require 'ruby-block nil t)       ; end に対応する行のハイライト
  (setq ruby-block-highlight nil))
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
          "nim03027/real"))
                ;;; bitly
  (setq twittering-tinyurl-service 'bit.ly)
  (setq twittering-bitly-login "o_2qpahq4o2g")
  (setq twittering-bitly-api-key "R_569bbc2f545c7bc590b6d3b35c0554e3")
  ;; 実行キー追加 デフォルトは[f4]キー
  ;;(global-set-key (kbd "C-c t u") 'twittering-tinyurl-replace-at-point)
  )

;; 再帰的にgrep
;; http://www.clear-code.com/blog/2011/2/16.html
(require 'grep)
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
(require 'dired-x)
;; diredから"r"でファイル名をインライン編集する
(require 'wdired)
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

;; @ tabbar
(when (require 'tabbar nil t)
  (require 'cl)
  (require 'tabbar-ruler)
  (require 'tabbar-extension)

  ;; scratch buffer以外をまとめてタブに表示する
  (setq tabbar-buffer-groups-function nil)
  (setq tabbar-buffer-list-function
        (lambda ()
          (remove-if
           (lambda(buffer)
             (unless (string-match (buffer-name buffer)
                                   "\\(*Apropos*\\|*shell*\\|*eshell*\\|*Customize*\\)")
               (find (aref (buffer-name buffer) 0) " *")))
           (buffer-list))))

  ;; tabbarを有効にする
  (tabbar-mode t)
  (defvar my-tabbar-displayed-buffers
    '("*scratch*" "*Messages*" "*Backtrace*" "*Colors*" "*Faces*" "*vc-")
    "*Regexps matches buffer names always included tabs.")

  ;; ボタンをシンプルにする
  (setq tabbar-home-button-enabled "")
  (setq tabbar-scroll-right-button-enabled "")
  (setq tabbar-scroll-left-button-enabled "")
  (setq tabbar-scroll-right-button-disabled "")
  (setq tabbar-scroll-left-button-disabled "")
  (defun my-tabbar-buffer-help-on-tab (tab)
    "Return the help string shown when mouse is onto TAB."
    (if tabbar--buffer-show-groups
        (let* ((tabset (tabbar-tab-tabset tab))
               (tab (tabbar-selected-tab tabset)))
          (format "mouse-1: switch to buffer %S in group [%s]"
                  (buffer-name (tabbar-tab-value tab)) tabset))
      (format "\
                               mouse-1: switch to buffer %S\n\
                               mouse-2: kill this buffer\n\
                               mouse-3: delete other windows"
              (buffer-name (tabbar-tab-value tab)))))

  (defun my-tabbar-buffer-select-tab (event tab)
    "On mouse EVENT, select TAB."
    (let ((mouse-button (event-basic-type event))
          (buffer (tabbar-tab-value tab)))
      (cond
       ((eq mouse-button 'mouse-2)
        (with-current-buffer buffer
          (kill-buffer)))
       ((eq mouse-button 'mouse-3)
        (delete-other-windows))
       (t
        (switch-to-buffer buffer)))
      ;; Don't show groups.
      (tabbar-buffer-show-groups nil)))

  (setq tabbar-help-on-tab-function 'my-tabbar-buffer-help-on-tab)
  (setq tabbar-select-tab-function 'my-tabbar-buffer-select-tab)


  ;; Ctrl-Tab, Ctrl-Shift-Tab でタブを切り替える
  (dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
    (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))
  (defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
    `(defun ,name (arg)
       (interactive "P")
       ,do-always
       (if (equal nil arg)
           ,on-no-prefix
         ,on-prefix)))
  (defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
  (defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
  (global-set-key [(control tab)] 'shk-tabbar-next)
  (global-set-key [(control shift tab)] 'shk-tabbar-prev)

  ;; タブ上でマウスホイール操作無効
  (tabbar-mwheel-mode -1)
  
  ;; 左に表示されるボタンを無効化
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))

  ;; タブの長さ
  (setq tabbar-separator '(1.5))
  ;; タブ表示欄の見た目（フェイス）
  (set-face-attribute 'tabbar-default nil
                      :background "SystemMenuBar")

  ;; 選択タブの見た目（フェイス）
  (set-face-attribute 'tabbar-selected nil
                      :foreground "red1"
                      :background "SystemMenuBar"
                      :box (list
                            :line-width 1
                            :color "gray80"
                            :style 'released-button)
                      :overline "#F3F2EF"
                      :weight 'bold
                      :family "Inconsolata")

  ;; 非選択タブの見た目（フェイス）
  (set-face-attribute 'tabbar-unselected nil
                      :foreground "white"
                      :background "SystemMenuBar"
                      :box (list
                            :line-width 1
                            :color "gray80"
                            :style 'released-button)
                      :overline "#F3F2EF"
                      :family "Inconsolata")

  ;; タブ間隔の調整
  (set-face-attribute 'tabbar-separator nil
                      :height 0.1))

(setq-default bm-buffers-persistence nil)
(setq bm-restore-repository-on-load t)
(when (require 'bm nil t)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restre)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (global-set-key (kbd "M-SPC") 'bm-toggle)
  (global-set-key (kbd "M-c") 'bm-previous)
  (global-set-key (kbd "M-j") 'bm-next))

;;; @ paredit.el
(when (require 'paredit nil t)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'c-mode-hook 'enable-paredit-mode))

;;; @ elscreen.el
(when (require 'elscreen nil t)
  (elscreen-start)
  (setq elscreen-prefix-key "\C-z")
  (global-set-key (kbd "C-z C-l") 'elscreen-next)
  (global-set-key (kbd "C-z C-h") 'elscreen-previous)

  (defun elscreen-frame-title-update ()
    (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
      (let* ((screen-list (sort (elscreen-get-screen-list) '<))
             (screen-to-name-alist (elscreen-get-screen-to-name-alist))
             (title (mapconcat
                     (lambda (screen)
                       (format "%d%s %s"
                               screen (elscreen-status-label screen)
                               (get-alist screen screen-to-name-alist)))
                     screen-list " ")))
        (if (fboundp 'set-frame-name)
            (set-frame-name title)
          (setq frame-title-format title)))))

  (eval-after-load "elscreen"
    '(add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update))

;;; 色設定
  (defface elscreen-tab-other-screen-face
    '((((type x w32 mac) (class color))
       :background "Gray85" :foreground "Gray50")
      (((class color))
       (:background "red" :foreground "black" :underline t)))
    "Face for tabs other than current screen one."
    :group 'elscreen))
;;; @ color-theme (emacs 23)
(if (string-match "^23\." emacs-version)
    (when (require 'color-theme nil t)
      ))

;;; @ newsticker
(when (require 'newsticker nil t)
  (autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
  (autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t))

;; @ howm
(when (require 'howm-mode)
  (mapc
   (lambda (f)
     (autoload f
       "howm" "Hitori Otegaru Wiki Modoki" t))
   '(howm-menu howm-list-all howm-list-recent
               howm-list-grep howm-create
               howm-keyword-to-kill-ring))
  
  ;; howmメモの保存先
  (setq howm-directory (concat user-emacs-directory "howm"))
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

 ;;; @ rainbow-delimiters
(when (require 'rainbow-delimiters nil t)
  (global-rainbow-delimiters-mode t)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode))

;;; @ helm
(require 'helm-config nil t)

;;; @ smartchr.el
;; http://tech.kayac.com/archive/emacs-tips-smartchr.html
(require 'smartchr nil t)
(global-set-key (kbd "=") (smartchr '(" = "  " == " "=")))

;;; @ flymake
(when (require 'flymake nil t)
  (require 'rfringe nil t)
  ;; GUIの警告は表示しない
  (setq flymake-gui-warnings-enabled nil)
  ;; M-p/M-n で警告/エラー行の移動
  (global-set-key "\M-p" 'flymake-goto-prev-error)
  (global-set-key "\M-n" 'flymake-goto-next-error)
  
  ;; 警告エラー行の表示
  (global-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
  ;; Makefileがあれば利用し、なければ直接コマンドを実行する設定
  ;; Makefileの種類を定義
  (defvar flymake-makefile-filenames
    '("Makefile" "makefile" "GNUmakefile")
    "File names for make.")
  
  ;; Makefileがなければコマンドを直接利用するコマンドラインを作成
  (defun flymake-get-make-gcc-cmdline (source base-dir)
    (let (found)
      (dolist (makefile flymake-makefile-filenames)
        (if (file-readable-p (concat base-dir "/" makefile))
            (setq found t)))
      (if found
          (list "make"
                (list "-s"
                      "-C"
                      base-dir
                      (concat "CHK_SOURCES=" source)
                      "SYNTAX_CHECK_MODE=1"
                      "check-syntax"))
        (list (if (string= (file-name-extension source) "c") "gcc" "g++")
              (list "-o"
                    "/dev/null"
                    "-fsyntax-only"
                    "-Wall"
                    "-Wextra"
                    source)))))

  ;; Flymake初期化関数の作成
  (defun flymake-simple-make-gcc-init-impl
    (create-temp-f use-relative-base-dir
                   use-relative-source build-file-name get-cmdline-f)
    "Create syntax check command line for a directly checked source file. use CREATE-TEMP-F for createing temp copy."
    (let ((args nil)
          (source-file-name buffer-file-name)
          (buildfile-dir (file-name-directory source-file-name)))
      (if buildfile-dir
          (let* ((temp-source-file-name
                  (flymake-init-create-temp-buffer-copy create-temp-f)))
            (setq args
                  (flymake-get-syntax-check-program-args
                   temp-source-file-name
                   buildfile-dir
                   use-relative-base-dir
                   use-relative-source
                   get-cmdline-f))))
      args))

  ;; 初期化関数を定義
  (defun flymake-simple-make-gcc-init ()
    (message "%s" (flymake-simple-make-gcc-init-impl
                   'flymake-create-temp-inplace t t "Makefile"
                   'flymake-get-make-gcc-cmdline))
    (flymake-simple-make-gcc-init-impl
     'flymake-create-temp-inplace t t "Makefile"
     'flymake-get-make-gcc-cmdline))

  ;; 拡張子 .c, .cpp, c++などのときに上記関数を利用する
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'"
                 flymake-simple-make-gcc-init))
  (add-hook 'c-mode-hook
            '(lambda ()
               (flymake-mode t)))
  (add-hook 'c++-mode-hook
            '(lambda ()
               (flymake-mode t)))

  ;; http://d.hatena.ne.jp/nushio/20071201
  (defun flymake-show-and-sit ()
    "Displays the error/warning for the current line in the minibuffer"
    (interactive)
    (progn
      (let* ( (line-no             (flymake-current-line-no) )
              (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
              (count               (length line-err-info-list)))
        (while (> count 0)
          (when line-err-info-list
            (let* ((file       (flymake-ler-file      (nth (1- count) line-err-info-list)))
                   (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
                   (text       (flymake-ler-text      (nth (1- count) line-err-info-list)))
                   (line       (flymake-ler-line      (nth (1- count) line-err-info-list))))
              (message "[%s] %s" line text)))
          (setq count (1- count)))))
    (sit-for 60.0))
  (global-set-key (kbd "C-c d") 'flymake-show-and-sit)
  
  ;; flymakeの色を変更
  (set-face-background 'flymake-errline "red4")
  (set-face-background 'flymake-warnline "dark slate blue")

  ;; gotoした際にエラーメッセージをminibufferに表示する
  (defun display-error-message ()
    (message (get-char-property (point) 'help-echo)))
  (defadvice flymake-goto-prev-error (after flymake-goto-prev-error-display-message)
    (display-error-message))
  (defadvice flymake-goto-next-error (after flymake-goto-next-error-display-message)
    (display-error-message))
  (ad-activate 'flymake-goto-prev-error 'flymake-goto-prev-error-display-message)
  (ad-activate 'flymake-goto-next-error 'flymake-goto-next-error-display-message))

;;; @ shell-pop
(when  (require 'shell-pop nil t)
  (shell-pop-set-internal-mode "ansi-term")
  (shell-pop-set-internal-mode-shell "/bin/zsh")
  (shell-pop-set-window-height 50)
  (defvar ansi-term-after-hook nil)
  (defadvice ansi-term (after ansi-term-after-advice (arg))
    "run hook as after advice"
    (run-hooks 'ansi-term-after-hook))
  (ad-activate 'ansi-term))

;;; @ guide-key
(when (require 'guide-key nil t )
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
  (setq guide-key/highlight-command-regexp "rectangle")
  (guide-key-mode 1))  ; guide-key-mode を有効にする

(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; センテンスの終了である ';' を入力したら、自動改行+インデント
             (c-toggle-auto-hungry-state 1)
             ;; RET キーで自動改行+インデント
             (define-key c-mode-base-map "\C-m" 'newline-and-indent)))

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
               (gtags-make-complete-list))))

  ;;; - リスト11 kill-lineで行が連結したときにインデントを減らす
(defadvice kill-line (before kill-line-and-fixup activate)
  (when (and (not (bolp)) (eolp))
    (forward-char)
    (fixup-whitespace)
    (backward-char)))

  ;;; 同一バッファ名にディレクトリ付与
(when (require 'uniquify nil t )
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;;; @ menu-tree
(setq menu-tree-coding-system 'utf-8)
(require 'menu-tree)

;;; @ bongo
;;; http://pastelwill.jp/wiki/doku.php?id=emacs#bongo_itunes_の代わりに_emacs_を使う
(autoload 'bongo "bongo"
  "Start Bongo by switching to a Bongo buffer." t)
(defun load-path-setter (path-list target-path)
  (dolist (x path-list) (add-to-list target-path x)))
(load-path-setter
 '("/usr/local/Cellar/mplayer/1.1/bin"
   "/Applications/VLC.app/Contents/MacOS")
 'exec-path)
(require 'bongo-mplayer)
(setq bongo-enabled-backends '(mplayer))

;;; suggest-restart
(when (require 'suggest-restart nil t)
  (suggest-restart t))

;;; @ simplenote
;;; http://blog.serverworks.co.jp/tech/2010/06/30/emacs-iphone-simplenote-and-vuvuzela/
(when (require 'simplenote nil t)
  ;;  (setq simplenote-email “email@company.com”) ; ログイン用メールアドレス
  ;;  (setq simplenote-password “yourpassword”) ; ログイン用パスワード
  ;; (setq simplenote-directory “ディレクトリパス”)
  ;;  (simplenote-setup)
  )

(when (require 'midnight nil t)
  (setq clean-buffer-list-buffer-names
        (append clean-buffer-list-kill-buffer-names
                '("note.txt")))
  (setq clean-buffer-list-delay-general 1)
  (setq clean-buffer-list-delay-special 10))

(when
    (require 'cycle-buffer nil t)
  (eval-after-load "cycle-buffer"
    '(progn
       (setq cycle-buffer-allow-visible t)
       (setq cycle-buffer-show-length 12)
       (setq cycle-buffer-show-format '(" <(%s)>" . " %s"))))
  (global-set-key (kbd "M-]") 'cycle-buffer)
  (global-set-key (kbd "M-[") 'cycle-buffer-backward))

  ;;; 日本語マニュアル
(add-to-list 'Info-directory-list "~/.emacs.d/info")

;;; @ egg
;;(when (executable-find "git")
;;  (require 'egg nil t))

;;; @ yatex
(when (require 'yatex nil t)
  (setq auto-mode-alist
        (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
  (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
  (setq dvi2-command "open -a Preview"
        tex-command "~/Library/TeXShop/bin/platex2pdf-euc")
  ;; 文章作成時の漢字コードの設定
  ;; 1 = Shift_JIS, 2 = ISO-2022-JP, 3 = EUC-JP, 4 = UTF-8
  ;; default は 2
  (setq YaTeX-kanji-code 4) ; euc-jp
  (setq YaTeX-use-AMS-LaTeX t)
  ;; RefTeXをYaTeXで使えるようにする
  (add-hook 'yatex-mode-hook '(lambda () (reftex-mode t)))
  (setq reftex-enable-partial-scans t)
  (setq reftex-save-parse-info t)
  (setq reftex-use-multiple-selection-buffers t)
  ;;RefTeXにおいて数式の引用を\eqrefにする
  (setq reftex-label-alist '((nil ?e nil "~\\eqref{%s}" nil nil)))
  (setq bibtex-command "jbibtex -kanji=euc")
  (setq jbibtex-command "jbibtex -kanji=euc")
  ;;印刷コマンド
  (setq dviprint-command-format "dvips -f %f %t %s | lpr")
  (setq dviprint-from-format "-p %b")
  (setq dviprint-to-format "-l %e")
  
  ;;section型補間の規定値
  (setq section-name "section")
  ;;Begin ショートカットの禁止(いきなり補完入力)
  (setq YaTeX-no-begend-shortcut t)

  ;;数式の色付け
  (if (featurep 'hilit19)
      (hilit-translate
       formula 'DeepPink1))

  ;;数式モードの";"補間の強化
  (setq YaTeX-math-sign-alist-private
        '(("q"         "quad"          "__")
          ("qq"        "qquad"         "____")
          ("ls"        "varlimsup"     "___\nlim")
          ("li"        "varliminf"     "lim\n---")
          ("il"        "varinjlim"     "lim\n-->")
                                        ;("st"        "text{ s.~t. }" "s.t.")
          ("bigop"     "bigoplus"      "_\n(+)~")
          ("bigot"     "bigotimes"     "_\n(x)\n ~")
          ("pl"        "varprojlim"    "lim\n<--")))
  ;;数式モードの","補間
  (setq YaTeX-math-funcs-list
        '(("s"	"sin"           "sin")
          ("c"    "cos"           "cos") 
          ("t"    "tan"           "tan")
          ("hs"	"sinh"          "sinh")
          ("hc"   "cosh"          "cosh")
          ("ht"   "tanh"          "tanh")
          ("S"	"arcsin"        "arcsin")
          ("C"    "arccos"        "arccos")
          ("T"    "arctan"        "arctan")
          ("se"   "sec"           "sec")
          ("cs"   "csc"           "csc")
          ("cot"  "cot"           "cot")
          ("l"    "ln"            "ln")
          ("L"    "log"           "log")
          ("e"    "exp"           "exp")
          ("M"    "max"           "max")
          ("m"    "min"           "min")
          ("su"   "sup"           "sup")
          ("in"   "inf"           "inf")
          ("di"   "dim"           "dim")
          ("de"   "det"           "det")
          ("i"    "imath"         "i")
          ("j"    "jmath"         "j")
          ("I"    "Im"            "Im")
          ("R"    "Re"            "Re")))
  (setq YaTeX-math-key-list-private
        '(("," . YaTeX-math-funcs-list)))
  ;;新規ファイル作成時に自動挿入されるファイル名
  (setq YaTeX-template-file 
        (expand-file-name "~/.emacs.d/template/template.tex")))

;;; 自動改行の抑制
(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode t)
             (setq fill-colum 70)))

;;; @ latex
(add-hook 'yatex-mode-hook
          '(lambda ()
             (set-buffer-file-coding-system 'euc-jp)))
;; TeX数式プレビュー
;;(autoload 'latex-math-preview "latex-math-preview" nil t)
(autoload 'latex-math-preview-expression "latex-math-preview" nil t)
(autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
(autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
(autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)
(setq latex-math-preview-tex-to-png-for-preview '(platex dvipng))
(setq latex-math-preview-tex-to-png-for-save '(platex dvipng))
(setq latex-math-preview-tex-to-eps-for-save '(platex dvips-to-eps))
(setq latex-math-preview-tex-to-ps-for-save '(platex dvips-to-ps))
(setq latex-math-preview-beamer-to-png '(platex dvipdfmx gs-to-png))
(add-hook 'yatex-mode-hook
          '(lambda ()
             (YaTeX-define-key "p" 'latex-math-preview-expression)
             (YaTeX-define-key "\C-p" 'latex-math-preview-save-image-file)
             (YaTeX-define-key "j" 'latex-math-preview-insert-symbol)
             (YaTeX-define-key "\C-j" 'latex-math-preview-last-symbol-again)
             (YaTeX-define-key "\C-b" 'latex-math-preview-beamer-frame)))
(setq latex-math-preview-in-math-mode-p-func 'YaTeX-in-math-mode-p)

;;; ----------------------------------------
;;; @ 各種言語別設定
(add-hook
 'c-mode-common-hook
 (lambda ()
   ;; BSDスタイルをベースにする
   (c-set-style "bsd")
   ;; スペースでインデントをする
   (setq indent-tabs-mode nil)
   (setq c-basic-offset 4)
   ;; 自動改行（auto-new-line）と
   ;; 連続する空白の一括削除（hungry-delete）を
   ;; 有効にする
   (c-toggle-auto-hungry-state 1)
   ;; CamelCaseの語でも単語単位に分解して編集する
   ;; GtkWindow         => Gtk Window
   ;; EmacsFrameClass   => Emacs Frame Class
   ;; NSGraphicsContext => NS Graphics Context
   (subword-mode 1)))

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   ;; スペースでインデントをする
   (setq indent-tabs-mode nil)))

