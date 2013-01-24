;;; @ keybind
(when (eq system-type 'darwin)
  ;;(setq mac-command-key-is-meta nil)    ;コマンドキーをメタにしない
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)      ; Optionをメタに
  ;;  (setq mac-command-modifier 'super)    ; コマンドをSuperに
  (setq mac-pass-control-to-system t))   ; コントロールキーをMacではなくEmacsに渡す
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
    ("C-t" . nil)
    ;;    ("C-x C-o" . my-other-window)
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
    ("C-a" . my-beginning-of-indented-line)
    ("M-p" . scroll-down)
    ("M-n" . scroll-up)
    ("M-g" . goto-line)
    ("C-M-h" . delete-horizontal-space)
    ("M-f" . forward-word)
    ))

(windmove-default-keybindings)
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

;; 行頭のC-k一回で行全体を削除
(setq kill-whole-line t)
(iswitchb-mode 1)
;; C-x b でbuffersを選ぶとき便利
(if (string-match "^23\." emacs-version)
    (iswitchb-default-keybindings))
;; C-x,bでバッファリストをミニバッファに表示する
(setq read-buffer-function 'iswitchb-read-buffer) ; バッファ読み取り関数をiswitchbにする
(setq iswitchb-regexp nil)              ; 部分文字列の代わりに正規表現を使う場合はt
(setq iswitchb-prompt-newbuffer nil)    ; 新しいバッファを作成するときにいちいち聞いてこない
;; C-Ret で矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil)
