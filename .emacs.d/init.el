;;
;; init.el
;;
;; Last Change: 20-Jan-2013.
;;
;;;------------------------------
(eval-when-compile (require 'cl))
(require 'cl)

;; Language.
(set-language-environment 'Japanese)
;; Coding system.
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; ファイル名
(when (eq system-type 'darwin)          ; Mac のファイル名設定
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
(when (eq system-type 'w32)             ; Windowsのファイル名設定
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

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

;;; ネットワーク設定
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
    (setq url-proxy-services nil)))
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

(defun open-current-dir-with-finder ()
  "編集中のファイルのディレクトリを開く"
  (interactive)
  (shell-command (concat "open .")))

;;; http://shibayu36.hatenablog.com/entry/2012/12/18/161455
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))

;;; http://d.hatena.ne.jp/yascentur/20110621/1308585547
(defun split-window-vertically-n (num_wins)
  "ウィンドウを縦 n 分割"
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  "ウィンドウを横 n 分割"
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
;;; ------------------------------------------------------------------
;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "conf" "public_repos" "elpa" "elisp" "themes")
;;; @ auto-install 
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; install-elisp.el互換モードにする
  (auto-install-compatibility-setup)
  ;; ediff関連のバッファを1つのフレームにまとめる
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;; @ init-loader
(when (require 'init-loader nil t)
  (init-loader-load "~/.emacs.d/inits")
  (defun init-loader-re-load (re dir &optional sort)
    (let ((load-path (cons dir load-path)))
      (dolist (el (init-loader--re-load-files re dir sort))
        (condition-case e
            (let ((time (car (benchmark-run (load (file-name-sans-extension el))))))
              (init-loader-log (format "loaded %s. %s" (locate-library el) time)))
          (error
           ;; (init-loader-error-log (error-message-string e)) ;削除
           (init-loader-error-log (format "%s. %s" (locate-library el) (error-message-string e))) ;追加
           ))))))
;;; ------------------------------
;;; バッファ自動再読み込み
(global-auto-revert-mode 1)
;;; 大文字小文字返還
;;; C-x C-u/C-l 大文字小文字 upper / lower
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; 自動スペルチェック
(defun spell-check ()
  (setq-default flyspell-mode t)
  (setq ispell-dictionaryonary "american")
  (setq ispell-program-name "aspell")
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil))

;; yesは面倒なのでyで十分
(defalias 'yes-or-no-p 'y-or-n-p)
;; もろもろ非表示
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

;; Localeに合わせた環境の設定
(set-locale-environment nil)
(show-paren-mode 1)

;; http://d.hatena.ne.jp/syohex/20121225/1356449561
;; look command with auto-complete
(defun my/ac-look ()
  "`look' command with auto-completelook"
  (interactive)
  (unless (executable-find "look")
    (error "Please install `look' command"))
  (let ((word (thing-at-point 'word)))
    (unless word
      (error "not found word"))
    (let ((cmd (format "look %s" word)))
      (with-temp-buffer
        (call-process-shell-command cmd nil t)
        (split-string-and-unquote (buffer-string) "\n")))))

(defun ac-look ()
  (interactive)
  (let ((ac-menu-height 50)
        (ac-candidate-limit t))
    (auto-complete '(ac-source-look))))

(defvar ac-source-look
  '((candidates . my/ac-look)
    (requires . 2)))

(global-set-key (kbd "C-M-l") 'ac-look) ;; 好きなキーにしてください
;; ---------------------------------

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

;;; プロセス削除確認させない
(defadvice save-buffers-kill-terminal (before my-save-buffers-kill-terminal activate)
  (when (process-list)
    (dolist (p (process-list))
      (set-process-query-on-exit-flag p nil))))
;; GCを減らして軽くする
(setq gc-cons-threshold (* 5242880 2))
;; ログの記録行数を増やす
(setq message-log-max 10000)

;;; ------------------------------
;;; @ 各種設定
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
(setq large-file-worning-threshold (* 25 1024 1024))
;; ミニバッファで入力を取り消しても履歴に残す
;; 誤って取り消して入力が失われるのを防ぐため
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))

(setq max-specpdl-size 6000)
(setq max-lisp-eval-depth 1000)
;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; *.elを保存時、自動バイトコンパイル
;; (add-hook 'after-save-hook
;;         (lambda ()
;;          (let ((file (buffer-file-name)))
;;           (when (string-match ".*\\.el$" file)
;;            (byte-compile-file file))))

;;; @ key bind ------------------------------ 
;
;; 自動でファイルを挿入する
(auto-insert-mode t)

;;; ido.el
(ido-mode 1)                            ; コマンドがidoのものに置き換わる
(ido-everywhere 1)                      ; バッファ名・ファイル名入力全てがidoに置き換わる

;; evalした結果全部表示
(setq eval-expression-print-length nil)

(eval-when-compile
  (require 'cl))

;; 最終行に必ず１行挿入する
(setq require-final-newline t)

;; バッファの最後でnewlineで新規行を追加するのを禁止
(setq next-line-add-newlines nil)

;; 終了時にautosavefileを消す
(setq delete-auto-save-files t)

;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; 部分一致の補間機能を使う
(if (string-match "^23\." emacs-version)
    (partial-completion-mode t))
;;; 略語展開・補完を行うコマンドをまとめる
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially ;ファイル名の一部
        try-complete-file-name          ;ファイル名全体
        try-expand-all-abbrevs          ; 静的略語展開
        try-expand-dabbrev              ; 動的略語展開(カレントバッファ)
        try-expand-dabbrev-all-buffers  ; 動的略語展開 (全バッファ)
        try-expand-dabbrev-from-kill    ; 動的略語展開(キルリング : M-w / C-w の履歴
        try-complete-lisp-symbol-partially ; Lisp シンボル名の一部
        try-complete-lisp-symbol        ; Lispシンボル名全体
        ))

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

(setq ring-bell-function 'my-bell-function)
(setq ring-bell-function 'ignore)



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

