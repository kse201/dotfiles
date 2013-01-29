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
                    "-fsyntax-only"     ;文法チェックのみ
                    "-Wall"             ;厳しくチェック
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
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (flymake-mode t)))
  (add-hook 'c++-mode-hook
            '(lambda ()
               (flymake-mode t)))

  ;; elisp
  ;; http://d.hatena.ne.jp/sandai/20120304/p2
  ;; Emacs Lisp
  ;; http://www.emacswiki.org/emacs/FlymakeElisp
  (defun flymake-elisp-init ()
    (unless (string-match "^ " (buffer-name))
      (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
             (local-file  (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
        (list
         (expand-file-name invocation-name invocation-directory)
         (list
          "-Q" "--batch" "--eval"
          (prin1-to-string
           (quote
            (dolist (file command-line-args-left)
              (with-temp-buffer
                (insert-file-contents file)
                (condition-case data
                    (scan-sexps (point-min) (point-max))
                  (scan-error
                   (goto-char(nth 2 data))
                   (princ (format "%s:%s: error: Unmatched bracket or quote\n"
                                  file (line-number-at-pos)))))))
            )
           )
          local-file)))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.el$" flymake-elisp-init))

  (add-hook 'emacs-lisp-mode-hook
            ;; workaround for (eq buffer-file-name nil)
            (function (lambda () (if buffer-file-name (flymake-mode)))))

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
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (ad-activate 'flymake-goto-prev-error 'flymake-goto-prev-error-display-message)
  (ad-activate 'flymake-goto-next-error 'flymake-goto-next-error-display-message))


(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'LaTeX-mode 'flycheck-mode)
