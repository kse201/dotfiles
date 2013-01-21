;;; --------------------------------------------------------------------------------
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

;; スクリプト保存時、自動的にchmod+x
;;; ファイル先頭に#!が含まれているとき
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; emacs-lisp mode
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq indent-tabs-mode nil)))
