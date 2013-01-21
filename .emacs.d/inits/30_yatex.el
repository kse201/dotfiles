;;; @ yatex
(when (require 'yatex nil t)

  ;; auto-comlete-latex.el
  (when (require 'auto-complete-latex nil t)
    )

  ;; outline
  (defun latex-outline-level ()
    (interactive)
    (let ((str nil))
      (looking-at outline-regexp)
      (setq str (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
      (cond ;; キーワード に 階層 を返す
       ((string-match "documentclass" str) 1)
       ((string-match "documentstyle" str) 1)
       ((string-match "part" str) 2)
       ((string-match "chapter" str) 3)
       ((string-match "appendix" str) 3)
       ((string-match "subsubsection" str) 6)
       ((string-match "subsection" str) 5)
       ((string-match "section" str) 4)
       (t (+ 6 (length str)))
       )))

  (add-hook 'yatex-mode-hook
            '(lambda ()
               (setq outline-level 'latex-outline-level)
               (make-local-variable 'outline-regexp)
               (setq outline-regexp
                     (concat "[ \t]*\\\\\\(documentstyle\\|documentclass\\|"
                             "part\\|chapter\\|appendix\\|section\\|subsection\\|subsubsection\\)"
                             "\\*?[ \t]*[[{]"))
               (outline-minor-mode t)))
  
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
             (auto-fill-mode nil)
             (setq auto-fill-function nil)))

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
(add-hook 'yatex-mode-hook 'turn-on-reftex)
