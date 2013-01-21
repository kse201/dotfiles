;; @ tabbar
(when (require 'tabbar nil t)
  (require 'cl)
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
    '("*Messages*" "*Backtrace*" "*Colors*" "*Faces*" "*vc-")
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

  ;; ctrl-tab, ctrl-shift-tab でタブを切り替える
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
  (setq tabbar-separator '(0.5))
  ;; タブ表示欄の見た目（フェイス）
  (set-face-attribute 'tabbar-default nil
                      :background "gray80")

  ;; 選択タブの見た目（フェイス）
  (set-face-attribute 'tabbar-selected nil
                      :foreground "red"
                      :background "gray70"
                      :box (list
                            :line-width 1
                            :color "gray80"
                            :style 'released-button)
                      :overline "#F3F2EF"
                      :weight 'bold
                      :family "Inconsolata")

  ;; 非選択タブの見た目（フェイス）
  (set-face-attribute 'tabbar-unselected nil
                      :foreground "Black"
                      :background "gray90"
                      :box (list
                            :line-width 1
                            :color "gray80"
                            :style 'released-button)
                      :overline "#F3F2EF"
                      :family "Inconsolata")

  ;; タブ間隔の調整
  (set-face-attribute 'tabbar-separator nil
                      :height 0.5))
