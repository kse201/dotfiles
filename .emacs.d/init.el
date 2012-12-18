;;
;; init.el
;;
;; last updated : 2012/12/18

;; Language.
(set-language-environment 'Japanese)

;; Coding system.
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Keys.
(global-set-key "\C-z" 'term)

(if window-system (server-start))
;;; ------------------------------
;;; @ Function
;;; load-path $B$rDI2C$9$k4X?t$rDj5A(B
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; $B%-!<%P%$%s%IEPO?4JN,2=(B
;; http://pod.hatenablog.com/entry/2012/12/10/204538

(defun define-many-keys (key-map key-table)
  (loop for (key . cmd) in key-table
        do (define-key key-map (read-kbd-macro key) cmd)))

(let ((default-directory "~/.emacs.d/elisp/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; $B0z?t$N%G%#%l%/%H%j$H$=$N%5%V%G%#%l%/%H%j$r(Bload-path$B$KDI2C(B
(add-to-load-path "elisp" "conf" "public_repos" "elpa")

;; $B5/F0;~$K%&%#%s%I%&:GBg2=(B
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

;; $BNI$$46$8$K%&%#%s%I%&J,3d(B
(defun good-split-window ()
  (interactive)
  (if (< (window-width) (window-height))
      (split-window-vertically)
    (split-window-horizontally)))

;; $B=DJ,3d%&%#%s%I%&$r0\F0;~$K(B $B%&%#%s%I%&$N2#I}HfN($rJQ2=(B
(defun my-other-window ()
  "Auto resize window when 'other-window"
  (interactive)
  (other-window 1)
  (let (( max-width (truncate (* (screen-width) 0.65))))
    (if (< (window-width) max-width)
        (enlarge-window-horizontally (- max-width (window-width))))))

;;; $B%^!<%/2U=j$K0\F0(B
(defun move-to-mark ()
  (interactive)
  (let ((pos (point)))
    (goto-char (mark))
    (push-mark pos)))

;;; ------------------------------
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-frame-position (selected-frame) 0 0)
         (setq term-setup-hook 'jbr-init)
         (setq window-setup-hook 'jbr-init))
        ((eq ws 'ns)
         ;; for MacBook Air(Late2010) 11inch display
         (set-frame-position (selected-frame) 0 0)
         (set-frame-size (selected-frame) 95 47))))

;; auto-install$B$K$h$C$F%$%s%9%H!<%k$5$l$k(BEmacs Lisp$B$r%m!<%I%Q%9$K2C$($k(B
;; $B%G%U%)%k%H$O!"(B~/.emacs.d/auto-install/
(add-to-list 'load-path "~/.emacs.d/auto-install/")
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; ediff$B4XO"$N%P%C%U%!$r(B1$B$D$N%U%l!<%`$K$^$H$a$k(B
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; $B8=:_9T$K?'$r$D$1$k(B
(global-hl-line-mode 1)
;; $B%b!<%I%i%$%s$K;~9o$rI=<($9$k(B
(display-time)
;; $B%9%?!<%H%"%C%W;~$N%(%3!<NN0h%a%C%;!<%8$NHsI=<((B
(setq inhibit-startup-echo-area-message -1)
;; $B%+!<%=%k$NE@LG$r;_$a$k(B
(blink-cursor-mode 0)
;; $BI>2A$7$?7k2L$rA4ItXa0M(B
(setq eval-comment-region nil)
;; $B9THV9f!&7eHV9f$rI=<($9$k(B
(line-number-mode 1)
;; $B%j!<%8%g%s$K?'$r$D$1$k(B
(transient-mark-mode 1)
;; GC$B$r8:$i$7$F7Z$/$9$k(B ($B%G%U%)%k%H$N(B10$BG\(B)
(setq gc-cons-threshold (* gc-cons-threshold))
;; $B%m%0$N5-O?9T?t$rA}$d$9(B
(setq message-log-max 10000)
;; $B%9%/%m!<%k%P!<$r1&B&$K(B
(set-scroll-bar-mode 'right)
;; $B8=:_$N4X?tL>$r%b!<%I%i%$%s$K(B
(which-function-mode 1)
;; $B%9%/%m!<%k;~$N%+!<%=%k0LCV$N0];}(B
(setq scroll-preserve-screen-position t)
;; $B2hLL%9%/%m!<%k;~$N=EJ#9T?t(B
(setq next-screen-context-lines 1)
;; ------------------------------
;; @ backup
   ;; $BJQ99%U%!%$%k$N%P%C%/%"%C%W(B
(setq make-backup-files t)

;; $BJQ99%U%!%$%k$NHV9f$D$-%P%C%/%"%C%W(B
(setq version-control t)

;; $BJT=8Cf%U%!%$%k$N%P%C%/%"%C%W(B
(setq auto-save-list-file-name t)
(setq auto-save-list-file-prefix t)

;; $BJT=8Cf%U%!%$%k$N%P%C%/%"%C%W@h(B
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; $BJT=8Cf%U%!%$%k$N%P%C%/%"%C%W4V3V!JIC!K(B
(setq auto-save-timeout 30)

;; $BJT=8Cf%U%!%$%k$N%P%C%/%"%C%W4V3V!JBG80!K(B
(setq auto-save-interval 500)

;; $B%P%C%/%"%C%W@$Be?t(B
(setq kept-old-versions 1)
(setq kept-new-versions 2)
;;; $B8E$$%P%C%/%"%C%W%U%!%$%k$O:o=|$7$J$$(B
(setq delete-old-versions nil)

;; zsh $B$r;H$&(B
(setq shell-file-name "/bin/zsh")
(setq enable-recursice-minibuffers t)
;; $B%@%$%"%m%0%\%C%/%9$r;H$o$J$$$h$&$K$9$k(B
(setq use-dialog-box nil)
(defalias 'message-box 'message)
;; $BMzNr$r$?$/$5$sJ]B8$9$k(B
(setq history-length 10000)
;; $B%-!<%9%H%m!<%/$r%(%3!<%(%j%"$KAa$/I=<($9$k(B
(setq echo-keystrokes 0.1)
;; $BBg$-$$%U%!%$%k$r3+$3$&$H$7$?$H$-$K7Y9p$rH/@8$5$;$k(B
;; $B%G%U%)%k%H$O(B10MB$B$J$N$G(B25MB$B$K3HD%$9$k(B
(setq large-file-worning-threshold (* 25 1024 1024))
;; $B%_%K%P%C%U%!$GF~NO$r<h$j>C$7$F$bMzNr$K;D$9(B
;; $B8m$C$F<h$j>C$7$FF~NO$,<:$o$l$k$N$rKI$0$?$a(B
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))
;; yes$B$OLLE]$J$N$G(By$B$G==J,(B
(defalias 'yes-or-no-p 'y-or-n-p)
;; $B$b$m$b$mHsI=<((B
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

;; $B:G6a;H$C$?%U%!%$%k$r%a%K%e!<I=<((B
(recentf-mode t)
;; $B:G6a;H$C$?%U%!%$%k$NI=<(?t(B
(setq recentf-max-menu-items 10)
;; $B:G6a$N%U%!%$%k(B500$B8D8D$rJ]B8$9$k(B
(setq recentf-max-saved-items 500)

;; $B%_%K%P%C%U%!$NMzNr$rJ]B8$9$k(B
(savehist-mode 1)

;; *.el$B$rJ]B8;~!"<+F0%P%$%H%3%s%Q%$%k(B
;;(add-hook 'after-save-hook
;;         (lambda ()
;;          (let ((file (buffer-file-name)))
;;           (when (string-match ".*\\.el$" file)
;;            (byte-compile-file file))))

;; recentf-ext.el
;;(global-set-key (kbd "C-;") 'recentf-open-files )

;; $B%-!<%P%$%s%I(B
(define-many-keys global-map
  '(("C-h" . delete-backward-char)
    ("M-?" . help-for-help)
    ("M-?" . help-for-help)
    ("C-z" . nil)
    ("C-c i" . indent-region ); $B%$%s%G%s%H(B
    ("C-c C-i" . dabbrev-expand ); $BJd40(B
    ("C-c );" . comment-region ); $B%3%a%s%H%"%&%H(B
    ("C-c :" . uncomment-region ); $B%3%a%s%H2r=|(B
    ("C-\\" . nil ); \C-\$B$NF|K\8lF~NO$N@_Dj$rL58z$K$9$k(B
    ("C-m" . newline-and-indent)
    ("C-c l" . toggle-truncate-lines)
    ("C-x C-o" . my-other-window)
    ("M-a" . mark-whole-buffer)
    ("M-y" . backward-kill-word ); $B0l$DA0$NC18l:o=|(B
    ("C-x o" . browse-url-at-point );$B%V%i%&%65/F0(B
    ("C-x C-g" . goto-line ); $B;XDj9T$X0\F0(B
    ("C-c w h" . windmove-left)
    ("C-c w j" . windmove-down)
    ("C-c w k" . windmove-up)
    ("C-c w l" . windmove-right)
    ("C-x SPC" . good-split-window)
    ("C-c C-@" . move-to-mark)
    ))
(global-set-key  (kbd "C-c C-s") '(lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "C-c C-e") '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(define-key mode-specific-map "c" 'compile)

;; $BHO0O;XDj$7$F$$$J$$$H$-!"(BC-w$B$GA0$NC18l$r:o=|(B
;;http://dev.ariel-networks.com/wp/documents/aritcles/emacs/part16
(defadvice kill-region (around kill-word-or-kill-region activate)
  (if (and (interactive-p) transient-mark-mode (not mark-active))
      (backward-kill-word 1)
    ad-do-it))

;; minibuffer$BMQ(B
(define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)

;; Locale$B$K9g$o$;$?4D6-$N@_Dj(B
(set-locale-environment nil)
(show-paren-mode 1)
;; $B2hLLFb$K<}$^$i$J$$;~$O3g8LFb$b8w$i$;$k(B
(setq show-paren-style 'mixed)
(make-face 'paren-mismatch)
(set-face-foreground 'paren-mismatch "white")
(set-face-background 'paren-mismatch "lightcoral")
(setq show-paren-face  'paren-match)
(setq show-paren-mismatch-face 'paren-mismatch)
;; $B6/NO$JJd405!G=$r;H$&(B
;; (partial-completion-mode 1)
;; $B2hA|%U%!%$%k$rI=<($9$k(B
(auto-image-file-mode t)
;; $B<+F0$G%U%!%$%k$rA^F~$9$k(B
(auto-insert-mode t)
;; C-x,b$B$G%P%C%U%!%j%9%H$r%_%K%P%C%U%!$KI=<($9$k(B
(iswitchb-mode 1)
;; C-x b $B$G(Bbuffers$B$rA*$V$H$-JXMx(B
(if (string-match "23" emacs-version)
    (iswitchb-default-keybindings))

(defface my-hl-line-face
  ;; $BGX7J$,(Bdark$B$J$i$PGX7J?'$r:0$K(B
  '((((class clolor) (background dark))
     (:background "NavyBlue" t))
    ;; $BGX7J$,(Blight$B$J$i$PGX7J?'$rNP$K(B
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; Shift + $BLp0u$GHO0OA*Br(B
(if (string-match "23" emacs-version)
    (pc-selection-mode))
;; $BA*BrHO0O$K?'$r$D$1$F8+$?L\$r$o$+$j$d$9$/(B
(transient-mark-mode 1)
;; $B%U%)%s%H@_Dj(B
(set-face-attribute
 'default nil
 :family "Ricty"
 :height 140)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec
  :family "Ricty"))

;; $B%?%$%H%k%P!<$K%U%!%$%k$N%U%k%Q%9$rI=<((B
(setq frame-title-format "%f")
;; $B9THV9fI=<((B
(global-linum-mode t)
;; $B9THV9f$N%U%)!<%^%C%H(B
;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
(set-face-attribute 'linum nil :height 0.8)
(setq linum-format "%4d")
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))
;; TAB$B$NI=<(I}(B 4
(setq-default tab-width 4)
;; $B%$%s%G%s%H$K%?%VJ8;z$r;HMQ$7$J$$(B
(setq-default indent-tabs-mode nil)
;; eval$B$7$?7k2LA4ItI=<((B
(setq eval-expression-print-length nil)
;; $B9TF,$N(BC-k$B0l2s$G9TA4BN$r:o=|(B
(setq kill-whole-line t)
;; $BBP1~3g8L$N%O%$%i%$%H(B
(setq show-paren-delay 0) ; $BI=<($^$G$NIC?t(B
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "red")


;; $B%j!<%8%g%sFb$N9T?t$HJ8;z?t$r%b!<%I%i%$%s$KI=<($9$k(B ($BHO0O;XDj;z$N$_(B)
;; http://d.hatena.nejp/sonota88/20110224/
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

(when (eq system-type 'darwin)
  ;;(setq mac-command-key-is-meta nil)    ;$B%3%^%s%I%-!<$r%a%?$K$7$J$$(B
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)      ; Option$B$r%a%?$K(B
  ;;  (setq mac-command-modifier 'super)    ; $B%3%^%s%I$r(BSuper$B$K(B
  (setq mac-pass-control-to-system t))   ; $B%3%s%H%m!<%k%-!<$r(BMac$B$G$O$J$/(BEmacs$B$KEO$9(B

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(eval-when-compile
  (require 'cl))

;; $B%P%C%F%j!<;DNLI=<((B
(display-battery-mode t)
;; $B%U%!%$%k%5%$%:$rI=<((B
(size-indication-mode t)

;; backup autosave
(setq make-backup-files nil)
(setq auto-save-default t)
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" , (expand-file-name "~/.emacs.d/backups/") t)))
;; $B%*!<%H%;!<%V@8@.$^$G$NIC4V3V(B
(setq auto-save-timeout 15)
;; $B%*!<%H%;!<%V@8@.$^$G$N%?%$%W4V3V(B
(setq auto-save-interval 60)

;; $B:G=*9T$KI,$:#19TA^F~$9$k(B
(setq require-final-newline t)

;; $B%P%C%U%!$N:G8e$G(Bnewline$B$G?75,9T$rDI2C$9$k$N$r6X;_(B
(setq next-line-add-newlines nil)

;; $B=*N;;~$K(Bautosavefile$B$r>C$9(B
(setq delete-auto-save-files t)

;; $BJ]4I;~$KBgJ8;z>.J8;z$r6hJL$7$J$$(B
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; $B9TKv6uGr6/D4(B
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; $BItJ,0lCW$NJd4V5!G=$r;H$&(B
(if (string-match "23" emacs-version)
    (partial-completion-mode t))

;; emacs-lisp mode
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   ;; $B%9%Z!<%9$G%$%s%G%s%H(B
   (setq indent-tabs-mode nil)))

;; M-g $B$G(BM-x goto-line
(global-set-key (kbd "M-g") 'goto-line)

;; $B$h$=$N(Bwindow$B$K%+!<%=%k$rI=<($7$J$$(B
(setq cursor-in-non-selected-windows nil)
;; $B2hA|%U%!%$%k$rI=<((B
(auto-image-file-mode)
;; $BL5BL$J6u9T$r2D;k2=(B
(setq-default indicate-empty-lines t)
;; isearch$B$N%O%$%i%$%H$NH?1~$rNI$/$9$k(B
(setq isearch-lazy-highlight-initial-delay 0)
;; line-space
(setq-default line-spacing 1)

;; $BJ8;z%3!<%I(B
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
;; $B%U%!%$%kL>(B
(when (eq system-type 'darwin)          ; Mac $B$N%U%!%$%kL>@_Dj(B
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
(when (eq system-type 'w32)             ; Windows$B$N%U%!%$%kL>@_Dj(B
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;; C-k$B$G9TA4BN$r:o=|(B

(global-set-key [f12] 'speedbar)

;; $B%U%!%$%k$r3+$$$?;~$K0JA0JT=8$7$F$$$?>l=j$K0\F0(B
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

;; Emacs $B$N(BCommands Hisotry$B$r:F5/F08l$b;HMQ$9$k(B
;; http://qiita.com/items/4b489c0abbb39a5dcc45
(setq desktop-globals-to-save '(extended-command-history))
(setq desktop-files-not-to-save "")
(desktop-save-mode 1)

;; window$B0\F0(B
;; http://d.hatena.ne.jp/tomoya/20120512/1336832436
(windmove-default-keybindings 'super)
;;Mac$BMQ(B
;; (windmove-default-keybindings 'meta)
;; (windmove-default-keybindings) $B0z?t$J$7$N>l9g$O(BShift

;; $B%&%#%s%I%&A`:n$NMzNr$r(Bundo/redo
;; C-c <left> / C-c <right>
(when (fboundp 'winner-mode)
  (winner-mode t))

;; C-a $B$G%$%s%G%s%H$GHt$P$7$?9TF,$K0\F0(B
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

;; $B%9%/%i%C%A%P%C%U%!$N<j5-%a%C%;!<%8>C5n(B
(setq initial-scratch-message "")
;; *scratch*$B$r>C$5$J$$(B
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" $B$r:n@.$7$F(B buffer-list $B$KJ|$j9~$`(B
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
          ;; *scratch* $B%P%C%U%!$G(B kill-buffer $B$7$?$iFbMF$r>C5n$9$k$@$1$K$9$k(B
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))

(add-hook 'after-save-hook
          ;; *scratch* $B%P%C%U%!$NFbMF$rJ]B8$7$?$i(B *scratch* $B%P%C%U%!$r?7$7$/:n$k(B
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))
;; beep$B$r>C$9(B
(defun my-bell-function ()
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                                keyboard-quit mwheel-scroll down up next-line previous-line
                                backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)
(setq ring-bell-function 'ignore)

;; howm$B$N@_Dj(B
(setq howm-menu-lang 'ja)
(global-set-key "\C-x,," 'howm-menu)
(mapc
 (lambda (f)
   (autoload f
     "howm" "Hitori Otegaru Wiki Modoki" t))
 '(howm-menu howm-list-all howm-list-recent
             howm-list-grep howm-create
             howm-keyword-to-kill-ring))

;; C-Ret $B$G6k7AA*Br(B
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; http://qiita.com/items/f0db094fde6640143f42
(if (file-directory-p (expand-file-name "~/bin"))
    (progn
      (add-to-list 'exec-path (expand-file-name "~/bin"))
      (setenv "PATH" (mapconcat 'identity exec-path ":"))))

;; GUI$B$GD>@\%U%!%$%k$r3+$$$?>l9g%U%l!<%`$r:n@.$7$J$$(B
                                        ;(add-hook 'before-make-frame-hook
                                        ;         (lambda ()
                                        ;          (when (eq tabbar-mode t)
                                        ;           (switch-to-buffer (buffer-name))
                                        ;          (delete-this-frame))))

;; http://qiita.com/items/b836e7792be0a7c65fd4
;; C$B7OE}(B,Python$B$K$F(B1$B9T(B80$BJ8;z$rD6$($k$H%O%$%i%$%H(B
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

;; Java$B$G(B1$B9T(B100$BJ8;z$rD6$($k$H%O%$%i%$%H(B
(add-hook 'java-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

;; time-stamp
(when (require 'time-stamp nil t)
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-active t)
  (setq time-stamp-start "last updated : ")
  (setq time-stamp-format "%04y/%02m/%02d")
  (setq time-stamp-end " \\|$"))

;; $B%9%/%j%W%HJ]B8;~!"<+F0E*$K(Bchmod+x
;;; $B%U%!%$%k@hF,$K(B#!$B$,4^$^$l$F$$$k$H$-(B
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(require 'cl)

;; package.el
(when (require 'package nil t)
  ;; $B%P%C%1!<%8%j%]%8%H%j$K(BMarmalade$B$H3+H/<T1?1D$N(BELPA$B$rDI2C(B
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))
(require 'melpa)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; $B5/F0;~$K(BEmacsWiki$B$N%Z!<%8L>$rJd408uJd$K2C$($k(B
            (auto-install-update-emacswiki-package-name t)
            (auto-install-compatibility-setup)))

(add-to-list 'load-path "~/src/emacswikipages/" t)

;; $BMzNr$r<!2s(BEmacs$B5/F0;~$K$bJ]B8$9$k(B
(require 'saveplace )
(savehist-mode 1)

;; $BBP1~$9$k3g8L$r8w$i$;$k(B
(require 'paren)

;; $B:G6a;H$C$?%U%!%$%k$K2C$($J$$%U%!%$%k$r@55,I=8=$G;XDj$9$k(B
(setq recentf-exclude '("/TAGS$" "/var/tmp/"))
(require 'recentf-ext)

(require 'auto-async-byte-compile)
;; $B<+F0%P%$%H%3%s%Q%$%k$rL58z$K$9$k%U%!%$%kL>$N@55,I=8=(B
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
(setq auto-async-byte-compile-exclude-files-regexp "^_")

;; $B:G=*99?7F|$N<+F0A^F~(B
(require 'time-stamp)

;; $B%F!<%^FI$_9~$_@_Dj(B
(if (string-match "23" emacs-version)
    (when (require 'color-theme nil t)
      (color-theme-initialize)
      (require 'color-theme-solarized)
      (color-theme-solarized-light)))

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
    ;; anything$B4XO"%-!<%P%$%s%I(B
    (define-many-keys global-map
      '(( "M-y" . anything-show-kill-ring)
        ( "M-x" . anything-M-x)
        ( "C-; C-;" . anything)
        ( "C-c C-b" . anything-buffers-list) ;; $B%P%C%U%!0lMw(B
        ( "C-s" . anything-occur)
        ( "C-x b" . anything-for-files)
        ( "C-x C-f" . anything-find-files)
        ))

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
     anything-c-moccur-higligt-info-line-flag t ; $B%P%C%U%!$N>pJs$r%O%$%i%$%H$9$k(B
     anything-c-moccur-enable-auto-look-flag t  ;$BA*BrCf$N8uJd$N0LCV$rB>$N(Bwindow$B$KI=<($9$k(B
     anything-c-moccur-enable-initial-pattern t) ;$B5/F0;~$K%]%$%s%H$N0LCV$NC18l$r=i4|%Q%?!<%s$K$9$k(B
    ;; C-M-o $B$K(Banything-c-moccur-occur-by-moccur$B$r3d$jEv$F$k(B
    (global-set-key (kbd "C-M-o") 'anything-c-moccur-occur-by-moccur))

  (when (require 'auto-install nil t)
    (require 'anything-auto-install))

  (when (require 'anything-complete nil t)
    (anything-lisp-complete-symbol-set-timer 150))

  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install)))

(add-to-list 'anything-sources 'anything-c-source-emacs-commands)

;; auto-complete
;; $BJd408uJd$r<+F0%]%C%W%"%C%W(B
(when (require 'auto-complete nil t)
  (global-auto-complete-mode t)
  (setq ac-modes (cons 'js-mode ac-modes)))
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacd.d/ac-dict")
  (setq ac-ignore-case t)
  (ac-config-default)                     ; $B%G%U%)%k%H@_Dj(B
  (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
  (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
  (ac-mode t))

;; open-junk-file.el
(require 'open-junk-file)
(setq open-junk-file-formant "~/junk/%Y/%m-%d-%H%M%S.")

;; color-moccur $B8!:w7k2L$N%j%9%H%"%C%W(B
(when (require 'color-moccur nil t)
  (global-set-key (kbd "M-o") 'occur-by-moccur)
  ;; $B%9%Z!<%96h@Z$j$G(BAND$B8!:w(B
  (setq moccur-split-word t)
  ;; $B%G%#%l%/%H%j8!:w$N$H$-=|30$9$k%U%!%$%k(B
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  ;; MMigemo$B$rMxMQ$G$-$k4D6-$G$"$l$P(BMigemo$B$r;H$&(B
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t ))
    (setq moccur-use-migemo t)))

;; grep$B7k2L%P%C%U%!$G$N%+!<%=%k0\F0$G%@%$%J%_%C%/$K%U%!%$%k$r3+$$$F$/$l$k(B
(when (require 'color-grep nil t)
  (setq color-grep-sync-kill-buffer t)
  ;; M-x grep-find$B$G(BPerl$B$N(Back$B%3%^%s%I$r;H$&$h$&JQ99(B
  (setq grep-find-command "ack --nocolor --nogroup "))

;; undohist $BJT=8MzNr$N5-21(B
(when (require 'undohist nil t)
  (undohist-initialize))

;; multi-term
;;(when (require 'multi-term nil t)
;;  (setq multi-term-program "/bin/zsh"))

;; multi-shell
(when (require 'multi-shell nil t)
  (setq multi-shell-command "/bin/zsh"))

;; $BF10l%U%!%$%kL>$N%P%C%U%!L>$K$O%G%#%l%/%H$rI=<((B
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; yasnippet.el
(when (require 'yasnippet-config nil t)
  (yas/setup "~/.emacs.d/elisp/yasnippet-0.6.1c"))

(require 'summarye)

(when (require 'redo+ nil t)
  (global-set-key (kbd  "C-.") 'redo))

;; elisp$B$K$FJQ?t!&4X?t(B
(defun elisp-mode-hooks ()
  ;;  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (require 'eldoc-extension nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)
;;(print-to-string emacs-lisp-mode-hook)

;; http://d.hatena.ne.jp/sandai/20120303/p1
;; $B%+!<%=%kIU6a$K$"$k(BEmacs Lisp$B$N4X?t$dJQ?t$N%X%k%W$r%(%3!<%(%j%"$KI=<((B
;; http://www.emacswiki.org/emacs/eldoc-extension.el
(when (require 'eldoc-extension nil t)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-minor-mode-string ""))

;; C-eldoc.el
;; C$B8@8l$N4X?t$dJQ?t$N%X%k%W$r%(%3!<%(%j%"$KI=<((B
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

;; grep$B7k2L%P%C%U%!$G$N%+!<%=%k0\F0$G%@%$%J%_%C%/$K%U%!%$%k$r3+$$$F$/$l$k(B
(when (require 'color-grep)
  (setq color-grep-sync-kill-buffer t))

;; e2wm.el
;; http://d.hatena.ne.jp/kiwanami/20100528/1275038929
(require 'e2wm nil t)

;;(require 'w3m)
;;(require 'w3m-load)
;;(setq w3m-use-cookies t)
;; (setq browse-url-browser-function 'w3m-browse-url)
;;(setq w3m-key-binding 'info)
;;(global-set-key (kbd "C-x C-b") 'bs-show)

;; ruby
(require 'ruby-electric nil t)          ; $B3g8L$N<+F0A^F~(B
(when (require 'ruby-block nil t)       ; end $B$KBP1~$9$k9T$N%O%$%i%$%H(B
  (setq ruby-block-highlight nil))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

;; ruby-mode-hook$BMQ$N4X?t$rDj5A(B
(defun ruby-mode-hooks ()
  (inf-ruby-keys)
  (ruby-electric-mode t)
  (ruby-block-mode t))
(add-hook 'ruby-mode-hook 'ruby-mode-hooks) ; ruby-mode-hook$B$KDI2C(B

;; twittering-mode$BFI$_9~$_(B
(when (require 'twittering-mode nil t)
  ;; $B5/F0;~%Q%9%o!<%IG'>Z(B *$BMW(B gpg$B%3%^%s%I(B
  (setq twittering-use-master-password t)
  ;; $B%Q%9%o!<%I0E9f%U%!%$%kJ]B8@hJQ99(B ($B%G%U%)$O%[!<%`%G%#%l%/%H%j(B)
  (setq twittering-private-info-file "~/.emacs.d/twittering-mode.gpg")
  ;; $BI=<($9$k=q<0(B $B6h@Z$j@~$$$l$?$i8+$d$9$$(B
  (setq twittering-status-format "%i @%s %S %p: n %T  [%@]%r %R %f%Ln -------------------------------------------")
  ;; $B%"%$%3%s$rI=<($9$k(B
  (setq twittering-icon-mode t)
  ;; $B%"%$%3%s%5%$%:$rJQ99$9$k(B *48$B0J30$r4uK>$9$k>l9g(B $BMW(B imagemagick$B%3%^%s%I(B
  (setq twittering-convert-fix-size 40)
  ;; $B99?7$NIQEY!JIC!K(B
  (setq twittering-timer-interval 40)
  ;; $B%D%$!<%H<hF@?t(B
  (setq twittering-number-of-tweets-on-retrieval 50)
  ;; o $B$G<!$N(BURL$B$r%V%i%&%6$G%*!<%W%s(B
  (add-hook 'twittering-mode-hook
            (lambda ()
              (local-set-key (kbd "o")
                             (lambda ()
                               (interactive)
                               (twittering-goto-next-uri)
                               (execute-kbd-macro (kbd "C-m"))))))
  (add-to-list 'exec-path "/usr/local/bin")
  ;; $B5/F0;~$K3+$/%?%$%`%i%$%s$N@_Dj(B
  ;; http://christina04.blog.fc2.com/blog-entry-175.html
  (setq twittering-initial-timeline-spec-string
        '(":home"
          ":replies"
          ":favorites"
          ":direct_messages"
          ":search/emacs/"
          "user_name/list_name"))
                ;;; bitly
  (setq twittering-tinyurl-service 'bit.ly)
  (setq twittering-bitly-login "o_2qpahq4o2g")
  (setq twittering-bitly-api-key "R_569bbc2f545c7bc590b6d3b35c0554e3")
  ;; $B<B9T%-!<DI2C(B $B%G%U%)%k%H$O(B[f4]$B%-!<(B
  ;;(global-set-key (kbd "C-c t u") 'twittering-tinyurl-replace-at-point)
  )

;; $B:F5"E*$K(Bgrep
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

;; dired$B$rJXMx$K(B
(require 'dired-x)
;; dired$B$+$i(B"r"$B$G%U%!%$%kL>$r%$%s%i%$%sJT=8$9$k(B
(require 'wdired)
(define-key dired-mode-map "r" 'wdir3ed-change-to-wdired-mode)

;; $B%U%!%$%kL>$,=EJ#$7$F$$$?$i%G%#%l%/%H%jL>$rDI2C(B
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

(when (require 'navi2ch nil t)
  ;; $B%l%9$r$9$Y$FI=<($9$k(B
  (setq navi2ch-article-exist-message-range '(1 . 1000)) ;$B4{B8%9%l(B
  (setq navi2ch-article-new-message-range '(1000 . 1))   ;$B?7%9%l(B
  ;; Board$B%b!<%I$N%l%9?tMs$K%l%9$NA}2C?t$rI=<($9$k(B
  (setq  navi-board-insert-subject-with-diff t)
  ;; Board$B%b!<%I$N%l%9?tMs$K%l%9$NL$FI?t$rI=<($9$k(B
  (setq navi2ch-board-insert-subject-with-unread t)
  ;; $BHD0lMw$N%+%F%4%j$r%G%U%)%k%H$G$9$Y$F3+$$$FI=<($9$k(B
  (setq navi2ch-list-init-open-category nil)
  ;; $B%9%l$r(Bexpire($B:o=|(B)$B$7$J$$(B
  (setq navi2ch-board-expire-date nil)
  ;; $BMzNr$N9T?t$r@)8B$7$J$$(B
  (setq navi2ch-history-max-line nil))

;; tabbar.el
(when (require 'tabbar nil t)
  (require 'cl)
  (require 'tabbar-ruler)
  (require 'tabbar-extension)

  ;; scratch buffer$B0J30$r$^$H$a$F%?%V$KI=<($9$k(B
  (setq tabbar-buffer-groups-function nil)
  (setq tabbar-buffer-list-function
        (lambda ()
          (remove-if
           (lambda(buffer)
             (unless (string-match (buffer-name buffer)
                                   "\\(*Apropos*\\|*shell*\\|*eshell*\\|*Customize*\\)")
               (find (aref (buffer-name buffer) 0) " *")))
           (buffer-list))))

   ;; tabbar$B$rM-8z$K$9$k(B
   (tabbar-mode t)
   (defvar my-tabbar-displayed-buffers
     '("*scratch*" "*Messages*" "*Backtrace*" "*Colors*" "*Faces*" "*vc-")
     "*Regexps matches buffer names always included tabs.")

   ;; $B%\%?%s$r%7%s%W%k$K$9$k(B
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


  ;; Ctrl-Tab, Ctrl-Shift-Tab $B$G%?%V$r@Z$jBX$($k(B
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

  ;; $B%?%V>e$G%^%&%9%[%$!<%kA`:nL58z(B
  (tabbar-mwheel-mode -1)
  
  ;; $B:8$KI=<($5$l$k%\%?%s$rL58z2=(B
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))

  ;; $B%?%V$ND9$5(B
  (setq tabbar-separator '(1.5))
  ;; $B%?%VI=<(Ms$N8+$?L\!J%U%'%$%9!K(B
   (set-face-attribute 'tabbar-default nil
                       :background "SystemMenuBar")

   ;; $BA*Br%?%V$N8+$?L\!J%U%'%$%9!K(B
   (set-face-attribute 'tabbar-selected nil
                       :foreground "red3"
                       :background "SystemMenuBar"
                       :box (list
                             :line-width 1
                             :color "gray80"
                             :style 'released-button)
                       :overline "#F3F2EF"
                       :weight 'bold
                       :family "$B#M#S(B $B#P%4%7%C%/(B"
                       )

   ;; $BHsA*Br%?%V$N8+$?L\!J%U%'%$%9!K(B
   (set-face-attribute 'tabbar-unselected nil
                       :foreground "black"
                       :background "SystemMenuBar"
                       :box (list
                             :line-width 1
                             :color "gray80"
                             :style 'released-button)
                       :overline "#F3F2EF"
                       :family "$B#M#S(B $B#P%4%7%C%/(B"
                       )

   ;; $B%?%V4V3V$ND4@0(B
   (set-face-attribute 'tabbar-separator nil
                       :height 0.1)
   )

;; paredit.el
(when (require 'paredit nil t)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode))


;; newsticker.el
(when (require 'newsticker nil t)
  (autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
  (autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t)

  (setq newsticker-url-list
        '(("Slashdot" "http://rss.slashdot.org/Slashdot/slashdot")
          ("TechCrunch" "http://feeds.feedburner.com/TechCrunch")
          ("CNET Japan" "http://japan.cnet.com/rss/index.rdf")
          )))

(when (require 'howm-mode)

  ;; howm$B%a%b$NJ]B8@h(B
  (setq howm-directory (concat user-emacs-directory "howm"))
  ;; howm-menu$B$N8@8l$rF|K\8l$K(B
  (setq howm-menu-lang 'ja)
  (define-key global-map (kbd "C-x C-, C-, ") 'howm-menu))

(when (require 'rainbow-delimiters nil t)
  (global-rainbow-delimiters-mode t)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode))

;; helm
(require 'helm-config)

;; smartchr.el
;; http://tech.kayac.com/archive/emacs-tips-smartchr.html
(require 'smartchr)
(global-set-key (kbd "=") (smartchr '(" = "  " == " "=")))


(when (require 'flymake nil t)
  ;; GUI$B$N7Y9p$OI=<($7$J$$(B
  (setq flymake-gui-warnings-enabled nil)
  ;; $BA4$F$N%U%!%$%k$G(Bflymake$B$rM-8z2=(B
  (add-hook 'find-file-hook 'flymake-find-file-hook)
  ;; M-p/M-n $B$G7Y9p(B/$B%(%i!<9T$N0\F0(B
  (global-set-key "\M-p" 'flymake-goto-prev-error)
  (global-set-key "\M-n" 'flymake-goto-next-error)

  ;; $B7Y9p%(%i!<9T$NI=<((B
  (global-set-key "\C-cd" 'flymake-display-err-menu-for-current-line)
  ;; Makefile$B$,$"$l$PMxMQ$7!"$J$1$l$PD>@\%3%^%s%I$r<B9T$9$k@_Dj(B
  (defvar flymake-makefile-filenames
    '("Makefile" "makefile" "GNUmakefile")
    "File names for make.")

  ;; Makefile$B$,$J$1$l$P%3%^%s%I$rD>@\MxMQ$9$k%3%^%s%I%i%$%s$r:n@.(B
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
                      "check-synatx"))
        (list (if (string= (file-name-extension source) "c") "gcc" "g++")
              (list "-o"
                    "/dev/null"
                    "-fsyntax-only"
                    "-Wall"
                    "-Wextra"
                    "-pedantic"
                    source)))))

  ;; Flymake$B=i4|2=4X?t$N:n@.(B
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

  ;; $B=i4|2=4X?t$rDj5A(B
  (defun flymake-simple-make-gcc-init ()
    (message "%s" (flymake-simple-make-gcc-init-impl
                   'flymake-create-temp-inplace t t "Makefile"
                   'flymake-get-make-gcc-cmdline))
    (flymake-simple-make-gcc-init-impl
     'flymake-create-temp-inplace t t "Makefile"
     'flymake-get-make-gcc-cmdline))

  ;; $B3HD%;R(B .c, .cpp, c++$B$J$I$N$H$-$K>e5-4X?t$rMxMQ$9$k(B
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
            (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
                   (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
                   (text (flymake-ler-text (nth (1- count) line-err-info-list)))
                   (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
              (message "[%s] %s" line text)))
          (setq count (1- count)))))
    (sit-for 60.0))
  (global-set-key (kbd "C-c d") 'flymake-show-and-sit))

;; shell-pop
;; C-t$B$G(Bshell$B%]%C%W%"%C%W(B
(when  (require 'shell-pop nil t)
  (shell-pop-set-internal-mode "ansi-term")
  (shell-pop-set-internal-mode-shell "/bin/zsh")
  (shell-pop-set-window-height 50)
  (defvar ansi-term-after-hook nil)
  (add-hook 'ansi-term-after-hook
            '(lambda ()
               (define-key term-raw-map (kbd "C-t") 'shell-pop)))
  (defadvice ansi-term (after ansi-term-after-advice (arg))
    "run hook as after advice"
    (run-hooks 'ansi-term-after-hook))
  (ad-activate 'ansi-term)
  (global-set-key (kbd "C-t") 'shell-pop))

(when (require 'guide-key nil t )
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
  (setq guide-key/highlight-command-regexp "rectangle")
  (guide-key-mode 1))  ; guide-key-mode $B$rM-8z$K$9$k(B

(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; $B%;%s%F%s%9$N=*N;$G$"$k(B ';' $B$rF~NO$7$?$i!"<+F02~9T(B+$B%$%s%G%s%H(B
             (c-toggle-auto-hungry-state 1)
             ;; RET $B%-!<$G<+F02~9T(B+$B%$%s%G%s%H(B
             (define-key c-mode-base-map "\C-m" 'newline-and-indent)))

(when (locate-library "gtags") (require 'gtags))
(global-set-key "\M-t" 'gtags-find-tag)     ;$B4X?t$NDj5A85$X(B
(global-set-key "\M-r" 'gtags-find-rtag)    ;$B4X?t$N;2>H@h$X(B
(global-set-key "\M-s" 'gtags-find-symbol)  ;$BJQ?t$NDj5A85(B/$B;2>H@h$X(B
(global-set-key "\M-p" 'gtags-find-pattern)
(global-set-key "\M-f" 'gtags-find-file)    ;$B%U%!%$%k$K%8%c%s%W(B
(global-set-key [?\C-,] 'gtags-pop-stack)   ;$BA0$N%P%C%U%!$KLa$k(B
(add-hook 'c-mode-common-hook
          '(lambda ()
             (gtags-mode 1)
             (gtags-make-complete-list)))

;;- $B%j%9%H(B11 kill-line$B$G9T$,O"7k$7$?$H$-$K%$%s%G%s%H$r8:$i$9(B
(defadvice kill-line (before kill-line-and-fixup activate)
  (when (and (not (bolp)) (eolp))
    (forward-char)
    (fixup-whitespace)
    (backward-char)))

;; $BF10l%P%C%U%!L>$K%G%#%l%/%H%jIUM?(B
(when (require 'uniquify nil t )
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;;; @ menu-tree
(setq menu-tree-coding-system 'utf-8)
(require 'menu-tree)

;; $BF|K\8l%^%K%e%"%k(B
(add-to-list 'Info-directory-list "~/.emacs.d/info")

;;; @ yatex
(when (require 'yatex nil t)
  (setq auto-mode-alist
        (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
  (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
  (setq dvi2-command "open -a Preview"
        tex-command "~/Library/TeXShop/bin/platex2pdf-euc"))
;;; @ latex
(add-hook 'yatex-mode-hook
'(lambda ()
   (set-buffer-file-coding-system 'euc)))
