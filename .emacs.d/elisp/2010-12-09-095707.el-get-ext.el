;; (save-window-excursion (shell-command (format "emacs-test -l test-minimum -l %s %s &" buffer-file-name buffer-file-name)))
(defmacro el-get:use (package-name))
(el-get:use el-get)

(defvar el-get-init-files-pattern "~/emacs/init.d/[0-9]*.el")
(defun el-get:packages ()
  ;; TODO convert to elisp
  (mapcar 'cadr (read (concat "(" (shell-command-to-string
                                   (format "grep -h '^(el-get:use' %s"
                                           el-get-init-files-pattern))
                              ")"))))
;; (el-get:packages)

(defun el-get-from-url (url)
  "Register el-get package and install it with guess."
  (interactive "sel-get from URL: ")
  (setq url (el-get-transform-url url))
  (el-get-from url
               (read-string "Package name: " (el-get-guess-name-from-url url))
               (completing-read "Type: " (el-get-get-all-types) nil t
                                (el-get-guess-type-from-url url))))
(defun el-get-from-emacswiki (name)
  "Register el-get package and install it from emacswiki."
  (interactive
   (list (replace-regexp-in-string
          "\\.el$" ""
          (completing-read "el-get from emacswiki: "
                           (and (boundp 'auto-install-package-name-list)
                                auto-install-package-name-list)))))
  (el-get-from nil name "emacswiki"))
;; (el-get-from-emacswiki "123-menu")
;; (el-get-remove "123-menu")
;; (el-get-from-url "http://127.0.0.1:9998/test.el")
;; (el-get-remove "test")


(defun el-get-from (url name type)
  (unless (el-get-recipe-exist-p name)
    (el-get-create-recipe url name type (car el-get-recipe-path)))
  (add-to-list 'el-get-sources (intern name))
  (el-get-install name))
(defun el-get-transform-url (url)
  "Convert http://github.com/user/package to git://github.com/user/package.git"
  (if (string-match "https?://github.com/" url)
      (concat "git"
              (replace-regexp-in-string "^https?" "" url)
              ".git")
    url))
;; (el-get-transform-url "https://github.com/yaotti/anything-with-everything.el")
(defun el-get-recipe-exist-p (package)
  (let ((el-get-sources (el-get-read-all-recipes 'merge)))
    (el-get-package-p package)))

(defmacro case-string-match (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(string-match ,key ,str))
				 keylist))
		't)
	     ,@body)))
       clauses)))
(put 'case-string-match 'lisp-indent-function 1)

(defun el-get-guess-type-from-url (url)
  (case-string-match url
    (("\\.git$") "git")
    (("^lp:" "bzr") "bzr")
    (("svn") "svn")
    ((":pserver:") "cvs")
    (("^ftp:") "ftp")
    (("\\.tar\\.gz$" "\\.tar.bz2$" "tgz$" "tbz$") "http-tar")
    (("mercurial" "/hg/") "hg")
    (("emacswiki.org/") "emacswiki")
    (t "http")))

(defun el-get-guess-name-from-url (url)
  (replace-regexp-in-string "\\.\\(el\\|git\\)$" ""
                            (file-name-nondirectory url)))
(defun el-get-get-all-types ()
  (loop for elt in el-get-methods 
        for i from 0
        when (zerop (mod i 2))
        collect (substring (symbol-name elt) 1)))
(defun el-get-create-recipe-from-string (str dir)
  (find-file-other-window (concat (file-name-as-directory dir) name ".el"))
  (goto-char (point-max))
  (insert str)
  (basic-save-buffer))
(defun el-get-create-recipe (url name type dir)
  (el-get-create-recipe-from-string (el-get-recipe-string url name type) dir))
;; (el-get-recipe-string "git://github.com/kiwanami/emacs-deferred.git" "deferred" "git")
;; (el-get-recipe-string "http://example.com/a.tar.gz" "a" "http-tar")
;; (el-get-recipe-string "http://example.com/a.tar.bz2" "a" "http-tar")
;; (el-get-recipe-string "http://example.com/a.el" "a" "http")
;; (el-get-recipe-string "http://www.emacswiki.org/emacs/download/a.el" "a" "emacswiki")

(defun el-get-recipe-string (url name type)
  (case-string-match type
    (("emacswiki") (el-get-recipe-string:emacswiki name))
    (("http-tar")  (case-string-match url
                     (("\\.tar\\.gz$" "tgz$")
                      (el-get-recipe-string:http-tar-gz url name))
                     (("\\.tar.bz2$" "tbz$")
                      (el-get-recipe-string:http-tar-bz2 url name))))
    (t             (el-get-recipe-string:url url name type))))
(defun el-get-recipe-string:url (url name type)
  (format "\
\(:name %s
       :type %s
       :url %S)" name type url))
(defun el-get-recipe-string:emacswiki (name)
  (format "(:name %s :type emacswiki)" name))
(defun el-get-recipe-string:elpa (name)
  (format "(:name %s :type elpa)" name))
(defun el-get-recipe-string:http-tar (url name options)
  (format "\
\(:name %s
       :type http-tar
       :options %S
       :url %S)" name options url))
(defun el-get-recipe-string:http-tar-gz (url name)
  (el-get-recipe-string:http-tar url name '("xzf")))
(defun el-get-recipe-string:http-tar-bz2 (url name)
  (el-get-recipe-string:http-tar url name '("xjf")))
