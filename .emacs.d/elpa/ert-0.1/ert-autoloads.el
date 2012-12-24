;;; ert-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ert-run-tests-interactively deftest) "ert" "ert.el"
;;;;;;  (20693 21717))
;;; Generated autoloads from ert.el

(autoload 'deftest "ert" "\
Define NAME (a symbol) as a test.

\(fn NAME () [:documentation DOCSTRING] [:expected-result TYPE] BODY...)" nil (quote macro))

(autoload 'ert-run-tests-interactively "ert" "\
Run the tests specified by SELECTOR and display the results in a buffer.

\(fn SELECTOR &optional OUTPUT-BUFFER-NAME MESSAGE-FN)" t nil)

;;;***

;;;### (autoloads nil nil ("ert-pkg.el") (20693 21717 242126))

;;;***

(provide 'ert-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ert-autoloads.el ends here
