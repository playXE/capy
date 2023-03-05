(load "compat-chez.scm")
;; (load "compat-scheme48.scm") ; but first ,open structures listed in this file 

;;======================================================================
;;
;; Initialize the system:
;;
;;======================================================================

(load "require.scm")
(current-require-path (list "."))
(load "runtime.scm")
(load "expander.scm")

;; Compile standard libraries.
;; This only needs to be done once.

(ex:expand-file "standard-libraries.scm" "standard-libraries.exp")
(load "standard-libraries.exp")
(ex:expand-file "file2.ss" "file2.exp")
(load "file2.exp")

