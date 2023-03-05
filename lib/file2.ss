
(load "./compat-chez.scm")
(load "./runtime.scm")
(load "./standard-libraries.exp")
(load "expander.exp")

(ex:expand-file "file3.ss" "file3.exp")

(load "file3.exp")