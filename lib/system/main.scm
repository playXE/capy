(ex:expand-file "lib/system/expander/standard-library.scm" "standard-library.exp")

(ex:repl 
'(
    (library (foo bar)
        (export)
        (import (core primitives))
        (define (foo x y) (+ x y)))))

#f