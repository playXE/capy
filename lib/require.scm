;; Implementation of REQUIRE, for loading libraries.
;;
;; Provides:
;;     require
;;     current-capy-root
;;     current-library-resolver 
;;     current-require-path
;;     current-require-path-suffix-optional
;;     current-require-path-suffixes
;;     current-require-path-suffixes-compiled
;;
;; Names starting with require/ are "not exported."

;; require/resolve-ext: string x (list-of String) -> string+{#f}
;; Given a base name and a list of extentions, return the first
;; name that exists, or #f if none do.
(define (require/resolve-ext base exts)
  (if (null? exts) #f
      (let ((src (string-append base "." (car exts))))
        (if (file-exists? src) src
            (require/resolve-ext base (cdr exts))))))

;; require/resolve-dir: String x String x Boolean -> String+{#f}
;; Given the name of a module and a directory name, resolves that into
;; either an absolute path to an existing file, or returns #f.  If
;; noisy, it complains when loading source because the fasl is stale.

(define (require/resolve-dir name dir noisy)
  (define base
    (if (absolute-file-name? dir)
        (string-append dir "/" name)
        (string-append (current-capy-root) "/" dir "/" name)))
  (define (warn src)
    (if noisy
        (format #t "Warning: loading source in favor of stale fasl file: ~a~%"
                src))
    src)
  (if (and (current-require-path-suffix-optional)
           (file-exists? base))
      base
      (let ((src
             (require/resolve-ext base
                                  (current-require-path-suffixes)))
            (fasl
             (require/resolve-ext base
                                  (current-require-path-suffixes-compiled))))
        (if (and fasl src)
            (if (file-newer? fasl src) fasl (warn src))
            (or fasl src)))))

;; require/resolve: String+Symbol x [Boolean] -> String+{#f}
;; Given the name of a library, return a filename if found, or #f otherwise.

(define (require/resolve name . quiet)
  (let ((name (cond
                ((string? name) name)
                ((symbol? name) (symbol->string name))
                ((pair? name) (symbol->string (car name)))
                (else (error "Bad argument to resolver: " name))))
        (noisy (or (null? quiet) (not (car quiet)))))
    (let loop ((path (current-require-path)))
      (if (null? path) #f
          (or (require/resolve-dir name (car path) noisy)
              (loop (cdr path)))))))

(define *require-loaded-files* '())

;; clear-require-loaded-files! resets the "require database" so that
;; subsequent invocations of require will load libraries afresh.

(define (clear-require-loaded-files!)
  (set! *require-loaded-files* '()))

;; require: String+Symbol -> Boolean

(define (require name)
  (cond
    (((current-library-resolver) name)
        => (lambda (file)
             (if (member file *require-loaded-files*)
                 #f
                 (begin
                  (set! *require-loaded-files*
                        (cons file *require-loaded-files*))
                  (load file)
                  #t))))
    (else
      (error "Could not locate library: " name))))

(define current-library-resolver
  (make-parameter require/resolve (lambda (val)
    (unless (procedure? val)
      (error "current-library-resolver: not a procedure: " val))
      
    val)))

(define current-capy-root
  (make-parameter "."))

(define current-require-path
  (make-parameter '()))

(define current-require-path-suffix-optional
  (make-parameter #f))
                 
(define current-require-path-suffixes
  (make-parameter '("sch" "scm" "ss")))

(define current-require-path-suffixes-compiled
  (make-parameter '("fasl")))