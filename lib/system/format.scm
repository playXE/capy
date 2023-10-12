;;An SRFI-28 and SRFI-29 compliant version of format.  It requires
;;SRFI-23 for error reporting.
(define format
  (lambda (format-string . objects)
    (let ((buffer (open-output-string)))
      (let loop ((format-list (string->list format-string))
                 (objects objects)
                 (object-override #f))
        (cond ((null? format-list) (get-output-string buffer))
              ((char=? (car format-list) #\~)
               (cond ((null? (cdr format-list))
                      (error 'format "Incomplete escape sequence"))
                     ((char-numeric? (cadr format-list))
                      (let posloop ((fl (cddr format-list))
                                    (pos (string->number
                                          (string (cadr format-list)))))
                        (cond ((null? fl)
                               (error 'format "Incomplete escape sequence"))
                              ((and (eq? (car fl) '#\@)
                                    (null? (cdr fl)))
                                    (error 'format "Incomplete escape sequence"))
                              ((and (eq? (car fl) '#\@)
                                    (eq? (cadr fl) '#\*))
                               (loop (cddr fl) objects (list-ref objects pos)))
                              (else
                                (posloop (cdr fl)
                                         (+ (* 10 pos)
                                            (string->number
                                             (string (car fl)))))))))
                     (else
                       (case (cadr format-list)
                         ((#\a)
                          (cond (object-override
                                 (begin
                                   (display object-override buffer)
                                   (loop (cddr format-list) objects #f)))
                                ((null? objects)
                                 (error 'format "No value for escape sequence"))
                                (else
                                  (begin
                                    (display (car objects) buffer)
                                    (loop (cddr format-list)
                                          (cdr objects) #f)))))
                         ((#\s)
                          (cond (object-override
                                 (begin
                                   (display object-override buffer)
                                   (loop (cddr format-list) objects #f)))
                                ((null? objects)
                                 (error 'format "No value for escape sequence"))
                                (else
                                  (begin
                                    (write (car objects) buffer)
                                    (loop (cddr format-list)
                                          (cdr objects) #f)))))
                         ((#\%)
                          (if object-override
                              (error 'format "Escape sequence following positional override does not require a value"))
                          (display #\newline buffer)
                          (loop (cddr format-list) objects #f))
                        ((#\~)
                          (if object-override
                              (error 'format "Escape sequence following positional override does not require a value"))
                          (display #\~ buffer)
                          (loop (cddr format-list) objects #f))
                         (else
                           (error 'format "Unrecognized escape sequence"))))))
              (else (display (car format-list) buffer)
                    (loop (cdr format-list) objects #f)))))))
