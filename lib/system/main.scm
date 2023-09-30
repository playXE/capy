(let ([port (file-io/open-file "test.scm" 'input 'text)])
    (let loop ([ch (io/get-char port #f)])
        (if (eof-object? ch)
            (begin
                (io/close-port port)
                'done)
            (begin
                (print ch)
                (loop (io/get-char port #f))))))