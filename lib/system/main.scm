(let ([port (open-text-output-file "file.txt")])
    (put-string port "Hello, World!\n")
    (flush-output-port port)
    (close-port port))

(let ([port (open-text-input-file "file.txt")])
    (let ([str (read-line port)])
        (close-port port)
        str))
