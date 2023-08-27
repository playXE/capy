(let loop ((i 0))
    (if (< i 10)
        (begin 
            (loop (+ i 1)))))