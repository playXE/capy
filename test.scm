(call-with-values 
  (lambda ()
    (call-with-values 
      (lambda () (values 1 2))
      (lambda results 
        (apply values results))))
  (lambda (A B)
    42
  )
)
