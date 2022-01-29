(define (string-to-KMP-procedure str)
  (let ([len (string-length str)])
    (if (zero? len) (lambda (text) 0)
      (let* ([len-1 (1- len)] [offset (make-vector len-1 0)])
        (let loop ([i 1])
          (if (< i len-1)
            (let ([c (string-ref str i)])
              (let loop ([d (vector-ref offset (1- i))])
                (cond
                  [(char=? c (string-ref str d))
                   (vector-set! offset i (1+ d))]
                  [(positive? d)
                   (loop (vector-ref offset (1- d)))]))
              (loop (1+ i)))))
        (lambda (text)
          (let ([l (string-length text)])
            (let loop ([i 0] [j 0])
              (cond
                [(>= i len) (- j i)] [(>= j l) #f]
                [(char=? (string-ref str i) (string-ref text j))
                 (loop (1+ i) (1+ j))]
                [(positive? i)
                 (loop (vector-ref offset (1- i)) j)]
                [else (loop 0 (1+ j))]))))))))

(define (match-demo str text)
  (cond [((string-to-KMP-procedure str) text) => (lambda (n)
         (display text) (newline) (display (make-string n #\space))
         (display str) (newline))]
        [else (display "Match failed.") (newline)]))

(match-demo "abcdabd" "abc abcdab abcdabcdabde")
