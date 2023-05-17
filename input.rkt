#lang racket

(require csv-reading	
        "structs.rkt")

(provide try-get-order-items)

(define (valid-entry? value)
  (and (integer? (string->number value))
       (> (string->number value) 0)))

(define (valid-material-and-quantity? row)
  (and 
       (or (valid-entry? (list-ref row 16))
           (string? (list-ref row 16)))
       (valid-entry? (list-ref row 17))))

;not filtering out rows shorter than 18 yet to avoid errors with list-ref in valid-material-and-quantity?
; TODO: think what to do with rows with 0 on width or height - now filtering out
(define (clean-data rows)
    (define clean-data (filter (lambda (row) 
                (or (< (length row) 18)
                    (and
                        (valid-material-and-quantity? row)
                        (valid-entry? (list-ref row 3))
                        (valid-entry? (list-ref row 4)))))
        rows))
    clean-data)

(define (valid-row? row)
    (and 
        (= (length row) 18)
        (or (equal? (list-ref row 15) "AA")
            (equal? (list-ref row 15) "NN"))))

(define (validate-rows data)
    (define invalid-rows '())
    (for-each (lambda (i row)
                (when (not (valid-row? row)) (set! invalid-rows (append invalid-rows (list (+ i 2))))))
        (range (length data))
        data)
    invalid-rows)

(define (try-get-order-items data)
  (define cleaned-data (clean-data data))
  (define invalid-rows (validate-rows cleaned-data))
  (define result '())
  (if (not (empty? invalid-rows))
      invalid-rows
      (for ([row cleaned-data])
        (let process-row ([n (string->number (list-ref row 17))])
          (cond 
           [(= n 0) '()]
           [else 
            (set! result
                  (append result 
                          (list (order-item-struct 
                                 (string->number (list-ref row 3))
                                 (string->number (list-ref row 4))
                                 (string->number (list-ref row 16))
                                 (if (equal? (list-ref row 15) "AA") #t #f)))))
            (process-row (sub1 n))]))))
  result)