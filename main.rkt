#lang racket

(require csv-reading
        racket/date
        "structs.rkt"
        "input.rkt"
        "output.rkt"
        "cutting.rkt")
(provide process-file
         cutting-result->output
         make-csv-file-name)

(define curr-sheet-number (make-parameter #f))
(define curr-material-id (make-parameter #f))

(define (cutting-pattern->list pattern)
    (define-values (pattern-x pattern-y pattern-width pattern-height) 
                    (values (cutting-pattern-struct-x pattern) (cutting-pattern-struct-y pattern)
                            (cutting-pattern-struct-width pattern) (cutting-pattern-struct-height pattern)))
    (define results (list (curr-sheet-number) (curr-material-id) pattern-x pattern-y pattern-width pattern-height))
    results)

(define (string->csv-string lst)
  (string-join (map (curry format "~a") lst) ","))

(define (output->string output)
  (string-join
   (for/list ((lst output))
     (string-append (string->csv-string lst) "\r\n"))
   ""))

(define (process-file data)
    (define result (try-get-order-items data))
    (if (integer? (first result))
        (raise-user-error (format "Invalid data error: wrong data format on rows: ~a " (string-join (map number->string result) ", ")))
        (process-data result)))

(define (make-csv-file-name)
    (format "~a-cutting-patterns.csv" (string-append
                                                            (number->string (date-day (current-date)))
                                                            "-"
                                                            (number->string (date-month (current-date)))
                                                            "-"
                                                            (number->string (date-year (current-date))))))

(define (cutting-result->output cutting-result)
    (define cutting-patterns (hash-ref cutting-result 'cutting-patterns))
    (define unused-items (hash-ref cutting-result 'unused-items))
    (define sheets (hash-ref cutting-result 'sheets))
    (define csv-header '("Sheet" "Material" "X" "Y" "Width" "Height"))
    (define data 
        (map (lambda (sheet-pattern)
                (parameterize* ([curr-sheet-number (index-of cutting-patterns sheet-pattern)]
                                [curr-material-id (rectangular-sheet-struct-material-id (list-ref sheets (curr-sheet-number)))])
                    (map cutting-pattern->list sheet-pattern))) cutting-patterns))
    (define to-csv (append (list csv-header) (apply append data)))
    (define csv-string (output->string to-csv))
    csv-string)

; Print the cutting patterns
; (println "Sheets:")
; (for-each (lambda (i sheet-patterns)
;             (println (format "Sheet ~a:" (+ i 1)))
;             (for-each (lambda (pattern)
;                         (define-values (x y item-width item-height) (values (cutting-pattern-struct-x pattern) (cutting-pattern-struct-y pattern)
;                                                                             (cutting-pattern-struct-width pattern) (cutting-pattern-struct-height pattern)))
;                         (println (format "  Order Item: width=~a, height=~a, position=(~a, ~a), material=~a"
;                                          item-width item-height x y (rectangular-sheet-struct-material-id (list-ref sheets i)))))
;                       sheet-patterns))
;           (range (length sheets))
;           cutting-patterns)

; (println (format "Number of Sheets: ~a" num-sheets))
; (println (format "Waste Percentage: ~a" waste-percentage))
; (println "Unused Items:")
; (for-each println unused-items)