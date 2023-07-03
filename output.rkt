#lang racket

(require csv-reading
        racket/date
        racket/draw
        racket/gui
        "structs.rkt"
        "input.rkt"
        "cutting.rkt")
(provide process-file
         cutting-result->output
         make-csv-file-name
         save-cutting-maps)

(define curr-sheet-number (make-parameter #f))
(define curr-material-id (make-parameter #f))

(define (process-file data)
    (define result (try-get-order-items data))
    (if (integer? (first result))
        (raise-user-error (format "Invalid data error: wrong data format on rows: ~a ~a" 
          (if (>= (string-length (string-join (map number->string result) ", ")) 5)
            (substring (string-join (map number->string result) ", ") 0 4)
            (string-join (map number->string result) ", "))
          (if (>= (string-length (string-join (map number->string result) ", ")) 5)
            "..."
            "")))
        (process-data result)))

(define (cutting-pattern->list pattern)
    (define-values (pattern-x pattern-y pattern-width pattern-height) 
                    (values (cutting-pattern-struct-x pattern) (cutting-pattern-struct-y pattern)
                            (cutting-pattern-struct-width pattern) (cutting-pattern-struct-height pattern)))
    (define results (list (add1 (curr-sheet-number)) (curr-material-id) pattern-x pattern-y pattern-width pattern-height))
    results)

(define (string->csv-string lst)
  (string-join (map (curry format "~a") lst) ","))

(define (output->string output)
  (string-join
   (for/list ((lst output))
     (string-append (string->csv-string lst) "\r\n"))
   ""))

(define (make-csv-file-name)
    (format "~a-cutting-patterns.csv" (string-append
                                          (number->string (date-day (current-date)))
                                          "-"
                                          (number->string (date-month (current-date)))
                                          "-"
                                          (number->string (date-year (current-date))))))

(define (make-pdf-file-name)
    (format "~a-cutting-maps.pdf" (string-append
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
    (define unused-items-list (map (lambda (item) (list "NOT USED" (order-item-struct-material-id item) "X" "X" (order-item-struct-width item) (order-item-struct-height item))) unused-items))
    (define data 
        (map (lambda (sheet-pattern)
                (parameterize* ([curr-sheet-number (index-of cutting-patterns sheet-pattern)]
                                [curr-material-id (rectangular-sheet-struct-material-id (list-ref sheets (curr-sheet-number)))])
                    (map cutting-pattern->list sheet-pattern))) cutting-patterns))
    (define to-csv (append (list csv-header) unused-items-list (apply append data)))
    (define csv-string (output->string to-csv))
    csv-string)

(define (save-cutting-maps cutting-patterns sheets)
  (define page-width 700)
  (define page-height 518)
  (define output-path (put-file "Save file to..." #f #f (make-pdf-file-name) "pdf"))
  (define dc (new pdf-dc%
                [interactive #f]
                [as-eps #f]
                [width page-width]
                [height page-height]
                [output output-path]))
  (send dc start-doc "")
  (send dc set-pen (new pen% [color "black"] [width 0.5] [style 'solid]))
  (for ([page cutting-patterns]
        [i (range 0 (length cutting-patterns))])
    (send dc start-page)
    (define curr-material (rectangular-sheet-struct-material-id (list-ref sheets i)))
    (send dc draw-rectangle 5 5 (/ 2800 4) (/ 2070 4))
    (send dc set-font (make-font #:size 24 #:family 'swiss #:weight 'bold #:size-in-pixels? #t))
    (send dc draw-text (format "Sheet # ~a" (add1 i)) (+ 15 (/ 2800 4)) 5)
    (send dc set-font (make-font #:size 16 #:family 'swiss #:weight 'bold #:size-in-pixels? #t))
    (send dc draw-text (format "Material # ~a" curr-material) (+ 15 (/ 2800 4)) 35)
    (for ([item page])
      (define-values (x y width height) 
        (values (cutting-pattern-struct-x item)
                (cutting-pattern-struct-y item)
                (cutting-pattern-struct-width item)
                (cutting-pattern-struct-height item)))
      (send dc draw-rectangle (+ 5 (/ x 4)) (+ 5 (/ y 4)) (/ width 4) (/ height 4)))
    (send dc end-page))
  (send dc end-doc))