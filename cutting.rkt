#lang racket

(require "splitting.rkt")
(require "placement.rkt")
(require "structs.rkt")

(define (process-file order-items)
    (define material-width 2400)
    (define material-height 1200)
    (define groupped-items (group-by order-item-struct-material-id order-items))
    (define sorted-item-list (map sort-by-area-desc groupped-items))
    (define sorted-groupped-items-list (apply append sorted-item-list))
    (define result (get-cutting-patterns material-width material-height sorted-groupped-items-list))
    result)

(define (sort-by-area-desc group)
  (cond [(empty? group) empty]
        [else
         (let ((pivot (first group))
               (rest (rest group)))
           (append (sort-by-area-desc (filter (lambda (x) (> (* (order-item-struct-width x) (order-item-struct-height x)) 
                                                             (* (order-item-struct-width pivot) (order-item-struct-height pivot)))) rest))
                   (list pivot)
                   (sort-by-area-desc (filter (lambda (x) (<= (* (order-item-struct-width x) (order-item-struct-height x))
                                                              (* (order-item-struct-width pivot) (order-item-struct-height pivot)))) rest))))]))
(define (item-fits-on-material? item material-width material-height)
    (or (and (<= (order-item-struct-width item) material-width) 
             (<= (order-item-struct-height item) material-height))
        (and (order-item-struct-rotate item)
              (<= (order-item-struct-height item) material-width)
              (<= (order-item-struct-width item) material-height))))

(define (get-cutting-patterns material-width material-height order-items)
    (define sheets (list (rectangular-sheet-struct material-width
                                                   material-height
                                                   (order-item-struct-material-id (first order-items))
                                                   (list (space-struct 0 0 material-width material-height)))))
    (println sheets)
    (define cutting-patterns (list '()))
    (define unused-items '())
    (for ([item order-items])
        (cond
            [(not (item-fits-on-material? item material-width material-height))
                (set! unused-items (append unused-items (list item)))
                (println (format "item ~a, ~a doesn't fit in any available material sheet size. Adding it to unused items." (order-item-struct-width item) (order-item-struct-height item)))]
            [else
                (define placement (find-placement item sheets))
                (when (empty? placement)
                    (println "placement is empty")
                    (set! sheets (append sheets (list (rectangular-sheet-struct 
                                                            material-width material-height
                                                            (order-item-struct-material-id item)
                                                            (list (space-struct 0 0 material-width material-height))))))
                    (set! cutting-patterns (append cutting-patterns (list '())))
                    (set! placement (find-placement item sheets))
                    (println (format "placement after adding a new sheet ~a" placement)))
                (define b-item (list-ref placement 0))
                (define b-sheet (list-ref placement 1))
                (define b-space (list-ref placement 2))
                (define-values (curr-item best-sheet best-space)
                                (values b-item b-sheet b-space))
                ; unpacking best space
                (define-values (best-x best-y best-width best-height) 
                            (values (space-struct-x best-space) (space-struct-y best-space)
                                    (space-struct-width best-space) (space-struct-height best-space)))
                (define best-sheet-index (index-of sheets best-sheet))
                ; Add the item to the cutting pattern of the best sheet
                (println (format "Item ~a x ~a placed at coordinates (x=~a, y=~a) on cutting sheet ~a"
                            (order-item-struct-width curr-item) (order-item-struct-height curr-item)
                            best-x best-y best-sheet-index))

                (set! cutting-patterns
                    (map (lambda (i sheet-pattern)
                                    (if (equal? i best-sheet-index)
                                        (append sheet-pattern (list (cutting-pattern-struct best-x best-y (order-item-struct-width curr-item) (order-item-struct-height curr-item))))
                                        sheet-pattern))
                                (range (length cutting-patterns)) cutting-patterns))
                (set-rectangular-sheet-struct-available-spaces! best-sheet (generate-available-spaces best-sheet (last (list-ref cutting-patterns best-sheet-index))))
                (println (rectangular-sheet-struct-available-spaces best-sheet))
                ]))

    ;;calculate material waste
    (define total-area
        (for/foldr ([acc 0])
                    ([sheet sheets])
                        (+ acc (* (rectangular-sheet-struct-width sheet) 
                                (rectangular-sheet-struct-height sheet)))))

    (define occupied-area
        (for/foldr ([main-acc 0])
                    ([sheet-pattern cutting-patterns])
                    (for/foldr ([pattern-acc 0])
                                ([pattern sheet-pattern])
                                (+ pattern-acc (* (cutting-pattern-struct-width pattern)
                                                (cutting-pattern-struct-height pattern))))))


    (define unused-area (- total-area occupied-area))
    (define waste-percentage (* (/ unused-area total-area) 100))
    (println (format "waste percentage ~a" (exact->inexact waste-percentage)))
    (make-hash
        (list
            (cons 'cutting-patterns cutting-patterns)
            (cons 'number-of-sheets (length sheets))
            (cons 'waste-percentage waste-percentage)
            (cons 'unused-items unused-items)
            (cons 'sheets sheets))))





;; Testing

(define order-items (list
                     (order-item-struct 490 2000 4784 #t)
                     (order-item-struct 482 1500 4784 #t)
                     (order-item-struct 989 564 4827 #t)
                     (order-item-struct 989 564 4827 #t)
                     (order-item-struct 989 564 4827 #t)
                     (order-item-struct 490 564 4827 #t)
                     (order-item-struct 490 564 4827 #t)
                     (order-item-struct 490 564 4827 #t)
                     (order-item-struct 482 564 4827 #t)
                     (order-item-struct 490 562 4827 #t)
                     (order-item-struct 490 564 4827 #t)
                     (order-item-struct 490 564 4827 #t)
                     (order-item-struct 490 564 4827 #t)
                     (order-item-struct 490 564 4827 #t)
                     (order-item-struct 490 564 4827 #t)
                     (order-item-struct 490 564 4827 #t)
                     (order-item-struct 690 564 4827 #t)
                     (order-item-struct 62 690 4827 #t)))

(define cutting-result (process-file order-items))
(define cutting-patterns (hash-ref cutting-result 'cutting-patterns))
(define num-sheets (hash-ref cutting-result 'number-of-sheets))
(define waste-percentage (hash-ref cutting-result 'waste-percentage))
(define unused-items (hash-ref cutting-result 'unused-items))
(define sheets (hash-ref cutting-result 'sheets))

; Print the cutting patterns
(println "Sheets:")
(for-each (lambda (i sheet-patterns)
            (println (format "Sheet ~a:" (+ i 1)))
            (for-each (lambda (pattern)
                        (define-values (x y item-width item-height) (values (cutting-pattern-struct-x pattern) (cutting-pattern-struct-y pattern)
                                                                            (cutting-pattern-struct-width pattern) (cutting-pattern-struct-height pattern)))
                        (println (format "  Order Item: width=~a, height=~a, position=(~a, ~a), material=~a"
                                         item-width item-height x y (rectangular-sheet-struct-material-id (list-ref sheets i)))))
                      sheet-patterns))
          (range (length sheets))
          cutting-patterns)

(println (format "Number of Sheets: ~a" num-sheets))
(println (format "Waste Percentage: ~a" waste-percentage))
(println "Unused Items:")
(for-each println unused-items)

