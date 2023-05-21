#lang racket

(require racket/set
        "splitting.rkt"
         "placement.rkt"
         "guillotine/splitting.rkt"
         "structs.rkt"
         "utils.rkt")

(provide process-data)

(define (process-data order-items)
    (define material-width 2800)
    (define material-height 2070)
    (define groupped-items (group-by order-item-struct-material-id order-items))
    (define sorted-item-list (map sort-by-long-side groupped-items))
    (define sorted-groupped-items-list (apply append sorted-item-list))
    (define result (get-cutting-patterns material-width material-height sorted-groupped-items-list))
    result)

(define (get-cutting-patterns material-width material-height order-items)
    (define sheets (list (rectangular-sheet-struct material-width
                                                   material-height
                                                   (order-item-struct-material-id (first order-items))
                                                   (list (space-struct 0 0 material-width material-height)))))
    (define cutting-patterns (list '()))
    (define unused-items '())
    (define used-areas '())
    (for ([item order-items])
        (cond
            [(not (item-fits-on-material? item material-width material-height))
                (set! unused-items (append unused-items (list item)))
                (println (format "item ~a, ~a doesn't fit in any available material sheet size. Adding it to unused items." (order-item-struct-width item) (order-item-struct-height item)))]
            [else
                (define placement (find-placement item sheets))
                (when (empty? placement)
                    (set! sheets (append sheets (list (rectangular-sheet-struct 
                                                            material-width material-height
                                                            (order-item-struct-material-id item)
                                                            (list (space-struct 0 0 material-width material-height))))))
                    (set! cutting-patterns (append cutting-patterns (list '())))
                    (set! placement (find-placement item sheets)))
                (define b-item (list-ref placement 0))
                (define b-sheet (list-ref placement 1))
                (define b-space (list-ref placement 2))
                (define-values (curr-item best-sheet best-space)
                                (values b-item b-sheet b-space))
                (define-values (best-x best-y best-width best-height) 
                            (values (space-struct-x best-space) (space-struct-y best-space)
                                    (space-struct-width best-space) (space-struct-height best-space)))
                (define best-sheet-index (index-of sheets best-sheet))
                ; Add the item to the cutting pattern of the best sheet
                (println (format "Item ~a x ~a placed at coordinates (x=~a, y=~a) on cutting sheet ~a"
                            (order-item-struct-width curr-item) (order-item-struct-height curr-item)
                            best-x best-y best-sheet-index))
                (define new-cutting-pattern (cutting-pattern-struct best-x best-y (order-item-struct-width curr-item) (order-item-struct-height curr-item)))
                (set! cutting-patterns
                    (map (lambda (i sheet-pattern)
                                    (if (equal? i best-sheet-index)
                                        (append sheet-pattern (list new-cutting-pattern))
                                        sheet-pattern))
                                (range (length cutting-patterns)) cutting-patterns))
                
                (define used-width (if (<= (+ best-x  (margin) (order-item-struct-width curr-item))
                                          material-width)
                                        (+ (order-item-struct-width curr-item) (margin))
                                        (+ (order-item-struct-width curr-item) 
                                            (- material-width (+ best-x  (order-item-struct-width curr-item))))))

                (define used-height (if (<= (+ best-y (margin) (order-item-struct-height curr-item))
                                          material-height)
                                        (+ (order-item-struct-height curr-item) (margin))
                                        (+ (order-item-struct-height curr-item) 
                                            (- material-height (+ best-y (order-item-struct-height curr-item))))))

                (set! used-areas (append used-areas (list (cons (order-item-struct-material-id curr-item)
                                                                (* used-width used-height)))))

                (define splitting-func (if (guillotine-cuts) generate-available-spaces-guillotine generate-available-spaces))
                (define spaces-after-splitting (splitting-func best-sheet (last (list-ref cutting-patterns best-sheet-index)) best-space))
                ; (set-rectangular-sheet-struct-available-spaces! best-sheet (merge-adjacent-spaces spaces-after-splitting))
                (set-rectangular-sheet-struct-available-spaces! best-sheet spaces-after-splitting)
                ]))

    (define (calculate-material-waste material-sheets)
        (define filtered-used-areas (filter (lambda (area) 
                                            (eq? (car area) 
                                                  (rectangular-sheet-struct-material-id (first material-sheets)))) 
                                used-areas))
        (define material-used-area (apply + (map cdr filtered-used-areas)))

        ;just multiply area of one to 
        (define material-total-area
            (for/foldr ([acc 0])
                        ([sheet material-sheets])
                            (+ acc (* (rectangular-sheet-struct-width sheet) 
                                    (rectangular-sheet-struct-height sheet)))))
        (define material-unused-area (- material-total-area material-used-area))
        ( / (round (* (exact->inexact (* (/ material-unused-area material-total-area) 100)) 10000))  10000.0))


    (define (get-sheets-summary) 
        (define material-ids (remove-duplicates (map rectangular-sheet-struct-material-id sheets)))
        (define summary (map (lambda (material)
                                (define material-sheets (filter (lambda (sheet) (= material (rectangular-sheet-struct-material-id sheet))) sheets))
                                (list
                                    material
                                    (length material-sheets)
                                    (length (filter (lambda (item) (= (order-item-struct-material-id item) material)) unused-items))
                                    (calculate-material-waste material-sheets)))
            material-ids))
        (for ([row summary])
            (println (list (second row) (fourth row))))
        summary)

    (make-hash
        (list
            (cons 'cutting-patterns cutting-patterns)
            (cons 'number-of-sheets (length sheets))
            (cons 'unused-items unused-items)
            (cons 'sheets sheets)
            (cons 'sheets-summary (get-sheets-summary)))))
