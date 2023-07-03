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
                ; (println (format "item ~a, ~a doesn't fit in any available material sheet size. Adding it to unused items." (order-item-struct-width item) (order-item-struct-height item)))
                ]
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
                (set-rectangular-sheet-struct-available-spaces! best-sheet spaces-after-splitting)
                ]))

    (define (calculate-cutting-map-used-area cutting-map)
        (define used-areas (map (lambda (item)
                                    (define-values (item-x item-y item-width item-height) 
                                            (values (cutting-pattern-struct-x item) (cutting-pattern-struct-y item)
                                                    (cutting-pattern-struct-width item) (cutting-pattern-struct-height item)))
                                    (define used-width (if (<= (+ item-x (margin) item-width)
                                                                material-width)
                                                                (+ item-width (margin))
                                                                (+ item-width (- material-width (+ item-x item-width)))))

                                    (define used-height (if (<= (+ item-y (margin) item-height)
                                                                material-height)
                                                                (+ item-height (margin))
                                                                (+ item-height (- material-height (+ item-y item-height)))))
                                    (* used-width used-height))
                                cutting-map))
        
        (apply + used-areas))

    (define (calculate-material-waste material-sheets)
        (define material-sheets-indexes (map (lambda (sheet) (index-of sheets sheet)) material-sheets))
        (define material-cutting-maps (filter (lambda (cutting-map) (member (index-of cutting-patterns cutting-map) material-sheets-indexes)) cutting-patterns))
        (define material-used-areas (map (lambda (cutting-map)
                                            (calculate-cutting-map-used-area cutting-map))
                                        material-cutting-maps))
        (define material-waste-per-map (map (lambda (used-area) 
                                                (define unused-area (- (* material-height material-width) used-area))
                                                (* (/ unused-area (* material-height material-width)) 100)) material-used-areas))
    
        (define average-material-waste (/ (apply + material-waste-per-map) (length material-waste-per-map)))
        ( / (round (* (exact->inexact average-material-waste) 100))  100.0))

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
         summary)

    (make-hash
        (list
            (cons 'cutting-patterns cutting-patterns)
            (cons 'number-of-sheets (length sheets))
            (cons 'unused-items unused-items)
            (cons 'sheets sheets)
            (cons 'sheets-summary (get-sheets-summary)))))
