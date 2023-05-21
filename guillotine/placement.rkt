#lang racket

(require "../structs.rkt"
         "../utils.rkt"
         math)

(provide (all-defined-out))

(define (fits-original-orientation-guillotine? item space sheet)
    (define available-spaces (rectangular-sheet-struct-available-spaces sheet))
    (define intersecting-spaces? (remove space available-spaces spaces-equal?))
    (define intersecting-original 
        (if (empty? intersecting-spaces?)
            '()
            (filter (lambda (x) 
                    (intersects? (cutting-pattern-struct (space-struct-x space)
                                                            (space-struct-y space)
                                                            (order-item-struct-width item)
                                                            (order-item-struct-height item)) x))
                intersecting-spaces?)))
    (and  
         (empty? intersecting-original)
         (<= (order-item-struct-width item) (space-struct-width space))
         (<= (order-item-struct-height item) (space-struct-height space))))

(define (fits-rotated-guillotine? item space sheet)
    (define available-spaces (rectangular-sheet-struct-available-spaces sheet))
    (define intersecting-spaces? (remove space available-spaces spaces-equal?))
    (define intersecting-rotated 
        (if (empty? intersecting-spaces?)
            '()   
            (filter (lambda (space) 
                        (intersects? (cutting-pattern-struct (space-struct-x space)
                                                                (space-struct-y space)
                                                                (order-item-struct-height item)
                                                                (order-item-struct-width item)) space))
                intersecting-spaces?)))
    (and  (empty? intersecting-rotated)
          (<= (order-item-struct-height item) (space-struct-width space))
          (<= (order-item-struct-width item) (space-struct-height space))))

(define (get-valid-spaces-guillotine item sheet)
    (define available-spaces (rectangular-sheet-struct-available-spaces sheet))
    (if (order-item-struct-rotate item)
        (filter (lambda (space)
                    (or (fits-original-orientation-guillotine? item space sheet)
                        (fits-rotated-guillotine? item space sheet)))
            available-spaces)
        (filter (lambda (space)
                    (fits-original-orientation-guillotine? item space sheet))
            available-spaces)))


(define (get-orientation-guillotine item space sheet)
    (cond
        ;===best orientation heuristics
        ; [(and (fits-original-orientation-guillotine? item space sheet)
        ;       (fits-rotated-guillotine? item space sheet))
        ;     (compare-aspect-ratios item space)]
        ; ======
        [(fits-original-orientation-guillotine? item space sheet) 'original]
        [(fits-rotated-guillotine? item space sheet) 'rotated]))


; (define (space-equals-order-item? item space)
;     (define-values (item-width item-height item-rotate) 
;                     (values (order-item-struct-width item) (order-item-struct-height item) 
;                             (order-item-struct-rotate item)))
;     (define-values (space-width space-height) 
;                     (values (space-struct-width space) (space-struct-height space)))
;     (define (item-matches-original? item space)
;         (and (= item-width space-width)
;              (= item-height space-height)))
;     (define (item-matches-rotated? item space)
;         (and (= item-height space-width)
;              (= item-width space-height)))
;     (if item-rotate
;         (or (item-matches-original? item space) 
;             (item-matches-rotated? item space))
;         (item-matches-original? item space)))


                                            