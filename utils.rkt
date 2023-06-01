#lang racket
(require "structs.rkt")

(provide (all-defined-out))

(define margin (make-parameter 0))
(define guillotine-cuts (make-parameter #f))
(define valid-spaces-func (make-parameter #f))
(define get-placement-func (make-parameter #f))
(define get-orientation-func (make-parameter #f))

(define (item-fits-on-material? item material-width material-height)
    (or (and (<= (order-item-struct-width item) material-width) 
             (<= (order-item-struct-height item) material-height))
        (and (order-item-struct-rotate item)
              (<= (order-item-struct-height item) material-width)
              (<= (order-item-struct-width item) material-height))))

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

(define (longest-side order-item)
    (cond
        [(> (order-item-struct-height order-item) (order-item-struct-width order-item)) (order-item-struct-height order-item)]
        [else 
            (order-item-struct-width order-item)]))

(define (shortest-side order-item)
    (cond
        [(<= (order-item-struct-height order-item) (order-item-struct-width order-item)) (order-item-struct-height order-item)]
        [else 
            (order-item-struct-width order-item)]))

(define (sort-by-long-side group)
  (cond [(empty? group) empty]
        [else
         (let ((pivot (first group))
               (rest (rest group)))
           (append (sort-by-long-side (filter (lambda (x) (> (longest-side x) 
                                                             (longest-side pivot) )) rest))
                   (list pivot)
                   (sort-by-long-side (filter (lambda (x) (<= (longest-side x)
                                                              (longest-side pivot))) rest))))]))
(define (aspect-ratio order-item)
    (/ (longest-side order-item) (shortest-side order-item)))

(define (sort-by-aspect-ratio group)
  (cond [(empty? group) empty]
        [else
         (let ((pivot (first group))
               (rest (rest group)))
           (append (sort-by-aspect-ratio (filter (lambda (x) (> (aspect-ratio x)
                                                             (aspect-ratio pivot) )) rest))
                   (list pivot)
                   (sort-by-aspect-ratio (filter (lambda (x) (<= (aspect-ratio x)
                                                              (aspect-ratio pivot))) rest))))]))

(define (space-matches-item? item space)
    (define-values (item-x item-y item-width item-height) 
                    (values (cutting-pattern-struct-x item) (cutting-pattern-struct-y item)
                            (cutting-pattern-struct-width item) (cutting-pattern-struct-height item)))
    (define-values (space-x space-y space-width space-height) 
                    (values (space-struct-x space) (space-struct-y space)
                            (space-struct-width space) (space-struct-height space)))
    (and (= item-x space-x)
         (= item-y space-y)
         (= item-width space-width)
         (= item-height space-height)))

(define (intersects? item space)
    (define-values (item-x item-y item-width item-height) 
                    (values (cutting-pattern-struct-x item) (cutting-pattern-struct-y item)
                            (cutting-pattern-struct-width item) (cutting-pattern-struct-height item)))
    (define-values (space-x space-y space-width space-height) 
                    (values (space-struct-x space) (space-struct-y space)
                            (space-struct-width space) (space-struct-height space)))

    (and (< item-x (+ space-x space-width))
         (> (+ item-x item-width (margin)) space-x)
         (< item-y (+ space-y space-height))
         (> (+ item-y item-height (margin)) space-y)))

(define (space-bigger space1 space2)
    (define space1-area (* (space-struct-width space1) (space-struct-height space1)))
    (define space2-area (* (space-struct-width space2) (space-struct-height space2)))
    (cond
        [(> space1-area space2-area) #t]
        [else #f]))

(define (spaces-equal? space1 space2)
    (define-values (space1-x space1-y space1-width space1-height) 
                    (values (space-struct-x space1) (space-struct-y space1)
                            (space-struct-width space1) (space-struct-height space1)))
    (define-values (space2-x space2-y space2-width space2-height) 
                    (values (space-struct-x space2) (space-struct-y space2)
                            (space-struct-width space2) (space-struct-height space2)))
    (and
        (= space1-x space2-x)
        (= space1-y space2-y)
        (= space1-width space2-width)
        (= space1-height space2-height)))

(define (space-equals-order-item? item space)
    (define-values (item-width item-height item-rotate) 
                    (values (order-item-struct-width item) (order-item-struct-height item) 
                            (order-item-struct-rotate item)))
    (define-values (space-width space-height) 
                    (values (space-struct-width space) (space-struct-height space)))
    (define (item-matches-original? item space)
        (and (= item-width space-width)
             (= item-height space-height)))
    (define (item-matches-rotated? item space)
        (and (= item-height space-width)
             (= item-width space-height)))
    (if item-rotate
        (or (item-matches-original? item space) 
            (item-matches-rotated? item space))
        (item-matches-original? item space)))

(define (compare-aspect-ratios item space)
    (define original-aspect-ratio-diff (abs (- (/ (order-item-struct-width item) (order-item-struct-height item))
                                            (/ (space-struct-width space) (space-struct-height space)))))

    (define rotated-aspect-ratio-diff (abs (- (/ (order-item-struct-height item) (order-item-struct-width item))
                                            (/ (space-struct-width space) (space-struct-height space)))))
    (if (< original-aspect-ratio-diff rotated-aspect-ratio-diff)
        'original
        'rotated))