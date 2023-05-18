#lang racket
(require "structs.rkt")
(provide generate-available-spaces
         generate-available-spaces-guillotine
         margin)

(define margin (make-parameter 0))

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
         (> (+ item-x item-width) space-x)
         (< item-y (+ space-y space-height))
         (> (+ item-y item-height) space-y)))


(define (split-intersecting-space item space)
    (define intersecting-spaces '())
    (define-values (item-x item-y item-width item-height) 
                    (values (cutting-pattern-struct-x item) (cutting-pattern-struct-y item)
                            (cutting-pattern-struct-width item) (cutting-pattern-struct-height item)))
    (define-values (space-x space-y space-width space-height) 
                    (values (space-struct-x space) (space-struct-y space)
                            (space-struct-width space) (space-struct-height space)))
    (when (intersects? item space)
            ;left space
            (when (> item-x (+ space-x (margin)))
                (set! intersecting-spaces (append intersecting-spaces 
                    (list (space-struct space-x space-y (- item-x space-x (margin)) space-height)))))
            ;right space
            (when (< (+ item-x item-width) (+ space-x space-width (margin)))
                (set! intersecting-spaces (append intersecting-spaces 
                (list (space-struct (+ item-x item-width (margin)) space-y (- space-width (+ item-x item-width (margin) (- space-x))) space-height)))))
            ;bottom space
            (when (> item-y (+ space-y (margin)))
                (set! intersecting-spaces (append intersecting-spaces 
                    (list (space-struct space-x space-y space-width (- item-y space-y (margin)))))))
            ;top space
            (when (< (+ item-y item-height) (+ space-y space-height (margin)))
                (set! intersecting-spaces (append intersecting-spaces
                    (list (space-struct space-x (+ item-y item-height (margin)) space-width (- space-height (+ item-y item-height (margin) (- space-y)))))))))
    intersecting-spaces)
    

(define (space-covered? space remaining-spaces)
    (define covered (filter (lambda (other-space)
                (define-values (space-x space-y space-width space-height) 
                                (values (space-struct-x space) (space-struct-y space)
                                        (space-struct-width space) (space-struct-height space)))
                (define-values (other-space-x other-space-y other-space-width other-space-height) 
                                (values (space-struct-x other-space) (space-struct-y other-space)
                                        (space-struct-width other-space) (space-struct-height other-space)))
                (and (not (equal? space other-space))
                    (>= space-x other-space-x)
                    (>= space-y other-space-y)
                    (<= (+ space-x space-width) (+ other-space-x other-space-width))
                    (<= (+ space-y space-height) (+ other-space-y other-space-height))))
        remaining-spaces))
    covered)


(define (filter-spaces remaining-spaces)
    (define remaining (filter (lambda (space) 
                (empty? (space-covered? space remaining-spaces))) remaining-spaces))
    remaining)

(define (generate-available-spaces sheet item)
    (define available-spaces (rectangular-sheet-struct-available-spaces sheet))
    (define available-spaces-clean (filter (lambda (available-space) 
                                        (not (space-matches-item? item available-space))) available-spaces))
    (define remaining-spaces-temp (map (lambda (space)
                                            (define spaces-from-intersection (split-intersecting-space item space))
                                            (if (empty? spaces-from-intersection)
                                                (list space)
                                                spaces-from-intersection))
                            available-spaces-clean))
    (define remaining-spaces (apply append remaining-spaces-temp))
    (define filtered-spaces (filter-spaces remaining-spaces))
    filtered-spaces)

;;; GUILLOTINE

(define (generate-available-spaces-guillotine sheet item)
    (define available-spaces (rectangular-sheet-struct-available-spaces sheet))
    (define available-spaces-clean (filter (lambda (available-space) 
                                        (not (space-matches-item? item available-space))) available-spaces))
    (define remaining-spaces-temp (map (lambda (space)
                                            (define spaces-from-intersection (split-intersecting-space-guillotine item space))
                                            (if (empty? spaces-from-intersection)
                                                (list space)
                                                spaces-from-intersection))
                            available-spaces-clean))
    (define remaining-spaces (apply append remaining-spaces-temp))
    (define filtered-spaces (filter-spaces remaining-spaces))
    filtered-spaces)


(define (space-bigger space1 space2)
    (define space1-area (* (space-struct-width space1) (space-struct-height space1)))
    (define space2-area (* (space-struct-width space2) (space-struct-height space2)))
    (cond
        [(> space1-area space2-area) #t]
        [else #f]))

(define (split-intersecting-space-guillotine item space)
    (define intersecting-spaces '())
    (define-values (item-x item-y item-width item-height) 
                    (values (cutting-pattern-struct-x item) (cutting-pattern-struct-y item)
                            (cutting-pattern-struct-width item) (cutting-pattern-struct-height item)))
    (define-values (space-x space-y space-width space-height) 
                    (values (space-struct-x space) (space-struct-y space)
                            (space-struct-width space) (space-struct-height space)))
    (when (intersects? item space)
            (define right-space
            ;right space
            (cond 
                [(< (+ item-x item-width) (+ space-x space-width (margin)))
                    (space-struct (+ item-x item-width (margin)) space-y (- space-width (+ item-x item-width (margin) (- space-x))) space-height)]
                [else
                    #f]))
            (when right-space
            (define-values (right-space-x right-space-y right-space-width right-space-height) 
                                (values (space-struct-x right-space) (space-struct-y right-space) (space-struct-width right-space) (space-struct-height right-space)))
            (println (format "[SPACES] Right space: x=~a y=~a width=~a height=~a" right-space-x right-space-y right-space-width right-space-height)))

            (define top-space
                (cond 
                [(< (+ item-y item-height) (+ space-y space-height (margin)))
                    (space-struct space-x (+ item-y item-height (margin)) space-width (- space-height (+ item-y item-height (margin) (- space-y))))]
                [else
                    #f]))

            (when top-space
            (define-values (top-space-x top-space-y top-space-width top-space-height) 
                                (values (space-struct-x top-space) (space-struct-y top-space)
                                        (space-struct-width top-space) (space-struct-height top-space)))
            (println (format "[SPACES] Top space: x=~a y=~a width=~a height=~a" top-space-x top-space-y top-space-width top-space-height)))


        (cond 
            [(and right-space top-space)
                (println "[SPACES] Both spaces are available")
                (cond
                    [(space-bigger top-space right-space) 
                        (println "[SPACES] Top space is bigger")
                        (set! intersecting-spaces (append intersecting-spaces (list top-space)))
                        (set! intersecting-spaces (append intersecting-spaces (list (space-struct (+ item-x item-width (margin)) space-y (- space-width (+ item-x item-width (- space-x))) item-height))))
                        (println (format "[SPACES] Top space added x=~a y=~a width=~a height=~a" (space-struct-x top-space) (space-struct-y top-space) (space-struct-width top-space) (space-struct-height top-space)))
                        (println (format "[SPACES] Right space reduced to x=~a y=~a width=~a height=~a" (+ item-x item-width (margin)) space-y (- space-width (+ item-x item-width (- space-x))) item-height))
                        ]
                    [else
                        (println "[SPACES] Right space is bigger")
                        (set! intersecting-spaces (append intersecting-spaces (list right-space)))
                        (set! intersecting-spaces (append intersecting-spaces (list (space-struct space-x (+ item-y item-height (margin)) item-width (- space-height (+ item-y item-height (- space-y)))))))
                        (println (format "[SPACES] Right space added x=~a y=~a width=~a height=~a" (space-struct-x right-space) (space-struct-y right-space) (space-struct-width right-space) (space-struct-height right-space)))
                        (println (format "[SPACES] Top space reduced to x=~a y=~a width=~a height=~a" space-x (+ item-y item-height (margin)) item-width (- space-height (+ item-y item-height (- space-y)))))
                        ])]

            [top-space 
                (println "[SPACES] Only top space available")
                (set! intersecting-spaces (append intersecting-spaces (list top-space)))]
            [right-space 
            (println "[SPACES] Only right space available")
            (set! intersecting-spaces (append intersecting-spaces (list right-space)))]))
        intersecting-spaces)