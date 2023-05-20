#lang racket
(require "../structs.rkt"
         "../utils.rkt")
(provide generate-available-spaces-guillotine)

(define (get-right-space-guillotine item space)
    (define-values (item-x item-y item-width item-height) 
                    (values (cutting-pattern-struct-x item) (cutting-pattern-struct-y item)
                            (cutting-pattern-struct-width item) (cutting-pattern-struct-height item)))
    (define-values (space-x space-y space-width space-height) 
                    (values (space-struct-x space) (space-struct-y space)
                            (space-struct-width space) (space-struct-height space)))
    (cond 
        [(< (+ item-x item-width) (- (+ space-x space-width) (margin)))
            (space-struct (+ item-x item-width (margin)) space-y (- space-width (+ item-width (margin))) space-height)]
        [else
            #f]))

(define (get-top-space-guillotine item space)
    (define-values (item-x item-y item-width item-height) 
                    (values (cutting-pattern-struct-x item) (cutting-pattern-struct-y item)
                            (cutting-pattern-struct-width item) (cutting-pattern-struct-height item)))
    (define-values (space-x space-y space-width space-height) 
                    (values (space-struct-x space) (space-struct-y space)
                            (space-struct-width space) (space-struct-height space)))
    (cond 
        [(< (+ item-y item-height) (- (+ space-y space-height) (margin)))
            (space-struct space-x (+ item-y item-height (margin)) space-width (- space-height (+ item-height (margin))))]
        [else
            #f]))

(define (split-space-guillotine item space)
    (define intersecting-spaces '())
    (define-values (item-x item-y item-width item-height) 
                    (values (cutting-pattern-struct-x item) (cutting-pattern-struct-y item)
                            (cutting-pattern-struct-width item) (cutting-pattern-struct-height item)))
    (define-values (space-x space-y space-width space-height) 
                    (values (space-struct-x space) (space-struct-y space)
                            (space-struct-width space) (space-struct-height space)))
    (define right-space (get-right-space-guillotine item space))
    (define top-space (get-top-space-guillotine item space))

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
                    (set! intersecting-spaces (append intersecting-spaces (list (space-struct space-x (+ item-y item-height (margin)) item-width (- space-height (+ item-height (margin)))))))
                    (println (format "[SPACES] Right space added x=~a y=~a width=~a height=~a" (space-struct-x right-space) (space-struct-y right-space) (space-struct-width right-space) (space-struct-height right-space)))
                    (println (format "[SPACES] Top space reduced to x=~a y=~a width=~a height=~a" space-x (+ item-y item-height (margin)) item-width (- space-height (+ item-height (margin)))))
                    ])]

        [top-space 
            (println "[SPACES] Only top space available")
            (set! intersecting-spaces (append intersecting-spaces (list top-space)))]
        [right-space 
        (println "[SPACES] Only right space available")
        (set! intersecting-spaces (append intersecting-spaces (list right-space)))])
        intersecting-spaces)


    (define (generate-available-spaces-guillotine sheet item best-space)
        (define available-spaces (rectangular-sheet-struct-available-spaces sheet))
        ; (define available-spaces-clean (filter (lambda (available-space) 
        ;                                     (not (space-matches-item? item available-space))) available-spaces))
        (define spaces-from-splitting (split-space-guillotine item best-space))
        (println spaces-from-splitting)
        (define spaces-not-empty (filter (lambda (list) (not (empty? list))) spaces-from-splitting))
        (println spaces-not-empty)
        (define remaining-spaces (filter (lambda (space) (not (spaces-equal? space best-space))) available-spaces))
        (define available-spaces-new (append remaining-spaces spaces-not-empty))
        (println remaining-spaces)
        (println available-spaces-new)
        available-spaces-new)