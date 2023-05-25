#lang racket
(require "structs.rkt"
         "utils.rkt")
(provide generate-available-spaces)

(define (split-intersecting-space item space)
    (define intersecting-spaces '())
    (define-values (item-x item-y item-width item-height) 
                    (values (cutting-pattern-struct-x item) (cutting-pattern-struct-y item)
                            (cutting-pattern-struct-width item) (cutting-pattern-struct-height item)))
    (define-values (space-x space-y space-width space-height) 
                    (values (space-struct-x space) (space-struct-y space)
                            (space-struct-width space) (space-struct-height space)))
    ;left space
    (when (> item-x (+ space-x (margin)))
        (println (format "[SPACES] Left space added x=~a y=~a width=~a height=~a" space-x space-y (- item-x space-x (margin)) space-height))
        (set! intersecting-spaces (append intersecting-spaces 
            (list (space-struct space-x space-y (- item-x space-x (margin)) space-height)))))
    ;right space
    (when (< (+ item-x item-width) (- (+ space-x space-width) (margin)))
        (println (format "[SPACES] Right space added x=~a y=~a width=~a height=~a" (+ item-x item-width (margin)) space-y (- space-width item-width (margin)) space-height))
        (set! intersecting-spaces (append intersecting-spaces 
            (list (space-struct (+ item-x item-width (margin)) space-y (- space-width (- item-x space-x) item-width (margin)) space-height)))))
        
    ;bottom space
    (when (> item-y (+ space-y (margin)))
            (println (format "[SPACES] Bottom space added x=~a y=~a width=~a height=~a"  space-x space-y space-width (- item-y space-y (margin))))
        (set! intersecting-spaces (append intersecting-spaces 
            (list (space-struct space-x space-y space-width (- item-y space-y (margin)))))))
    ;top space
    (when (< (+ item-y item-height) (- (+ space-y space-height) (margin)))
        (println (format "[SPACES] Top space added x=~a y=~a width=~a height=~a"  space-x (+ item-y item-height (margin)) space-width (- space-height item-height (margin))))
        (set! intersecting-spaces (append intersecting-spaces
            (list (space-struct space-x (+ item-y item-height (margin)) space-width (- space-height (- item-y space-y) item-height (margin)))))))
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


(define (generate-available-spaces sheet item best-space)
    (define available-spaces (rectangular-sheet-struct-available-spaces sheet))
    ;if item exactly matches the space - no need to split, just remove
    (define available-spaces-clean (filter (lambda (available-space) 
                                        (not (space-matches-item? item available-space))) available-spaces))
    (define intersecting-spaces (filter (lambda (space) (intersects? item space)) available-spaces-clean))
    (define nonintersecting-spaces (filter (lambda (space) (not (intersects? item space))) available-spaces-clean))
    (define spaces-from-splitting (map (lambda (space) (split-intersecting-space item space)) intersecting-spaces))
    (define spaces-not-empty (filter (lambda (list) (not (empty? list))) spaces-from-splitting))
    (define remaining-spaces (apply append spaces-not-empty))
    (println remaining-spaces)
    (define remaining-spaces-all (apply append remaining-spaces (list nonintersecting-spaces)))    
    (println remaining-spaces-all)
    (define filtered-spaces (filter-spaces remaining-spaces-all))
    (println filtered-spaces)
    filtered-spaces)


; (define (generate-available-spaces-guillotine sheet item best-space)
;         (define available-spaces (rectangular-sheet-struct-available-spaces sheet))
;         ; (define available-spaces-clean (filter (lambda (available-space) 
;         ;                                     (not (space-matches-item? item available-space))) available-spaces))
;         (define spaces-from-splitting (split-intersecting-space item best-space))
;         (define spaces-not-empty (filter (lambda (list) (not (empty? list))) spaces-from-splitting))
;         (define remaining-spaces (filter (lambda (space) (not (spaces-equal? space best-space))) available-spaces))
;         (define available-spaces-new (append remaining-spaces spaces-not-empty))
;         available-spaces-new)


; (define (merge-adjacent-spaces spaces)
;     (define (compare-by-x-coord space1 space2)
;         (define x1 (space-struct-x space1))
;         (define x2 (space-struct-x space2))
;         (cond
;             [(< x1 x2) -1]
;             [(> x1 x2) 1]
;             [else 0]))

;     (define sorted-spaces (sort spaces compare-by-x-coord))
;     (define merged-spaces '())
;     (for ([space sorted-spaces])
;         (println "processing space")
;         (define last-space (if (not (empty? merged-spaces)) (last merged-spaces) #f))
;         (cond 
;             ;; space is above last space
;             [(and (not (empty? merged-spaces)) 
;                   (= (+ (space-struct-y last-space) (space-struct-height last-space))  (space-struct-y space))
;                   (or (= (space-struct-x last-space) (space-struct-x space))
;                       (= (+ (space-struct-x last-space) (space-struct-width last-space))
;                          (+ (space-struct-x space) (space-struct-width space)))))
                
;                 (define new-space (space-struct (max (space-struct-x last-space) (space-struct-x space))
;                                                                 (min (space-struct-y last-space) (space-struct-y space))
;                                                                 (min (space-struct-width last-space) (space-struct-width space))
;                                                                 (+ (space-struct-height last-space) (space-struct-height space))))
;                 (set! merged-spaces (drop-right merged-spaces 1))
;                 (set! merged-spaces (append merged-spaces (list new-space)))
;                 ;splitting bigger space
;                 (cond 
;                     [(space-bigger space last-space)
;                         (define item (cutting-pattern-struct 
;                                         (space-struct-x new-space)
;                                         (space-struct-y space)
;                                         (space-struct-width new-space)
;                                         (- (space-struct-height new-space) (space-struct-height last-space))))
;                         (define top-space (get-top-space-guillotine item space))
;                         (define right-space (get-right-space-guillotine item space))
;                         (set! merged-spaces (append merged-spaces (list top-space)))
;                         (set! merged-spaces (append merged-spaces (list right-space)))]
;                     [(space-bigger last-space space)
;                         (define item (cutting-pattern-struct 
;                                         (space-struct-x new-space)
;                                         (space-struct-y last-space)
;                                         (space-struct-width new-space)
;                                         (- (space-struct-height new-space) (space-struct-height space))))
;                         (define top-space (get-top-space-guillotine item last-space))
;                         (define right-space (get-right-space-guillotine item last-space))
;                         (set! merged-spaces (append merged-spaces (list top-space)))
;                         (set! merged-spaces (append merged-spaces (list right-space)))
;                         ])]

;             ;; space is below last space
;             [(and (not (empty? merged-spaces)) 
;                   (= (+ (space-struct-y space) (space-struct-height space))  (space-struct-y last-space))
;                   (or (= (space-struct-x last-space) (space-struct-x space))
;                       (= (+ (space-struct-x last-space) (space-struct-width last-space))
;                          (+ (space-struct-x space) (space-struct-width space)))))
                
;                 (define new-space (space-struct (max (space-struct-x last-space) (space-struct-x space))
;                                                                 (min (space-struct-y last-space) (space-struct-y space))
;                                                                 (min (space-struct-width last-space) (space-struct-width space))
;                                                                 (+ (space-struct-height last-space) (space-struct-height space))))
;                 (set! merged-spaces (drop-right merged-spaces 1))
;                 (set! merged-spaces (append merged-spaces (list new-space)))
;                 ;splitting bigger space
;                 (cond 
;                     [(space-bigger space last-space)
;                         (define item (cutting-pattern-struct 
;                                         (space-struct-x new-space)
;                                         (space-struct-y space)
;                                         (space-struct-width new-space)
;                                         (- (space-struct-height new-space) (space-struct-height last-space))))
;                         (define top-space (get-top-space-guillotine item space))
;                         (define right-space (get-right-space-guillotine item space))
;                         (set! merged-spaces (append merged-spaces (list top-space)))
;                         (set! merged-spaces (append merged-spaces (list right-space)))]
;                     [(space-bigger last-space space)
;                         (define item (cutting-pattern-struct 
;                                         (space-struct-x new-space)
;                                         (space-struct-y last-space)
;                                         (space-struct-width new-space)
;                                         (- (space-struct-height new-space) (space-struct-height space))))
;                         (define top-space (get-top-space-guillotine item last-space))
;                         (define right-space (get-right-space-guillotine item last-space))
;                         (set! merged-spaces (append merged-spaces (list top-space)))
;                         (set! merged-spaces (append merged-spaces (list right-space)))
;                         ])]

;             ; [(and (not (empty? merged-spaces)) 
;             ;         (= (space-struct-y (last merged-spaces)) (space-struct-y space))
;             ;         (= (+ (space-struct-x (last merged-spaces)) (space-struct-width (last merged-spaces)))  (space-struct-x space))
;             ;         (= (space-struct-height (last merged-spaces)) (space-struct-height space)))
                    
;             ;     (println "merging horizontally")
;             ;     (set! merged-spaces (drop-right merged-spaces 1))
;             ;     (set! merged-spaces (append merged-spaces
;             ;                                 (list (space-struct (space-struct-x (last merged-spaces))
;             ;                                                     (space-struct-y (last merged-spaces))
;             ;                                                     (+ (space-struct-width (last merged-spaces)) (space-struct-width space))
;             ;                                                     (space-struct-height (last merged-spaces))
;             ;                                                     (+ (space-struct-height (last merged-spaces)) (space-struct-height space))))))]
            
;             [else
;                 (println "appending as is") 
;                 (set! merged-spaces (append merged-spaces (list space)))])
;         (set! merged-spaces (filter (lambda (x) x) merged-spaces)))
;     (println merged-spaces)
;     merged-spaces)