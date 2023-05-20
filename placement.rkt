#lang racket

(require "structs.rkt"
         "splitting.rkt"
         "utils.rkt"
         "guillotine/placement.rkt"
         math)

(provide find-placement)

;; Helper functions for placement by score
(define (fits-original-orientation? item space sheet)
    (and  
         (<= (order-item-struct-width item) (space-struct-width space))
         (<= (order-item-struct-height item) (space-struct-height space))))

(define (fits-rotated? item space sheet)
    (and  
          (<= (order-item-struct-height item) (space-struct-width space))
          (<= (order-item-struct-width item) (space-struct-height space))))

(define (get-valid-sheets item sheets)
    (filter (lambda (sheet)
                (and (= (order-item-struct-material-id item) 
                        (rectangular-sheet-struct-material-id sheet))
                        (not (empty? (rectangular-sheet-struct-available-spaces sheet)))))
            sheets))

(define (get-valid-spaces item sheet)
    (println "getting valid spaces")
    (define available-spaces (rectangular-sheet-struct-available-spaces sheet))
    (if (order-item-struct-rotate item)
        (filter (lambda (space)
                    (or (fits-original-orientation? item space sheet)
                        (fits-rotated? item space sheet)))
            available-spaces)
        (filter (lambda (space)
                    (fits-original-orientation? item space sheet))
            available-spaces)))

(define (compare-aspect-ratios item space)
    (define original-aspect-ratio-diff (abs (- (/ (order-item-struct-width item) (order-item-struct-height item))
                                            (/ (space-struct-width space) (space-struct-height space)))))

    (define rotated-aspect-ratio-diff (abs (- (/ (order-item-struct-height item) (order-item-struct-width item))
                                            (/ (space-struct-width space) (space-struct-height space)))))
    (if (< original-aspect-ratio-diff rotated-aspect-ratio-diff)
        'original
        'rotated))

(define (get-best-orientation item space sheet)
    (cond
        ;===best orientation heuristics
        ; [(and (fits-original-orientation? item space)
        ;       (fits-rotated? item space))
        ;     (compare-aspect-ratios item space)]
        ;; ======
        [(fits-original-orientation? item space sheet) 'original]
        [(fits-rotated? item space sheet) 'rotated]))


;; Helper functions for placing by dispance
(define (sort-spaces-proximity spaces)

  (define (distance-squared space)
    (let ([x (space-struct-x space)]
          [y (space-struct-y space)])
      (+ (* x x) (* y y))))
  
  (define (compare-distances space1 space2)
    (let ([dist1 (sqrt (distance-squared space1))] 
          [dist2 (sqrt (distance-squared space2))])
      (cond [(< dist1 dist2) -1]
            [(> dist1 dist2) 1]
            [else 0])))

   (sort spaces compare-distances))

(define (get-best-placement-by-score item sheet)
    (define best-score 0)
    (define best-space #f)
    (define-values (item-width item-height material-id rotate) 
                   (values (order-item-struct-width item) (order-item-struct-height item)
                            (order-item-struct-material-id item) (order-item-struct-rotate item)))
    (define valid-spaces 
        (if (guillotine-cuts) 
            (get-valid-spaces-guillotine item sheet)
            (get-valid-spaces item sheet)))
    (for ([space valid-spaces])
        (define-values (space-x space-y space-width space-height) 
                       (values (space-struct-x space) (space-struct-y space)
                               (space-struct-width space) (space-struct-height space)))
        
        (define score (/ (* item-width item-height)
                                 (* space-width space-height)))
                (if (not best-score)
                    (begin
                        (set! best-score score)
                        (set! best-space space))
                    (when (> score best-score)
                        (set! best-score score)
                        (set! best-space space))))
    (cond
            [(not (order-item-struct-rotate item))
               (define-values (space-x space-y space-width space-height) 
                        (values (space-struct-x best-space) (space-struct-y best-space)
                                (space-struct-width best-space) (space-struct-height best-space)))
               (list item sheet best-space)]
            [else
                (case 
                    (if (guillotine-cuts)
                        (get-best-orientation-guillotine item best-space sheet)
                        (get-best-orientation item best-space sheet))
                    ['original (list item sheet best-space)]
                    ['rotated (list (order-item-struct 
                                            item-height 
                                            item-width 
                                            (order-item-struct-material-id item)
                                            (order-item-struct-rotate item)) 
                                            sheet best-space)])]))

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

;; TODO: implement function for best placement by proximity proximity 
(define (get-best-placement-by-proximity item sheet)
    (define-values (item-width item-height material-id rotate) 
                    (values (order-item-struct-width item) (order-item-struct-height item)
                                (order-item-struct-material-id item) (order-item-struct-rotate item)))
        (define valid-spaces (get-valid-spaces item sheet))
        (define sorted-spaces (sort-spaces-proximity valid-spaces))
        (define matching-spaces (filter (lambda (space)
                                            (space-equals-order-item? item space)) sorted-spaces))
        (define best-space 
            (if (not (empty? matching-spaces))
                (first matching-spaces)
                (first sorted-spaces)))
        (cond
            [(not (order-item-struct-rotate item))
               (define-values (space-x space-y space-width space-height) 
                        (values (space-struct-x best-space) (space-struct-y best-space)
                                (space-struct-width best-space) (space-struct-height best-space)))
               (list item sheet best-space)]
            [else
                (case 
                    (if (guillotine-cuts)
                        (get-best-orientation-guillotine item best-space sheet)
                        (get-best-orientation item best-space sheet))
                    ['original (list item sheet best-space)]
                    ['rotated (list (order-item-struct 
                                            item-height 
                                            item-width 
                                            (order-item-struct-material-id item)
                                            (order-item-struct-rotate item)) 
                                            sheet best-space)])]))

(define (find-placement order-item sheets)
    ; sheets of the same material id with empty spaces
    (define valid-sheets (get-valid-sheets order-item sheets))
    ;; sheets where item can be places
    (define suitable-sheets (filter (lambda (sheet)
                                        (not (empty? (if (guillotine-cuts) 
                                                            (get-valid-spaces-guillotine order-item sheet)
                                                            (get-valid-spaces order-item sheet)))))
                                valid-sheets))
    (define best-sheet (if (empty? suitable-sheets)
                            #f
                            (first suitable-sheets)))
    (cond 
        [(not best-sheet) '()]
        [(order-item-struct-rotate order-item) (get-best-placement-by-score order-item best-sheet)]
        [(not (order-item-struct-rotate order-item)) (get-best-placement-by-score order-item best-sheet)]))
