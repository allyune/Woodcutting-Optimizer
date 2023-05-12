#lang racket

(require "structs.rkt")
(require math)
(provide find-placement)

;; Helper functions for placement by score
(define (fits-original-orientation? item space)
    (and (<= (order-item-struct-width item) (space-struct-width space))
                         (<= (order-item-struct-height item) (space-struct-height space))))

(define (fits-rotated? item space)
    (and (<= (order-item-struct-height item) (space-struct-width space))
                         (<= (order-item-struct-width item) (space-struct-height space))))

(define (get-valid-sheets item sheets)
    (filter (lambda (sheet)
                (and (= (order-item-struct-material-id item) 
                        (rectangular-sheet-struct-material-id sheet))
                        (not (empty? (rectangular-sheet-struct-available-spaces sheet)))))
            sheets))

(define (get-valid-spaces item sheet)
    (define available-spaces (rectangular-sheet-struct-available-spaces sheet))
    (if (order-item-struct-rotate item)
        (filter (lambda (space)
                    (or (fits-original-orientation? item space)
                        (fits-rotated? item space)))
            available-spaces)
        (filter (lambda (space)
                    (fits-original-orientation? item space))
            available-spaces)))

(define (compare-aspect-ratios item space)
    (define original-aspect-ratio-diff (abs (- (/ (order-item-struct-width item) (order-item-struct-height item))
                                            (/ (space-struct-width space) (space-struct-height space)))))

    (define rotated-aspect-ratio-diff (abs (- (/ (order-item-struct-height item) (order-item-struct-width item))
                                            (/ (space-struct-width space) (space-struct-height space)))))
    (if (< original-aspect-ratio-diff rotated-aspect-ratio-diff)
        'original
        'rotated))

(define (get-best-orientation item space)
    (cond
        [(and (fits-original-orientation? item space)
              (fits-rotated? item space))
            (compare-aspect-ratios item space)]
        [(fits-original-orientation? item space) 'original]
        [(fits-rotated? item space) 'rotated]))


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
    (for ([space (get-valid-spaces item sheet)])
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
                    (get-best-orientation item best-space)
                    ['original (list item sheet best-space)]
                    ['rotated (list (order-item-struct 
                                            item-height 
                                            item-width 
                                            (order-item-struct-material-id item)
                                            (order-item-struct-rotate item)) 
                                            sheet best-space)])]))

;; TODO: implement function for best placement by proximity proximity 
(define (get-best-placement-by-proximity item sheet)
    (define-values (item-width item-height material-id rotate) 
                    (values (order-item-struct-width item) (order-item-struct-height item)
                                (order-item-struct-material-id item) (order-item-struct-rotate item)))
        (define valid-spaces (get-valid-spaces item sheet))
        (define sorted-spaces (sort-spaces-proximity valid-spaces))
        (define best-space (first sorted-spaces))
        (cond
            [(not (order-item-struct-rotate item))
               (define-values (space-x space-y space-width space-height) 
                        (values (space-struct-x best-space) (space-struct-y best-space)
                                (space-struct-width best-space) (space-struct-height best-space)))
               (list item sheet best-space)]
            [else
                (case 
                    (get-best-orientation item best-space)
                    ['original (list item sheet best-space)]
                    ['rotated (list (order-item-struct 
                                            item-height 
                                            item-width 
                                            (order-item-struct-material-id item)
                                            (order-item-struct-rotate item)) 
                                            sheet best-space)])]))


(define (find-placement order-item sheets)
    (println (format "Getting placement for width=~a height=~a" (order-item-struct-width order-item) (order-item-struct-height order-item)))
    ; sheets of the same material id with empty spaces
    (define valid-sheets (get-valid-sheets order-item sheets))
    (println (format "Valid sheets ~a" valid-sheets))
    ;; sheets where item can be places
    (define suitable-sheets (filter (lambda (sheet)
                                        (not (empty? (get-valid-spaces order-item sheet))))
                                valid-sheets))
    (println (format "Suitable sheets ~a" suitable-sheets))
    (define best-sheet (if (empty? suitable-sheets)
                            #f
                            (first suitable-sheets)))
    (println (format "Best sheet sheets ~a" best-sheet))
    
    (cond 
        [(not best-sheet) '()]
        [(order-item-struct-rotate order-item) (get-best-placement-by-score order-item best-sheet)]
        [(not (order-item-struct-rotate order-item)) (get-best-placement-by-proximity order-item best-sheet)]))