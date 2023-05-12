#lang racket

(require "structs.rkt")
(provide find-placement)


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

(define (get-best-orientation item space)
    (define original-aspect-ratio-diff (abs (- (/ (order-item-struct-width item) (order-item-struct-height item))
                                           (/ (space-struct-width space) (space-struct-height space)))))

    (define rotated-aspect-ratio-diff (abs (- (/ (order-item-struct-height item) (order-item-struct-width item))
                                          (/ (space-struct-width space) (space-struct-height space)))))
    (if (< original-aspect-ratio-diff rotated-aspect-ratio-diff)
        'original
        'rotated))

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
        [(and (fits-original-orientation? item best-space)
              (fits-rotated? item best-space))
            (println "item fits in both orientations")
            (println (format "best orientation is ~a" (get-best-orientation item best-space)))
            (case (get-best-orientation item best-space)
                ['original (println "returning placement for original orientation")(list item sheet best-space)]
                ['rotated (list (order-item-struct 
                                        item-height 
                                        item-width 
                                        (order-item-struct-material-id item)
                                        (order-item-struct-rotate item)) 
                                        sheet best-space)])]
        [(fits-original-orientation? item best-space) (list item sheet best-space)]
        [(fits-rotated? item best-space) (list (order-item-struct 
                                                    item-height 
                                                    item-width 
                                                    (order-item-struct-material-id item)
                                                    (order-item-struct-rotate item)) 
                                                sheet best-space)]))

;; TODO: implement function for best placement by proximity proximity 
(define (get-best-placement-by-proximity item sheet)
    (void))

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
