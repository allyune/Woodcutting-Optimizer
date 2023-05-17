#lang racket/gui

(require csv-reading
         racket/snip	
         "main.rkt"
         "structs.rkt"
         "splitting.rkt")

;global variables
(define input-file #f)
(define data #f)
(define results #f)
(define rectangles '())
(define sheets '())

;; Styles
(define background-background-no-file (make-object color% 234 182 118))
(define background-background-with-file (make-object color% 36 155 98))
(define results-background (make-object color% 235 237 237))
(define results-text-color (make-object color% 80 80 80))
(define text-color "white")
(define grid-width 4)
(define rectangle-width 181)
(define rectangle-height 181)
(define gap 20)

;;Functions and classes

(define (start-func)
    (parameterize ([margin 10])
        (with-handlers 
            ([exn:fail?
                (lambda (e) 
                (println (format "Error messagw: ~a" (exn-message e)))
                    (cond
                        [(regexp-match? #px"^open-output-file: file exists.*" (exn-message e))]
                        [else (println "wrong data format")    
                            (define error (new dialog%	 
                                    [label "Input file error"]
                                    [parent frame]
                                    [width 150]
                                    [height 100]
                                    [style '(close-button)]))
                            (new message% [parent error] [label "Wrong data format, check the input file and try again"])
                            (new button% [parent error] [label "OK"] [callback (lambda (button event) (send error show #f))])
                            (send error show #t)
                            (set! input-file #f)
                            (send dropdown-panel set-canvas-background background-background-no-file)
                            (send frame refresh)
                            ]))])
            (define cutting-result (process-file data))
            (define cutting-patterns (hash-ref cutting-result 'cutting-patterns))
            (define cutting-sheets (hash-ref cutting-result 'sheets))
            (set! results cutting-result)
            (set! sheets cutting-sheets)
            (send dropdown-panel set-canvas-background background-background-with-file)
            (send dropdown-panel refresh)
            (when results
            (define output-path (put-file "Save file to..." #f #f (make-csv-file-name) "csv"))
            (when output-path
                (when (file-exists? output-path)
                    (delete-file output-path))
                (call-with-output-file output-path
                    (lambda (output-port)
                    (write-string (cutting-result->output results) output-port))))
                (define num-sheets (hash-ref results 'number-of-sheets))
                (define waste-percentage (exact->inexact (hash-ref results 'waste-percentage)))
                (send material-waste set-label (format "Waste percentage: ~a" waste-percentage))
                (send total-sheets set-label (format "Total number of cutting sheets: ~a" num-sheets))
                (send results-board refresh)))))

(define my-canvas%
  (class canvas%
    (define/override (on-paint)
      (let ([dc (send this get-dc)])
        (define background-color (if input-file background-background-with-file background-background-no-file))
        (send this set-canvas-background background-color)
        (define-values (canvas-width canvas-height) (send this get-size))
        (send dc set-font (make-font #:size 14 #:family 'swiss #:weight 'light))
        (send dc set-text-foreground text-color)
        (define text (if input-file 
                        (match (regexp-match #rx".*/(.*)$" (path->string input-file)) [(list _ match) match]) 
                        "Click here to select a file or drop the file to this area"))
        (define-values (text-width text-height a b) (send dc get-text-extent text))
        (define text-x (quotient (- canvas-width text-width) 2))
        (define text-y (quotient (- canvas-height text-height) 2))
        (send dc draw-text text text-x text-y)))

    (define/override (on-drop-file file)
      (set! results #f)
      (send results-board refresh)
      (set! input-file file)
      (set! data (rest (csv->list (open-input-file input-file))))
      (start-func)
      (super on-drop-file file))

     (define/override (on-event event)
      (define event-type (send event get-event-type))
      (when (eq? event-type 'left-down)
        (define file (get-file "Select CSV file" frame #f #f "csv" null '(("csv" "*.csv"))))
        (when file
            (let ([dc (send this get-dc)])
                (set! results #f)
                (send results-board refresh)
                (set! input-file file)
                (set! data (rest (csv->list (open-input-file input-file)))) 
                (start-func))))
      (super on-event event))
    (super-new
     [min-height 100]
     [stretchable-width #t]
     [stretchable-height #f])))


(define my-result-board%
  (class canvas%
     (define/override (on-event event)
      (define event-type (send event get-event-type))
      (when (and (eq? event-type 'left-down) results)
        (sheet-click-handler event))
      (super on-event event))
    (super-new)))

(define (make-popup patterns)
    (define popup-frame (new frame%	 
                    [label "Cutting pattern"]	 
                    [parent frame]	 
                    [width 400]	 
                    [height 500]
                    [stretchable-width #f]
                    [stretchable-height #f]))
                (define popup (new canvas% [parent popup-frame]
                            [min-width 700]
                            [min-height 518]
                            [paint-callback
                                (lambda (canvas dc)
                                    (for ([item patterns])
                                        (define-values (x y width height) (values (cutting-pattern-struct-x item) (cutting-pattern-struct-y item)
                                                                                  (cutting-pattern-struct-width item) (cutting-pattern-struct-height item)))
                                        (send dc draw-rectangle (/ x 4) (/ y 4) (/ width 4) (/ height 4))))]))
                (send popup set-canvas-background results-background)
                (send popup-frame show #t)
                (void))


(define (draw-rectangles dc)
    (define (draw-rectangle-with-info canvas-x canvas-y element index)
        (define x (+ canvas-x gap))
        (define y (+ canvas-y gap))
        (define brush (new brush% [color "white"]))
        (send dc set-brush brush)
        (send dc set-pen (new pen% [color "black"] [width 0.5] [style 'long-dash]))
        (define patterns (list-ref (hash-ref results 'cutting-patterns) index))
        (define sheet-num (+ index 1))
        (define material-id (rectangular-sheet-struct-material-id (list-ref sheets index)))
        (send dc draw-rectangle x y rectangle-width rectangle-height)
        (send dc set-pen (new pen% [color "black"] [width 0.5] [style 'solid]))
        (send dc draw-rectangle (+ x 3) (+ y 3) 175 130)
        (for ([item patterns])
            (define-values (item-x item-y item-width item-height) (values (cutting-pattern-struct-x item) (cutting-pattern-struct-y item)
                                                        (cutting-pattern-struct-width item) (cutting-pattern-struct-height item)))
            (send dc draw-rectangle (+ (/ item-x 16) x 3) (+ (/ item-y 16) y 3) (/ item-width 16) (/ item-height 16)))
        (send dc set-text-foreground "black")
        (send dc draw-text (format "Sheet ~a" sheet-num)
                        (+ x (/ rectangle-width 3))
                        (+ y 140)
                        'center)
        (send dc draw-text (format "~a" material-id)
                        (+ x (/ rectangle-width 3))
                        (+ y 155)
                        'center))
    
    (let loop ([patterns (hash-ref results 'cutting-patterns)]
                [row 0]
                [col 0]
                [index 0])
        (cond
        [(null? patterns) (void)]
        [(>= col grid-width) (loop patterns (+ row 1) 0 index)]
        [else 
            (let* ([rectangle-x (+ (* col (+ rectangle-width gap)) gap)] 
                    [rectangle-y (+ (* row (+ rectangle-height gap)) gap)])
                (draw-rectangle-with-info rectangle-x rectangle-y (car patterns) index)
                (loop (cdr patterns) row (+ col 1) (+ index 1)))]))
            (define-values (canvas-width canvas-height) (send dc get-size))
            (println canvas-height))

(define (sheet-click-handler event)
    (define clicked-x (send event get-x))
    (define clicked-y (send event get-y))
    (define relative-x (- clicked-x 20))
    (define relative-y (- clicked-y 20))
    (when (and
            (>= clicked-x 40)
            (>= clicked-y 40)
            (>= (modulo relative-x 201) 20)
            (>= (modulo relative-y 201) 20))
        (let* ([col (quotient relative-x 201)]
              [row (quotient relative-y 201)]
              [index (+ (* row grid-width) col)])
            (when (< index (length (hash-ref results 'cutting-patterns)))
                (println index)
                (make-popup (list-ref (hash-ref results 'cutting-patterns) index))
            ))))

;;Layout
(define frame (new frame% [label "Welcome to STAKO cutter"] [width 1000] [height 600]))

(define columns (new horizontal-panel% [parent frame]))

(define left (new vertical-panel% [parent columns] [min-width 500]))
(define right (new vertical-panel% [parent columns] [style (list 'vscroll 'hscroll)] [min-width 900] [stretchable-width #t] [stretchable-height #t]))

(define dropdown-panel (new my-canvas% [parent left] [style '(border)]
                                       [vert-margin 10] [horiz-margin 30]))

(define results-box (new group-box-panel%	 
   	 	[label "Results"]	 
   	 	[parent left]	 
   	 	[vert-margin 10]	 
   	 	[horiz-margin 10]	 
   	 	[border 2]	 
   	 	[alignment '(left top)]	 
   	 	[min-height 150]	 
   	 	[stretchable-width #t]	 
   	 	[stretchable-height #f]))

(define material-waste (new message% [parent results-box] [label "Material waste: "] [stretchable-width #t]))
(define total-sheets (new message% [parent results-box] [label "Total number of cutting sheets: "] [stretchable-width #t]))


(define results-board (new my-result-board% [parent right]
                                       [vert-margin 10] [horiz-margin 10]
                                       [stretchable-width #t] [stretchable-height #t]
                                       [min-height 400] [min-width 400]
                                       [paint-callback
                                        (lambda (canvas dc)
                                            (define text "Please select a file")
                                            (define-values (canvas-width canvas-height) (send results-board get-size))
                                            (define-values (text-width text-height a b) (send dc get-text-extent text))
                                            (define text-x (quotient (- canvas-width text-width) 2))
                                            (define text-y (quotient (- canvas-height text-height) 2))
                                            (cond
                                             [(not results) 
                                                (send dc set-text-foreground results-text-color)
                                                (send dc set-font (make-font #:size 24 #:family 'swiss #:weight 'bold))
                                                (send dc draw-text text text-x text-y)]
                                            [else
                                                (send dc set-font (make-font #:size 14 #:family 'swiss #:weight 'thin))
                                                (draw-rectangles dc)]))]))
                              
                                            
(send results-board set-canvas-background results-background)

(send dropdown-panel accept-drop-files #t)
(send frame show #t)