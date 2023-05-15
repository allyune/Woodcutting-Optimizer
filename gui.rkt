#lang racket/gui

(require csv-reading
         racket/snip	
         "main.rkt"
         "structs.rkt")
;maybe store it as a string? Depends on future usage.
(define input-file #f)
(define data #f)
(define results #f)
(define rectangles '())

;; Styles
(define background-background-no-file (make-object color% 234 182 118))
(define background-background-with-file (make-object color% 36 155 98))
(define text-color "white")
(define grid-width 4)
(define rectangle-size 100)
(define gap 20)

;;Functions and classes
(define (start-func)
    (with-handlers 
        ([exn?
            (lambda (e) (println "wrong data format")        
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
            )])
        (define cutting-result (process-file data))
        (define cutting-patterns (hash-ref cutting-result 'cutting-patterns))
        (set! results cutting-patterns)
        (send dropdown-panel set-canvas-background background-background-with-file)
        (send dropdown-panel refresh)
        (send results-board refresh)
        (define output-path (put-file "Save file to..." #f #f (make-csv-file-name) "csv"))
        (when output-path
            (call-with-output-file output-path
                (lambda (output-port)
                    (write-string (cutting-result->output cutting-result) output-port))))
        (define num-sheets (hash-ref cutting-result 'number-of-sheets))
        (define waste-percentage (hash-ref cutting-result 'waste-percentage))
        (send material-waste set-label (format "Waste percentage: ~a" waste-percentage))
        (send total-sheets set-label (format "Total number of cutting sheets: ~a" num-sheets))))

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
    ;   (send this set-canvas-background background-background-with-file)
      (start-func)
      (super on-drop-file file))

     (define/override (on-event event)
      (define event-type (send event get-event-type))
      (when (eq? event-type 'left-down)
        (define file (get-file "Select CSV file" frame #f #f "csv" null '(("csv" "*.csv"))))
        (when file
            (let ([dc (send this get-dc)])
                ; (send this set-canvas-background background-background-with-file)
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
      (when (eq? event-type 'left-down)
        (sheet-click-handler event))
      (super on-event event))
    (super-new)))

(define (make-popup element)
    (define popup-frame (new frame%	 
                    [label (number->string element)]	 
                    [parent frame]	 
                    [width 400]	 
                    [height 500]
                    [stretchable-width #f]
                    [stretchable-height #f]))
                (define popup (new canvas% [parent popup-frame]
                            [min-width 400]
                            [min-height 500]
                            [paint-callback
                                (lambda (canvas dc)
                                    (send dc set-font (make-font #:size 20))
                                    (send dc draw-text "Hello, World!" 50 50))]))
                (send popup-frame show #t)
                (void))


(define (draw-rectangles dc)
    (define (draw-rectangle-with-number canvas-x canvas-y number)
        (define x (+ canvas-x gap))
        (define y (+ canvas-y gap))
        
        (define brush (new brush% [color "white"]))
        (define pen (new pen% [color "black"] [width 0.5] [style 'long-dash]))
        
        (send dc set-brush brush)
        (send dc set-pen pen)
        (send dc draw-rectangle x y rectangle-size rectangle-size)
        
        (send dc set-text-foreground "black")
        (send dc draw-text (number->string number)
                        (+ x (/ rectangle-size 2))
                        (+ y (/ rectangle-size 2))
                        'center))
    
    (define-values (canvas-width canvas-height) (send dc get-size))
    
    (let loop ((elements (range 10))
                (row 0)
                (col 0))
        (cond
        ((null? elements)
        (void))
        
        ((>= col grid-width) ; Reached the end of the row, move to the next row
        (loop elements (+ row 1) 0))
        
        (else 
        (let* ([rectangle-x (+ (* col (+ rectangle-size gap)) gap)] 
                [rectangle-y (+ (* row (+ rectangle-size gap)) gap)])
            (draw-rectangle-with-number rectangle-x rectangle-y (car elements))
            (loop (cdr elements) row (+ col 1)))))))

(define (sheet-click-handler event)
    (define elements '(1 2 3 4 5 6 7 8 9 10))
    (define clicked-x (send event get-x))
    (define clicked-y (send event get-y))
    (define relative-x (- clicked-x 20))
    (define relative-y (- clicked-y 20))
    (when (and
            (>= clicked-x 40)
            (>= clicked-y 40)
            (>= (modulo relative-x 120) 20)
            (>= (modulo relative-y 120) 20))
        (let* ([col (quotient relative-x 120)]
              [row (quotient relative-y 120)]
              [index (+ (* row grid-width) col)])
            (when (< index (length elements))
                (println index)
                (make-popup (list-ref elements index))
            ))))

;;Layout
(define frame (new frame% [label "Welcome to STAKO cutter"] [width 1000] [height 600]))

(define columns (new horizontal-panel% [parent frame]))

(define left (new vertical-panel% [parent columns] [min-width 500]))
(define right (new vertical-panel% [parent columns] [min-width 500]))

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
                                       [vert-margin 10] [horiz-margin 10] [style '(vscroll)]
                                       [stretchable-width #t] [stretchable-height #t]
                                       [min-height 400] [min-width 400]
                                       [paint-callback
                                        (lambda (canvas dc)
                                            (if (not results)
                                            (begin
                                            (send dc set-text-foreground "white")
                                            (send dc draw-text "Please select a file" 0 0))
                                            (draw-rectangles dc)))
                                            ]))

(send results-board set-canvas-background background-background-no-file)

(send dropdown-panel accept-drop-files #t)
(send frame show #t)