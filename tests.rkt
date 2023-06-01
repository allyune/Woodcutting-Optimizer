#lang racket

(require rackunit
        "input.rkt"
        "structs.rkt"
        "utils.rkt"
        "guillotine/placement.rkt")

;; Input validation
(define (test-valid-entry?)
  (check-true (valid-entry? "42"))
  (check-true (valid-entry? "1023"))
  (check-false (valid-entry? "0"))
  (check-false (valid-entry? "3.14"))
  (check-false (valid-entry? "abc"))
  (check-false (valid-entry? ""))
  (check-false (valid-entry? " "))
  (check-false (valid-entry? "42abc"))
  (check-false (valid-entry? "42  ")))

(define (test-valid-material?)
  (check-true (valid-material? "42"))
  (check-true (valid-material? "abc"))
  (check-false (valid-material? "0"))
  (check-false (valid-material? "3.14"))
  (check-false (valid-material? ""))
  (check-false (valid-material? " ")))

(define test-data
  '(("6535" "22" "24" "746" "609" "18" "53" "145" "289" "142" "18" "1956" "1007" "1974" "0" "NN" "6521" "1")
    ("6535" "22" "25" "76" "450" "18" "292" "526" "409" "523" "1025" "348" "1515" "366" "0" "NN" "8773" "3")
    ("6535" "22" "26" "1179" "1600" "18" "412" "526" "530" "523" "1533" "348" "2023" "366" "0" "AA" "4832" "2")
    ("6535" "52" "27" "2612" "130" "18" "533" "526" "650" "523" "2041" "348" "2531" "366" "0" "AA" "9573" "0")
    ("6535" "52" "28" "2079" "208" "18" "653" "408" "770" "405" "2549" "848" "3031" "866" "0" "NN" "0" "3")))
    

(define (test-clean-data)
  (check-equal? (clean-data test-data)
   '(("6535" "22" "24" "746" "609" "18" "53" "145" "289" "142" "18" "1956" "1007" "1974" "0" "NN" "6521" "1")
    ("6535" "22" "25" "76" "450" "18" "292" "526" "409" "523" "1025" "348" "1515" "366" "0" "NN" "8773" "3")
    ("6535" "22" "26" "1179" "1600" "18" "412" "526" "530" "523" "1533" "348" "2023" "366" "0" "AA" "4832" "2"))))


(define (test-valid-row?)
  (check-true (valid-row? '("6535" "22" "24" "746" "609" "18" "53" "145" "289" "142" "18" "1956" "1007" "1974" "0" "NN" "6521" "1")))
  (check-true (valid-row? '("6535" "22" "26" "1179" "1600" "18" "412" "526" "530" "523" "1533" "348" "2023" "366" "0" "AA" "4832" "2")))
  (check-false (valid-row? '("6535" "22" "25" "76" "450" "18" "292" "526" "409" "523" "1025" "348" "1515" "366" "0" "BB" "8773" "3")))
  (check-false (valid-row? '("6535" "52" "28" "0" "208" "18" "653" "408" "770" "405" "2549" "848" "3031" "866" "0" "AA" "7521" "3")))
  (check-false (valid-row? '("6535" "52" "28" "0" "0" "18" "653" "408" "770" "405" "2549" "848" "3031" "866" "0" "AA" "7521" "3"))))


;; Placement Functions
(define test-space1 (space-struct 0 0 600 1000))
(define test-space2 (space-struct 600 1000 80 80))
(define test-sheet (rectangular-sheet-struct 100 100 1 (list test-space1 test-space2)))
(define test-item1 (order-item-struct 600 30 1 #t))
(define test-item2 (order-item-struct 80 80 1 #t))


(define (test-fits-original-orientation-guillotine?)
  (check-true (fits-original-orientation-guillotine? test-item1 test-space1 test-sheet))
  (check-true (fits-original-orientation-guillotine? test-item2 test-space2 test-sheet))
  (check-false (fits-original-orientation-guillotine? test-item1 test-space2 test-sheet)))

(define (test-fits-rotated-guillotine?)
  (check-true (fits-rotated-guillotine? test-item2 test-space1 test-sheet))
  (check-true (fits-rotated-guillotine? test-item1 test-space1 test-sheet))
  (check-true (fits-rotated-guillotine? test-item2 test-space2 test-sheet))
  (check-false (fits-rotated-guillotine? test-item1 test-space2 test-sheet)))

(test-valid-entry?)
(test-valid-material?)
(test-clean-data)
(test-valid-row?)
; (test-fits-original-orientation-guillotine?)
(test-fits-rotated-guillotine?)