 #lang racket

 (provide (struct-out rectangular-sheet-struct)
          (struct-out order-item-struct)
          (struct-out cutting-pattern-struct)
          (struct-out space-struct))

 ;;use define-struct/contract?
(struct rectangular-sheet-struct (width height material-id [available-spaces #:mutable]))
(struct order-item-struct (width height material-id rotate))
(struct cutting-pattern-struct (x y width height))
(struct space-struct (x y width height))

