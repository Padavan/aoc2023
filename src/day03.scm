(declare (unit day03))

(import (chicken string))
(import (chicken file))
(import (chicken sort))
(import sequences)
(declare (uses utils))

 ; --- COMMON
(define symbols (list "-" "+" "*" "$" "#" "@" "&" "#" "/" "=" "%"))

(define digits (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

; ((1 . 4) (2 . 6) (3 . 7) (6 . 1) (7 . 1) (8 . 4)) -> (((1 . 4) (2 . 6) (3 . 7)) ((6 . 1) (7 . 1) (8 . 4)))
(define (group-coord-pairs group-list current-pair)
  (cond
    ; if empty list -> push list with one current index-coord pair
    [(equal? group-list '()) (list (list (car current-pair)))] 
    ; if last element in last group is adjusten to current index-coord pair -> push current into group
    [(equal? (- (car current-pair) (car (car group-list))) 1) (append (list (append (list (car current-pair)) (car group-list))) (list-tail group-list 1))]
    ; if last element in last group is not adjusten -> create new group
    [else (append (list (list (car current-pair))) group-list)] ))


(define (get-digit-list line)
    (foldl group-coord-pairs '()
      (partition 
        (lambda (x) (member (cdr x) digits))
        (enumerate (string-chop line 1)))))

(define (get-symbol-list line)
  (map
    (lambda (pair) (car pair))
    (partition
      (lambda (x) (member (cdr x) symbols))
      (enumerate (string-chop line 1))
    )
  )
)

(define (distance a b)
  (sqrt
    (+
      (expt (- (car b)(car a)) 2)
      (expt (- (cdr b)(cdr a)) 2))))
  
; ((0 . 0) (1 . 0) (2 . 0)) <- valid
; ((5 . 0) (6 . 0) (7 . 0)) <- invalid
(define (is_group_adjasten group symbols_list)
  (foldl
    (lambda
      (first_acc first)
      (foldl
        (lambda
          (second_acc second)
          (if (< (distance first second) 2) #t second_acc))
        first_acc
        symbols_list
      )
    )
    #f
    group
  )
)

(define (sort-func a b)
  (cond ((> (- b a) 0) #t)
        (else #f))
)

; --- PART 1

; ((0 . 0) (1 . 0) (2 . 0)) -> 467
(define (get-number-from-matrix group matrix)
  (string->number (foldl
    (lambda
      (acc xy)
      (string-append acc (list-ref (list-ref matrix (cdr xy)) (car xy)))
    )
    ""
    group)))

(define (sum-of-part-numbers input)
  
  ; get (((5 . 0) (6 . 0) (7 . 0)) ((0 . 0) (1 . 0) (2 . 0)) ((6 . 2) (7 . 2) (8 . 2)) ((2 . 2) (3 . 2)) ((0 . 4) (1 . 4) (2 . 4)) ((7 . 5) (8 . 5)) ((2 . 6) (3 . 6) (4 . 6)) ((6 . 7) (7 . 7) (8 . 7)) ((5 . 9) (6 . 9) (7 . 9)) ((1 . 9) (2 . 9) (3 . 9)))
  (define coord_groups
    (foldl
      (lambda (acc cur)
        (append
          acc
          (map
            (lambda (group)
              (map
                (lambda (x) (cons x (car cur)))
                (sort group sort-func))
            )
            (cdr cur)
          )
        )
      )
      '()
      (enumerate (map get-digit-list input))
    )
  )
    
  ; ((3 . 1) (6 . 3) (3 . 4) (5 . 5) (3 . 8) (5 . 8))
  (define symbols_list
    (foldl 
      (lambda (acc cur)
        (append acc
                (map
                  (lambda (x) (cons x (car cur)))
                  (cdr cur))))
      '()
      (enumerate (map get-symbol-list input))))
    
    
  (define coord_with_adjesten_symbols 
    (partition (lambda (group) (is_group_adjasten group symbols_list)) coord_groups)                            
  )
    
  (define matrix (map (lambda (line) (string-chop line 1)) input))
    
    
  (foldl (lambda (acc group) (+ acc (get-number-from-matrix group matrix))) 0 coord_with_adjesten_symbols)
)



; 529618
(define (part-1 input)
  (display "  Part 1: ")
  (display (sum-of-part-numbers input))
  (newline)
  )

; --- PART 2
(define (get-asterisk-list line)
  (map
    (lambda (pair) (car pair))
    (partition
      (lambda (x) (equal? (cdr x) "*"))
      (enumerate (string-chop line 1))
    )
  )
)

(define (get-digit-groups-adj-to-asterisk xy groups)
  (partition (lambda (group) (is_group_adjasten group (list xy))) groups) 
)


(define (sum-of-gear-ratio input)
  (define matrix (map (lambda (line) (string-chop line 1)) input))
  (define asterisk_list
    (foldl 
      (lambda (acc cur)
        (append acc
                (map
                  (lambda (x) (cons x (car cur)))
                  (cdr cur))))
      '()
      (enumerate (map get-asterisk-list input))))
  
  (define coord_groups
    (foldl
      (lambda (acc cur)
        (append
          acc
          (map
            (lambda (group)
              (map
                (lambda (x) (cons x (car cur)))
                (sort group sort-func))
            )
            (cdr cur)
          )
        )
      )
      '()
      (enumerate (map get-digit-list input))
    )
  )

  (define asterisk_adjusten_pairs_list
    (partition
      (lambda (group_array) (equal? (length group_array) 2))
      (map (lambda (asterisk_xy) (get-digit-groups-adj-to-asterisk asterisk_xy coord_groups)) asterisk_list)
      )  
  )
  
  (foldl
    (lambda (acc digit_pair)

      (+ acc (*
        (get-number-from-matrix (list-ref digit_pair 0) matrix)
        (get-number-from-matrix (list-ref digit_pair 1) matrix)
   )))
    0
    asterisk_adjusten_pairs_list))

; 77509019
(define (part-2 input)
  (display "  Part 2: ")
  (display (sum-of-gear-ratio input))
  (newline))

(define (run-day-03)
  (display "Day 3")
  (newline)
  (define file_input "input/day03.txt")
  (cond ((not (file-exists? file_input)) (display "ERROR: There is no input file for day 3")(newline)(exit)))
  (define task-input (read-it file_input))
  (part-1 task-input)
  (part-2 task-input))
