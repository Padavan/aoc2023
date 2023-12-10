(declare (unit day04))

(import (chicken string))
(import (chicken file))
(declare (uses utils))

(define (parse_card line)
  (define card_id (string->number (list-ref (string-split (car (string-split line ":"))) 1)))
  (define winning_numbers (map (lambda (str_num) (string->number str_num)) (string-split (list-ref (string-split (list-ref (string-split line ":") 1) "|") 0))))
  (define numbers_you_have (map (lambda (str_num) (string->number str_num)) (string-split (list-ref (string-split (list-ref (string-split line ":") 1) "|") 1))))
  (list card_id winning_numbers numbers_you_have)
  )

; --- PART 1
(define (get-points-from-card id_win_have_list)
  (define winning_numbers (list-ref id_win_have_list 1))
  (define numbers_you_have (list-ref id_win_have_list 2))
  (define points
    (foldl
      (lambda (acc cur) (if (member cur winning_numbers) (* acc 2) acc))
      0.5
      numbers_you_have))
  (if (eq? points 0.5) 0 points)
)

(define (total-points input)
  (foldl (lambda (acc line) (+ acc (get-points-from-card (parse_card line)))) 0 input))

(define (part-1 input)
  (print "  Part 1: " (total-points input)))

; ---PART 2
(define (get-id-copies-pair id_win_have_list)
  (define card_number (list-ref id_win_have_list 0))
  (define winning_numbers (list-ref id_win_have_list 1))
  (define numbers_you_have (list-ref id_win_have_list 2))
  
  (define copies
    (foldl
      (lambda (acc cur) (if (member cur winning_numbers) (+ acc 1) acc))
      0
      numbers_you_have))
  (cons card_number copies)
)

(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))
    )
  )
)

(define (new_vector mapping idx addition)
  (vector-set! mapping idx (+ (vector-ref mapping idx) addition))
  mapping
)

(define (make_new_mapping mapping id_copies_pair idx)
  (define multiplier (vector-ref mapping idx))
  
  (let loop ((count 1) (m mapping))
    (if (> count (cdr id_copies_pair))
        m
        (loop (+ count 1) (new_vector m (+ idx count) multiplier))
    )
  )
  mapping
)

(define (play score_mapping id_copies_pair_list)
  (let round ((mapping score_mapping) (count 0))
    (if (eq? count (vector-length score_mapping))
        mapping
        (round (make_new_mapping
                 mapping
                 (assq (+ count 1) id_copies_pair_list)
                 count)
               (+ count 1))
  ))
  
  )

(define (total-cards input)
  (define cards_number (length input))
  (define score_mapping (make-vector cards_number 1))
  (vector-set! score_mapping 0 1)

  (define id_copies_pair_list (map (lambda (line) (get-id-copies-pair (parse_card line))) input))
  
  (foldl (lambda (acc cur) (+ acc cur)) 0 (vector->list (play score_mapping id_copies_pair_list))))

(define (part-2 input)
  (print "  Part 2: " (total-cards input)))

(define (run-day-04)
  (display "Day 4")
  (newline)
  (define file_input "input/day04.txt")
  (cond ((not (file-exists? file_input)) (display "ERROR: There is no input file ")(display file_input)(newline)(exit)))
  (define task-input (read-it file_input))
  (part-1 task-input)
  (part-2 task-input)
  (newline))
