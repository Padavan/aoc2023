(declare (unit day02))
(import (chicken io))
(import (chicken string))
(import (chicken file))

(define (read-it file-path)
  (call-with-input-file file-path (lambda (port) (read-lines port))))

; --- COMMON
(define (assign-rgb acc number_color_string)
  (let ((number_color_list (string-split number_color_string)))
    (cond ((equal? (list-ref number_color_list 1) "blue") (set-car! (list-tail acc 2) (string->number (list-ref number_color_list 0))))
          ((equal? (list-ref number_color_list 1) "red") (set-car! (list-tail acc 0) (string->number (list-ref number_color_list 0))))
          ((equal? (list-ref number_color_list 1) "green") (set-car! (list-tail acc 1) (string->number (list-ref number_color_list 0))))))
   acc)

(define (parse_game game)
  (map (lambda (game_set) (foldl assign-rgb (list 0 0 0) 
    (string-split game_set "#,"))) (string-split game "#;")
))
       
(define (check_rgb_rules current_status rgb)
  (cond ((or
           (> (list-ref rgb 0) 12)
           (> (list-ref rgb 1) 13)
           (> (list-ref rgb 2) 14)
          ) #f)
      (else current_status))  
  )

; --- PART 1
; return 0 if game impossible or ID
(define (get-id-or-null line)
  (define id
    (string->number (list-ref
      (string-split
        (car (string-split line "#:"))
        "# "
      )
      1)))
  
  (define game_data
    (parse_game (list-ref (string-split line "#:") 1)))
  
  (define game_possible? (foldl check_rgb_rules #t game_data))
  
  (if game_possible? id 0))


(define (part-1 input)
  (display "  Part 1: ")
  (display
    (foldl + 0 (map (lambda (r) (get-id-or-null r)) input)))
  (newline))
; 2076

; --- PART 2
(define (get-power-list acc current)
  (list 
    (if (> (list-ref current 0) (list-ref acc 0)) (list-ref current 0) (list-ref acc 0))
    (if (> (list-ref current 1) (list-ref acc 1)) (list-ref current 1) (list-ref acc 1))
    (if (> (list-ref current 2) (list-ref acc 2)) (list-ref current 2) (list-ref acc 2))
  ))

(define (get-power line)
  (define id
    (string->number (list-ref
      (string-split
        (car (string-split line "#:"))
        "# "
      )
      1)))
  
  (define game_data
    (parse_game (list-ref (string-split line "#:") 1)))
    
  (foldl * 1
    (foldl get-power-list (list 0 0 0) game_data)))

(define (part-2 input)
  (display "  Part 2: ")
  (display
    (foldl + 0 (map (lambda (r) (get-power r)) input)))
  (newline))
; 70950

(define (run-day-02)
  (display "Day 2")
  (newline)
  (cond ((not (file-exists? "input/day03.txt")) (display "ERROR: There is no input file for day 3")(newline)(exit)))
  (define task-input (read-it "input/day02.txt"))
  (part-1 task-input)
  (part-2 task-input))
 
