(declare (unit day01))
(import (chicken io))
(import (chicken string))
(import regex)

(define (read-it file-path)
  (call-with-input-file file-path (lambda (port) (read-lines port))))

(define (read-file-to-list file)
  (read-list (open-input-file file)))

(define (last_element l)
  (cond ((null? (cdr l)) (car l))
    (else (last_element (cdr l)))))

(define (get-calibration-value line)
  (define numbers (grep "\\d+" (string-chop line 1)))
  (string->number (conc (car numbers) (last_element numbers))))

(define (part-1 input)
  (display "  Part 1: ")

  (display
    (foldl + 0 (map (lambda (r) (get-calibration-value r)) input)))
  (newline))

(define letter_to_digit_map '(("one" . "1") ("two" . "2") ("three" . "3") ("four" . "4") ("five" . "5") ("six" . "6") ("seven" . "7") ("eight" . "8") ("nine" . "9")))

(define letter_to_digit_map_reverse '(("eno" . "1") ("owt" . "2") ("eerht" . "3") ("ruof" . "4") ("evif" . "5") ("xis" . "6") ("neves" . "7") ("thgie" . "8") ("enin" . "9")))

(define (get-calibration-value-with-letters line)
  (define numbers (grep "\\d+" (string-chop
    (string-translate* line letter_to_digit_map)
    1)))
  
  (define reverse-numbers (grep "\\d+" (string-chop
    (string-translate* (reverse-list->string (string->list line)) letter_to_digit_map_reverse)
    1)))
  
  (string->number (conc (car numbers) (car reverse-numbers))))

(define (part-2 input)
  (display "  Part 2: ")
  (display
    (foldl + 0 (map (lambda (r) (get-calibration-value-with-letters r)) input)))
  (newline))

(define (run-day-01)
  (display "Day 1")
  (newline)
  (define task-input (read-it "input/day01.txt"))
  (part-1 task-input)
  (part-2 task-input))
 
