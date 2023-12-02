(declare (unit day03))
(import (chicken io))
(import (chicken string))
(import (chicken file))

(define (read-it file-path)
  (call-with-input-file file-path (lambda (port) (read-lines port))))

(define (process-line line)
  0)

; --- PART 1
(define (part-1 input)
  (display "  Part 1: ")
  (display
    (foldl + 0 (map (lambda (r) (process-line r)) input)))
  (newline))

; --- PART 2
(define (part-2 input)
  (display "  Part 2: ")
  (display
    (foldl + 0 (map (lambda (r) (process-line r)) input)))
  (newline))

(define (run-day-03)
  (display "Day 3")
  (newline)
  (cond ((not (file-exists? "input/day03.txt")) (display "ERROR: There is no input file for day 3")(newline)(exit)))
  (define task-input (read-it "input/day03.txt"))
  (part-1 task-input)
  (part-2 task-input))
 
