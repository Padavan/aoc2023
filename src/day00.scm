(declare (unit day00))
(import (chicken io))
(import (chicken string))

(define (read-it file-path)
  (call-with-input-file file-path (lambda (port) (read-lines port))))

(define (read-file-to-list file)
  (read-list (open-input-file file)))

(define (run-day-00)
  ; (print "  Part 1: ")
  ; (newline)
  (define task-input (read-it "input/day00.txt"))
  ; (display (car task-input))
  (newline))
