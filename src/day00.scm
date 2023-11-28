(declare (unit day00))
(import (chicken io))

(define (read-it file)
  (define input-list list ())
  (let ([fh (open-input-file file)])
    (let loop ([line (read-line fh)])
      (if (eof-object? line)
          (close-input-port fh)
          (begin
            (print line)
            (loop (read-line fh))
            ))))
  )

(define (read-file-to-list file)
  (read-list (open-input-file file)))

(define (run-day-00)
  (display "day00")
  (newline)
  (display (read-it "input/day00.txt"))
)





