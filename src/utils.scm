(declare (unit utils))
(import (chicken io))

(define (read-it file-path)
  (call-with-input-file file-path (lambda (port) (read-lines port))))

(define (enumerate list)
  (let ((idx 0))
    (map
      (lambda (item) (let ((return-item (cons idx item))) (set! idx (+ idx 1)) return-item)) 
      list)
    ))

(define (last l)
  (cond ((null? (cdr l)) (car l))
    (else (last_element (cdr l)))))

(define (printf str)
  (display (quote 'str))(display ": ")(display str)(newline))