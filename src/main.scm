(import (chicken base))
(declare (uses day00))
(declare (uses day01))
(declare (uses day02))
(declare (uses day03))
(declare (uses day04))


(define (greetings)
  (display "Advent Of Code 2023 in Chicken Scheme")
  (newline))

(greetings)
(run-day-00)
(run-day-01)
(run-day-02)
(run-day-03)
(run-day-04)
