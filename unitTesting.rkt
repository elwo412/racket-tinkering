#lang racket/base

(require rackunit
         "racketlab.rkt")
(require rackunit/text-ui)

(define test-gauntlet
    (test-suite "test-gauntlet"
        (check-equal? (remove-when (lambda (x) (> x 3)) '(10 1 7 2)) '(1 2) "Question 1: Case 1")
        (check-equal? (minInt '(4 5 6 9 10)) 4 "Question 2: Case 1")
        (check-equal? (minInt '(5 2 6 9 10)) 2 "Question 2: Case 2")
        (check-equal? (minInt '(5 6 1 10)) 1 "Question 2: Case 3")
        (check-equal? (minInt '(4 5 6 -1 10)) -1 "Question 2: Case 4")
        (check-equal? (minInt '(4 5 6 9 -99)) -99 "Question 2: Case 5")
        (check-equal? (minInt '(15 20 5 2 8 15 3 14 13 6
                                6 1 9 2 9 12 7 15 18 20
                                17 11 15 4 15 15 10 14 17 14
                                19 20 19 11 19 10 17 19 5 17
                                9 15 3 9 20 9 16 18 17 10 -1)) -1 "Question 2: Case 6")
        (check-equal? (minInt '(4 8 6 5 5 4 2 3 8 0 2 2 0 4 6 0 5 0 3 5 4 3 6 9 7
                                7 0 6 6 8 9 6 3 3 7 6 5 9 1 2 4 3 6 4 1 4 7 7 7 5
                                4 9 2 6 2 7 3 0 2 5 0 3 1 9 1 5 9 8 3 0 0 6 7 3 7
                                8 5 5 5 5 3 2 7 5 5 8 2 1 5 9 9 4 9 8 9 0 8 5 1 5
                                7 3 6 0 0 1 2 1 3 0 8 4 6 3 0 9 5 8 7 0 5 7 7 7 0
                                8 3 2 2 4 5 6 6 8 5 4 7 5 1 2 0 5 6 6 6 1 2 7 9 1
                                4 9 5 3 7 5 2 9 6 0 0 3 5 2 5 2 7 2 3 3 7 9 7 5 4
                                5 2 6 8 1 6 0 7 7 1 9 3 6 7 5 9 1 9 8 4 7 8 0 7 7
                                4 8 8 1 9 7 1 6 0 5 8 8 1 8 8 2 0 7 8 9 5 8 5 3 7
                                3 1 3 0 4 8 1 4 8 4 3 0 2 4 9 0 6 3 2 9 9 4 6 1 9
                                6 4 1 1 9 2 4 2 6 0 8 6 9 7 8 0 5 6 9 4 0 4 9 8 2
                                1 2 4 0 7 5 9 7 6 5 0 0 1 0 4 6 3 2 3 8 1 2 6 2 9
                                7 5 8 1 1 9 4 3 6 5 2 2 8 6 5 0 8 6 3 5 5 4 5 1 4
                                0 1 4 1 1 2 2 7 8 4 5 6 7 4 4 6 2 1 7 6 6 3 1 7 6
                                5 0 6 1 3 9 4 1 8 0 0 2 3 4 4 9 9 5 5 1 6 4 2 3 0
                                0 6 7 6 0 3 9 2 3 0 9 5 2 4 2 8 3 9 3 2 3 1 4 0 3
                                4 9 2 4 8 4 0 8 7 1 6 4 0 5 6 7 5 0 6 7 1 0 5 9 0
                                3 1 3 8 8 8 4 6 2 1 4 7 6 1 8 3 5 6 5 3 4 3 4 4 9
                                4 7 7 3 9 3 0 9 7 0 7 0 4 3 5 9 5 1 5 5 6 6 0 6 8
                                5 6 4 2 4 9 0 2 4 0 7 0 6 3 5 8 6 9 4 1 6 2 1 8 6
                                6 4 0 2 9 7 7 2 2 0 6 9 9 9 8 4 5 3 4 6 7 1 4 6 2
                                2 2 2 2 5 5 3 4 2 4 4 5 7 2 6 4 3 6 8 4 8 0 4 7 5
                                5 4 5 7 4 8 7 7 8 6 2 6 2 9 8 6 2 3 6 9 6 8 3 5 7
                                7 1 8 3 9 6 5 0 3 8 4 8 8 2 7 4 1 9 1 0 6 4 5 8 1
                                1 7 9 4 4 7 7 7 6 7 1 5 5 0 2 3 6 1 5 3 7 9 7 4 3
                                9 1 8 7 5 0 0 8 2 1 9 4 8 6 5 8 4 4 1 7 5 7 0 4 3
                                9 8 6 2 8 2 3 4 9 0 0 5 5 3 8 0 6 1 4 9 7 5 3 9 0 -2)) -2 "Question 2: Case 7")

        (check-equal? (manycall 7 (lambda (x) (+ x 1)) 10) 16 "Question 3: Case 1")
        (check-equal? (manycall 6 (lambda (x) (+ x 1)) 10) 16 "Question 3: Case 2")
        (check-equal? (manycall 0 (lambda (x) (+ x 1)) 10) 10 "Question 3: Case 3")
        (check-equal? (manycall 1 (lambda (x) (+ x 1)) 10) 10 "Question 3: Case 4")
        ;add more unit tests to manycall
        (check-equal? (to-words 0) "zero" "Question 4: Case 0")
        (check-equal? (to-words -1) 'error "Question 4: Case 1")
        (check-equal? (to-words 200) 'error "Question 4: Case 2")
        (check-equal? (to-words 1) "one" "Question 4: Case 3")
        (check-equal? (to-words 199) "one hundred and ninety nine" "Question 4: Case 4")
        (check-equal? (to-words 155) "one hundred and fifty five" "Question 4: Case 5")
        (check-equal? (to-words 42) "forty two" "Question 4: Case 6")
        (check-equal? (to-words 13) "thirteen" "Question 4: Case 7")
        (check-equal? (to-words 100) "one hundred" "Question 4: Case 8")
        (check-equal? (to-words 101) "one hundred and one" "Question 4: Case 8")
        question5Tests

        
        )
    )

(define question5Tests
    (test-suite "question5Tests"
        ;PART A
        (check-equal? (initialWIList '()) '() "Case 1")
        (check-equal? (initialWIList '(time is)) '((time 0) (is 1)) "Case 1")
        (check-equal? (initialWIList '(time is long but life is short)) '((time 0) (is 1) (long 2) (but 3) (life 4) (is 5) (short 6)) "Case 2")
        (check-equal? (initialWIList '(racket is very co0l)) '((racket 0) (is 1) (very 2) (co0l 3)) "Case 3")

        ;PART A 2
        (check-equal? (initialWIListHelper 3 '(racket is very co0l)) '((racket 3) (is 4) (very 5) (co0l 6)) "Case 1 ptb")
        (check-equal? (initialWIListHelper 4 '(life is short)) '((life 4) (is 5) (short 6)) "Case 2 ptb")

        ;PART B
        (check-equal? (mergeWI '(life 4) '((time 0) (is 5))) '((time 0) (is 5) (life 4)) "Case 1 ptc") ; pair not in word list
        (check-equal? (mergeWI '(is 1) '((time 0) (is 5))) '((time 0) (is 5)) "Case 2 ptc") ; pair is less than pair in word list
        (check-equal? (mergeWI '(timed 1) '((timed 0) (is 5))) '((timed 1) (is 5)) "Case 3 ptc") ; pair is greater than pair in word list
        ;NOT SURE IF VALID CASE (check-equal? (mergeWI '(is 7) '((time 0) (is 5) (is 6))) '((time 0) (is 7)) "Case 4 ptc") ; multiple pairs in word list (greater) 

        ;PART C
        (check-equal? (mergeByWord '((time 0) (is 1) (long 2) (but 3) (life 4) (is 5) (short 6))) '((time 0) (long 2) (but 3) (life 4) (is 5) (short 6)) "Case 1 ptC") 
        (check-equal? (mergeByWord '((roses 0) (are 1) (red 2) (apples 3) (are 4) (red 5) (this 6) (poem 7) (has 8) (no 9) (point 10))) '((roses 0) (apples 3) (are 4) (red 5) (this 6) (poem 7) (has 8) (no 9) (point 10)) "Case 2 ptC")
        (check-equal? (mergeByWord '((silly 0) (sally 1) (was 2) (sad 3) (sally 4) (is 5) (silly 6))) '((was 2) (sad 3) (sally 4) (is 5) (silly 6)) "Case 3 ptC")
        (check-equal? (mergeByWord '((bad 0) (bad 1) (good 2) (bad 3) (bad 4) (bad 5) (good 6))) '((bad 5) (good 6)) "Case 4 ptC")
        (check-equal? (mergeByWord '((bad 0) (bad 1) (bad 2) (bad 3) (bad 4) (bad 5) (bad 6))) '((bad 6)) "Case 5 ptC")

        ;FINAL PART
        (check-equal? (wordMaxIndex '(Hi silly sally sally was surely silly)) '((Hi 0) (sally 3) (was 4) (surely 5) (silly 6)) "Case 1 Final")
        (check-equal? (wordMaxIndex '(Timmy turner was turning Timmy junior around the corner he was)) '((turner 1) (turning 3) (Timmy 4) (junior 5) (around 6) (the 7) (corner 8) (he 9) (was 10)) "Case 2 Final")
        (check-equal? (wordMaxIndex '(aba aba aba aba baa aba aba)) '((baa 4) (aba 6)) "Case 3 Final")
        )
    )


(run-tests test-gauntlet 'verbose)
;(run-tests question5Tests 'verbose)