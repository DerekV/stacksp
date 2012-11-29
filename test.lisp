
(load "./stacksp.lisp")


(defparameter *tests*
  '(
    ( "A short quine with numbers" (1 2) (1 2) )
    ( "Null identity" () () )
    ( "Hello world quine" (hello world) (hello world))
    ( "Ten minus three" (3 10 -) (7) )
    ( "Greedy add" (5 10 15 20 25 +!) (75))
    ( "Greedy subtract" (2 3 5 7 100 -!) (83))
    ( "Hundred plus hundred" (100 100 +) (200))
    ( "Add consumes two items" (leave-me 1 2 3 5 7 + leave-me) (leave-me 1 2 3 12 leave-me))
    ( "Add consumes two items" (leave-me 1 2 3 5 7 - leave-me) (leave-me 1 2 3 2 leave-me))
    ( "Dup 0 actually removes an item" (1 2 3 4 0 dup) (1 2 3))
    ( "A nested stack is a nested stack" (1 2 3 (a b c) 4 5 6) (1 2 3 (a b c) 4 5 6))
    ( "Duping a stack" ( start (a stack is nice) 3 dup end) (start (a stack is nice) (a stack is nice) (a stack is nice) end))
    ( "Trash some stuff from a stack" (a b c d e f g 4 trash) (a b c ) )
    ( "Trash 0" (a b c d 0 trash fin) (a b c d fin))
    ( "Define an add2 function" ( (2 x +) (x) add2 define 99 add2 0 add2) (101 2))
    ( "Define a niave multiplication function" 
      ( (y x dup +!) (y x) my-mult define 2 25 my-mult 12 12 my-mult)
      (50 144))
    ))

(defun test-stacksp (tests) 
  (mapcar (lambda (test-case)
	    (destructuring-bind (description input-deck expected-output) test-case
	      (let ((actual-output (reverse (get-stack (process-deck input-deck)))))
		(list
		 description
		 (equal expected-output actual-output)
		 input-deck
		 expected-output
		 actual-output))))
	  tests))

(defun print-stacksp-test-report (report)
  (mapcar (lambda (test-result)
	    (destructuring-bind (description success input-deck expected-output actual-output) test-result
	      (format t "============~%~:[FAIL~;pass~]: ~a~%For       ~a~%Expected  ~a~%Got       ~a~%~%" 
		      success description input-deck (reverse expected-output) (reverse actual-output))
	      success))
	  report))

(defun test-and-print-report (tests)
  (print-stacksp-test-report (test-stacksp tests)))

