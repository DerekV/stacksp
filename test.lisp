
(load "./stacksp.lisp")


(defparameter *lisp-list-tests*
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

(defparameter *string-tests* 
'(
  ( "Empty is empty" "" "" )
  ( "A B C is A B C" "A B C" "A B C" )
  ;;( "A nested stack" "A B C [ 1 2 3 ] FIN" "A B C [ 1 2 3 ] FIN" )
  ( "Some addition and subtraction" "START 1 2 3 + 10 - + END" "START 6 END" )
))

(defun test-lisp-lists (tests) 
  (run-tests-using tests 
		   (lambda (input)
		     (reverse (get-stack (process-deck input))))
		   (lambda (expected-output actual-output) 
		     (equal expected-output actual-output))))

(defun test-input-strings (tests)
  (run-tests-using tests 
		   (lambda (input)
		     (stack-to-string (get-stack (process-deck (string-to-stack input)))))
		   (lambda (expected-output actual-output)
		     (equal expected-output (string-trim " " actual-output)))))

(defun run-tests-using (tests output-generator output-verifier)
  (mapcar (lambda (test-case)
	    (destructuring-bind (description input expected-output) test-case
	      (let ((actual-output (funcall output-generator input)))
		(list
		 description
		 (funcall output-verifier expected-output actual-output)
		 input
		 expected-output
		 actual-output))))
	  tests))

(defun print-stacksp-test-report (report)
  (mapcar (lambda (test-result)
	    (destructuring-bind (description success input expected-output actual-output) test-result
	      (format t "============~%~:[FAIL~;pass~]: ~a~%For       ~a~%Expected  ~a~%Got       ~a~%~%" 
		      success description input expected-output actual-output)
	      success))
	  report))

(defun all-true (list) 
  (every #'identity list))

(defun deep-all-true (tree)
  (if (consp tree)
      (every #'deep-all-true tree)
    tree))

(defun run-tests () 
  (deep-all-true
   (list 
    (print-stacksp-test-report (test-lisp-lists *lisp-list-tests*))
    (print-stacksp-test-report (test-input-strings *string-tests*)))))
