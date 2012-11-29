
(defparameter root-context
  (let*
      ((new-stack nil)
       (ftbl (make-hash-table))
       (lookup
	(lambda (s)
	  (or (gethash s ftbl)
	      (lambda (ctx) (context-with ctx (cons s (get-stack ctx)))))))
       (add
	(lambda (s f)
	  (setf (gethash s ftbl) f))))
    (vector new-stack lookup add)))

(defun context-with (old-context stack)
  (if (not (vectorp old-context)) (format t "context ~a " old-context))
  (vector stack (elt old-context 1) (elt old-context 2)))

(defun make-context (&optional parent-context initial-stack)
  (let ((parent (or parent-context root-context))
	(ftbl (make-hash-table)))
    (let
	((lookup (lambda (s)
		   (or (gethash s ftbl)
		       (resolve parent s))))
	 (add (lambda (s f) (setf (gethash s ftbl) f))))
      (vector initial-stack lookup add))))

(defun resolve (context symb)
  (if (not (vectorp context)) (format t "context ~a " context))
  (funcall (elt context 1) symb))

(defun add-function (context symb function)
  (funcall (elt context 2) symb function)
  context)  ;; todo - destuctive


(defun get-stack (context)
  (elt context 0))

(defun process-deck (deck &optional old-context)
  (let ((ctx (or old-context root-context)))
    (if (null deck)
	ctx
      (let* ((item (pop deck))
	     (rest deck)q
	     (fun (resolve ctx item)))
	(process-deck rest (funcall fun ctx))))))

(defun string-to-stack (str)
  (with-input-from-string (in str)
			  (loop for
				x = (read in nil nil)
				while x
				collect x)))

(defun stack-to-string (stack)
  (loop for x in (reverse stack) collect
	(cond
	 ((null x) "NIL ")
	 ((listp x)
	  (concatenate 'string
		       "[ "
		       (stack-to-string x)
		       "] "))
	 (T (format nil "~a " x)))
	into z
	finally (return (apply #'concatenate (cons 'string z)))))

(defun read-stack-from-line ()
  (string-to-stack (read-line)))

(defun print-stack-immediate (stack)
  (write-line (stack-to-string stack))
  (finish-output))

(defun stackp-repl ()
  (loop for result = (get-stack (process-deck (read-stack-from-line)))
        until (equal result '(done))
        do (print-stack-immediate result)))

(add-function root-context '+ (lambda (context)
				(let ((stack (get-stack context)))
				  (context-with context
						(cons (funcall #'+ (pop stack) (pop stack)) stack)))))

(add-function root-context '- (lambda (context)
				(let ((stack (get-stack context)))
				  (context-with context
						(cons (funcall #'- (pop stack) (pop stack)) stack)))))

(add-function root-context '+! (lambda (context)
				 (let ((stack (get-stack context)))
				   (context-with context
						 (list (apply #'+ stack))))))

(add-function root-context '-! (lambda (context)
				 (let ((stack (get-stack context)))
				   (context-with context
						 (list (apply #'- stack))))))

(add-function root-context 'dup (lambda (context)
				  (let* ((stack (get-stack context))
					 (times (pop stack))
					 (item (pop stack)))
				    (context-with context
						  (dotimes (i times stack) (push item stack))))))

(add-function root-context 'trash (lambda (context)
				    (let* ((stack (get-stack context))
					   (howmany (pop stack)))
				      (context-with context
						    (nthcdr howmany stack)))))

(add-function root-context 'context (lambda (context)
				      (let* ((stack (get-stack context))
					     (howmany (pop stack)))
					(context-with context
					 (cons (subseq stack 0 howmany) (nthcdr howmany stack))))))

(add-function root-context 'define
	      (lambda (defining-context)
		(let* ((stack (get-stack defining-context))
		       (symb (pop stack))
		       (var-list (pop stack))
		       (deck (pop stack)))
		  (finish-output)
		  (setf (elt defining-context 0) stack)
		  (add-function defining-context symb
				(lambda (calling-context)
				  (let ((local-context (make-context defining-context nil))
					(stack-of-caller (get-stack calling-context)))
				    ;; bind params
				    (loop for param in var-list do
					  (let ((bound-value (pop stack-of-caller)))
					    (finish-output)
					    (add-function local-context param
							  (lambda (supra-context)
							    (context-with supra-context
									  (cons bound-value (get-stack supra-context)))))))
				  (let ((processed (process-deck deck local-context)))
				    (context-with calling-context (append (get-stack processed) stack-of-caller))))))
		  (context-with defining-context stack))))
