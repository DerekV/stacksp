
(defparameter root-context
  (let* 
      (
       (ftbl (make-hash-table))
       (lookup 
	(lambda (s)
	  (or (gethash s ftbl)
	      (lambda (stack) (cons s stack)))))
       (add
	(lambda (s f)
	       (setf (gethash s ftbl) f))))
    (vector lookup add)))

(defun make-context (&optional parent-context)
  (let ((parent (or parent-context root-context))
	(ftbl (make-hash-table)))
    (let
	((lookup (lambda (s) 
		   (or (gethash s ftbl)
		       (resolve parent s))))
	 (add (lambda (s f) (setf (gethash s ftbl) f))))
      (vector lookup add))))

(defun resolve (context symb)
  (funcall (elt context 0) symb))

(defun add-function (context symb function)
  (funcall (elt context 1) symb function))


(defun process-stack (expression &optional stack context)
  (if (null expression)
      stack
    (let* ((ctx (or context (make-context)))
	   (item (pop expression))
	   (rest expression)
	   (fun (resolve ctx item)))
      (process-stack rest (funcall fun stack)))))

(defun string-to-stack (str)
  (with-input-from-string (in str)
			  (loop for
				x = (read in nil nil)
				while x
				collect x)))

(defun stack-to-string (stack)
  (format nil "~a" (reverse stack)))

(defun read-stack-from-line ()
  (string-to-stack (read-line)))

(defun print-stack-immediate (stack)
  (write-line (stack-to-string stack))
  (finish-output))

(defun stackp-repl ()
  (loop for result = (process-stack (read-stack-from-line))
        until (equal result '(done))
        do (print-stack-immediate result)))


(add-function root-context '+ (lambda (stack) (list (apply #'+ stack))))
(add-function root-context '- (lambda (stack) (list (apply #'- stack))))
