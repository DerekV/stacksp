
(defun make-context ()
  (lambda (s)
    (cond
     ((equal s '+) (lambda (stack) (list (apply #'+ stack))))
     (T (lambda (stack) (cons s stack))))))

(defun resolve (context symb)
  (funcall context symb))

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
