
(defun process-stack (expression &optional stack) 
  (let ((item (car expression))
	(rest (cdr expression)))
    (cond
     ((null expression) stack)
     ((numberp item)
      (process-stack rest (append stack (list item))))
     ((fboundp item)
      (process-stack rest (list (apply (symbol-function item) stack)))))))

(defun read-stack-from-line ()
  ( let 
    ((line (read-line))) 
    (with-input-from-string (in line) 
			    (loop for
				  x = (read in nil nil) 
				  while x 
				  collect x))))
(defun stackp-repl ()
  (loop for result = (process-stack (read-stack-from-line))
        until (equal result '(done))
        do
        (format t "~a~%" result)
        (finish-output)))
