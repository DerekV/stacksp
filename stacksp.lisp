
(defun process-stack (expression stack) 
  (let ((item (car expression))
	(rest (cdr expression)))
    (cond
     ((null expression) stack)
     ((numberp item)
      (process-stack rest (append stack (list item))))
     ((functionp item)
      (process-stack rest (list (apply item stack)))))))
