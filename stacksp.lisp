
(defun process-stack (expression &optional stack)
  (let ((item (car expression))
	(rest (cdr expression)))
    (cond
     ((null expression) stack)
     ((and (symbolp item) (fboundp item))
      (process-stack rest (list (apply (symbol-function item) stack))))
     (T
      (process-stack rest (append stack (list item)))))))

(defun string-to-stack (str)
  (with-input-from-string (in str)
			  (loop for
				x = (read in nil nil)
				while x
				collect x)))
(defun stack-to-string (stack)
  (format nil "~a" stack))

(defun read-stack-from-line ()
  (string-to-stack (read-line)))

(defun print-stack-immediate (stack)
    (write-line (stack-to-string stack))
    (finish-output))

(defun stackp-repl ()
  (loop for result = (process-stack (read-stack-from-line))
        until (equal result '(done))
        do (print-stack-immediate result)))
