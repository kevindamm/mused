;;;; Unit test framework.

;;; Library for a test harness for executing unit tests.  Example:
;;;
;;;   (deftest test-+ ()
;;;     (check
;;;       (= (+ 1 2) 3)
;;;       (= (+ 1 2 3) 6)
;;;       (= (+ -1 -3) -4)))
;;;
;;; Multiple tests can also be combined using deftest:
;;;
;;;   (deftest test-arithmetic ()
;;;     (combine-results
;;;       (test-+)
;;;       (test-*)))
;;;
;;; The output will indicate the full hierarchy of tests being evaluated.

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;; Check that each following form evaluates to non-NIL and return T or NIL.
(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

;; Evaluates each form for being non-NIL and returns T or NIL.
(defmacro combine-results (&body forms)
  (let ((result (gensym)))
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

;; Prints whether the test failed or succeeded.
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)
