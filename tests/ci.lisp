(defun do-all()
  (ql:quickload :picolens/tests)
  (uiop:quit
   (if (uiop:call-function "picolens/tests:run-tests")
       0 1)))

(do-all)
