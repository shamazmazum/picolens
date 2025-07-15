(defsystem :picolens
  :name :picolens
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Tiny functional references to structure slots for Common Lisp"
  :licence "2-clause BSD"
  :pathname "src"
  :components ((:file "package")
               (:file "picolens" :depends-on ("package")))
  :in-order-to ((test-op (load-op "picolens/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (funcall
                     (symbol-function
                      (intern (symbol-name '#:run-tests)
                              (find-package :picolens/tests))))))

(defsystem :picolens/tests
  :name :picolens/tests
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:picolens :fiveam :stateless-iterators))
