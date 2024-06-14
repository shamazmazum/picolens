(in-package :picolens/tests)

(serapeum:defconstructor node
  (value integer)
  (left  (or node null))
  (right (or node null)))

(picolens:gen-lenses node
  (node-value value)
  (node-left  left)
  (node-right right))

(defparameter *tree*
  (node 1
        (node 2
              (node 3
                    (node 4
                          (node 5 nil nil)
                          (node 6 nil nil))
                    (node 7
                          (node 8 nil nil)
                          (node 9 nil nil)))
              (node 10
                    (node 11
                          (node 12 nil nil)
                          (node 13 nil nil))
                    (node 14
                          (node 15 nil nil)
                          (node 16 nil nil))))
        (node 17
              (node 18
                    (node 19
                          (node 20 nil nil)
                          (node 21 nil nil))
                    (node 22
                          (node 23 nil nil)
                          (node 24 nil nil)))
              (node 25
                    (node 26
                          (node 27 nil nil)
                          (node 28 nil nil))
                    (node 29
                          (node 30 nil nil)
                          (node 31 nil nil))))))

(defun run-tests ()
  (explain! (run 'lens)))

(def-suite lens :description "Picolens test suite")

(in-suite lens)

(test axioms
  (si:do-iterator (lens (si:power (si:list->iterator (list #'left #'right)) 4))
    (let ((lens (picolens:compose (apply #'picolens:compose lens) #'value)))
      ;; set lens (view lens s) s == s
      (is (equalp (picolens:set lens (picolens:view lens *tree*) *tree*) *tree*))
      ;; view lens (set lens v s) == v
      (let ((v (random 1000)))
        (is (= (picolens:view lens (picolens:set lens v *tree*)) v)))
      ;; set lens v1 (set lens v2 s) == set lens v1 s
      (let ((v (random 1000)))
        (is (equalp (picolens:set lens v (picolens:set lens (random 10000) *tree*))
                    (picolens:set lens v *tree*)))))))

(test over-id
  (si:do-iterator (lens (si:power (si:list->iterator (list #'left #'right)) 4))
    (let ((lens (picolens:compose (apply #'picolens:compose lens) #'value)))
      (is (equalp *tree* (picolens:over lens #'identity *tree*))))))

(test compose-associativity
  (si:do-iterator (lens (si:power (si:list->iterator (list #'left #'right)) 4))
    (let ((lens (append lens (list #'value)))
          (v (random 1000)))
      (is (= (picolens:view (reduce #'picolens:compose lens :from-end nil) *tree*)
             (picolens:view (reduce #'picolens:compose lens :from-end t)   *tree*)))

      (is (equalp (picolens:set (reduce #'picolens:compose lens :from-end nil) v *tree*)
                  (picolens:set (reduce #'picolens:compose lens :from-end t)   v *tree*))))))

(test compose-identity
  (si:do-iterator (lens (si:power (si:list->iterator (list #'left #'right)) 4))
    (labels ((%collect (list acc)
               (cond
                 ((null list) acc)
                 ((zerop (random 2))
                  (%collect (cdr list) (picolens:compose acc (car list))))
                 (t
                  (%collect list (picolens:compose acc #'picolens:empty))))))
      (let* ((lens (append lens (list #'value)))
             (l1 (apply #'picolens:compose lens))
             (l2 (%collect lens #'picolens:empty))
             (v (random 1000)))
        (is (= (picolens:view l1 *tree*)
               (picolens:view l2 *tree*)))
        (is (equalp (picolens:set l1 v *tree*)
                    (picolens:set l2 v *tree*)))))))
