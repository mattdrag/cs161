;; Matt Dragotto - 804472271

(defun PAD(n)(cond ((= n 0) 1)((= n 1) 1)((= n 2) 1)(T (+ (PAD (- n 2)) (PAD (- n 3))))))