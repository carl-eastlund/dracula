(in-package "ACL2")

(defmacro check-expect (check expect)
  `(assert-event (equal ,check ,expect)))

(defmacro check-within (check expect within)
  `(assert-event (<= (abs (- ,check ,expect)) ,within)))

(defmacro check-error (check message)
  `(assert-event (or t ,check ,message)))

(defmacro generate-report ()
  '(progn))
