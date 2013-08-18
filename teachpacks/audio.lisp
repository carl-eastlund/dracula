(in-package "ACL2")
(encapsulate (((play-wav * *) => *))
   (local
    (defun play-wav (x a)
      (declare (ignore x a))
      t))
   (defthm play-produces-boolean
     (booleanp (play-wav x a))))
