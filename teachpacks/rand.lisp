#|
  Modified by Dale Vaillancourt 14 January 2006
  Removed uses of ACL2 state; the seed is now passed functionally.
  Modified by Carl Eastlund 14 May 2006
  Added next-seed function for updating the seed.
------------------------------------------------------------------

  A Simple Random Number Generator              Version 0.3
  Carl Eastlund <cce@ccs.neu.edu>              June 3, 2008
  Originally by: Jared Davis <jared@cs.utexas.edu>
  
  This file is in the public domain.  You can freely use it for any
  purpose without restriction. 
 
  This is just a basic pure multiplicative pseudorandom number gen-
  erator.  *M31* is the 31st mersenne prime, and *P1* is 7^5 which
  is a primitive root of *M31*, ensuring that our period is M31 - 1.
  This idea is described in "Elementary Number Theory and its Appli-
  cations" by Kenneth H. Rosen, Fourth Edition, Addison Wesley 1999,
  in Chapter 10.1, pages 356-358.
 
  You will want to use the following functions:

    (rand max seed)
       Returns a natural number from 0 to max-1, inclusive, based on seed.
       Successive calls should therefore use a new seed.
            
    (initial-seed)
       Returns an initial value for the random seed.

    (next-seed seed)
       Produces a new seed for use after a call to rand.
 
  Normally we are not particularly interested in reasoning about ran-
  dom numbers.  However, we can say that the number k generated is an
  an integer, and that 0 <= k < max when max is a positive integer. 
  See the theorems rand/range, initial-seed/seedp, and next-seed/seedp.

|#

(in-package "ACL2")
(set-verify-guards-eagerness 2)

(local (include-book "arithmetic-3/floor-mod/floor-mod" :dir :system))

(defconst *M31* 2147483647)    
(defconst *P1* 16807)
(defun initial-seed () 1382728371)

(defun next-seed (seed)
  (declare (xargs :guard (posp seed)))
  (max (mod (* *P1* seed) *M31*) 1))

(defun rand (max seed)
  (declare (xargs :guard (and (integerp max) (> max 0) (posp seed))))
  (mod seed max))

(defthm initial-seed/posp
  (posp (initial-seed)))

(defthm next-seed/posp
  (implies (posp seed) (posp (next-seed seed))))

(defthm rand/range
  (implies (and (posp s) (integerp n) (< 0 n))
           (and (integerp (rand n s))
                (<= 0 (rand n s))
                (< (rand n s) n))))

(in-theory (disable rand next-seed initial-seed))
           
