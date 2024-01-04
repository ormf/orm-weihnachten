;;; 
;;; examples.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :orm-weihnachten)

;;;;;; tutorial

; define a ref object
(defparameter x (ref 10))
; you can get and set it
(getr x)

(setr x 0.5)
(setf x (ref 0.5))

(create-element)

(setf *debug* nil)

(setr x 20)
(getr x)

(progn
  (clear-bindings)
  (setf x (make-ref 0.4))
  (setf x-db
        (make-computed (lambda () (float (min 0 (max -40 (round (rms->db (get-val x))))) 1.0))
                       (lambda (val)  (set-val x (float (max 0 (min 1 (if (<= val -40) 0 (db->rms val)))) 1.0)))))
  nil)

(set-val x 0.2)
(set-val x-db -16)




clog event from #<CLOG-ELEMENT {10075794E3}> -12.979400086720377d0 ;;; calculated event sets two knobs
      watch update: #<CLOG-ELEMENT {10075794A3}> -> 0.2244036908603927d0 ;;; knob 1
      watch update: #<CLOG-ELEMENT {1007579463}> -> 0.2244036908603927d0 ;;; knob 2
1. !! watch update: #<CLOG-ELEMENT {10075794E3}> -> -12.979400086720377d0 ;;; and triggers its own update

      watch update: #<CLOG-ELEMENT {1007579393}> -> -12.979400086720377d0 ;;; trigger update of same element in different window sets its two knobs.
            watch update: #<CLOG-ELEMENT {1007579353}> -> 0.2244036908603927d0  ;;; knob 1 in window 2
            watch update: #<CLOG-ELEMENT {1007579313}> -> 0.2244036908603927d0  ;;; knob 2 in window 2
2.=1. !!    watch update: #<CLOG-ELEMENT {1007579393}> -> -12.979400086720377d0 ;;; triggering its own update
3.    !!    watch update: #<CLOG-ELEMENT {10075794E3}> -> -12.979400086720377d0 ;;; ???

(setf *debug* nil)
(setf *debug* t)

(progn
  (clear-bindings)
  (setf x (ref 0.5))
  (setf x-db
        (computed
         (lambda () ;;; referred val or vals->this
           (progn
             (if *debug* (format t "~&recalc x->dB~%"))
             (min 0 (max -40 (round (rms->db (getr x)))))))
         (lambda (val &optional src) ;;; this->referred val or vals
           (progn
             (if *debug* (format t "~&recalc dB->x: ~a~%" val))
             (setr x (max 0 (min 1 (float (if (<= val -40) 0 (db->rms val))))) src)))))
  nil)

*bindings*

(ro-listeners x)
*debug*
; we can define new derived ref objects
(defparameter square (computed (lambda () (* (getr x) (getr x)))))
(getr square)
(setr x 4)

(defparameter cubic (computed (lambda () (expt (getr x) 3))))

(ro-observers x)

(defparameter plusthree (computed (lambda () (+ 3 (getr x)))))

 ;;; all the magic is automatically done
(getr square)

(clear-dependencies square (ro-update square))

(getr plusthree)

(getr x)

;;; although not recommended we can also define a setter for a computed value
(defparameter twice (computed (lambda () (* 2 (getr x)))
			      (lambda (val) (setr x (/ val 2)))))
(getr twice)
(setr twice 10)
(getr twice)
(getr x)

(defparameter debugF (ref NIL))
; watch is just a way to track a bunch of variables:
(defparameter unwatch (watch (lambda ()
                             (print "Watcher gets called: ")
                             (if (getr debugF)
                               (print (getr x))))))
; it gets called directly and whenever something relevant changes.
; Here is another advantage
; we can change x but watcher does not listen
(setr x 10)

; but if we set debug to true the watcher gets reevaluated and tracks the new dependencies.
(setr debugF t)

; so now x get printed
(setr x 5)

; also here
(setr twice 20)

; this is a cleanup
(funcall unwatch)

; no more updated
(setr x 5)


(ro-observers x)
(ro-dependencies square)

(setr x 20)
(eql x (first (ro-dependencies plusthree)))

(getr square)
(getr plusthree)


(setf *ref-seen* nil)
(
  (setf *debug* t))
