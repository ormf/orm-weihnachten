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
(setr x 20)
(getr x)

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
