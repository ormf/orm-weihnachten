;;; This is due to time constrains just a POC. It is very similar to your current
;;; implementation but it has some nice addons but also lacks some features.
;;; 100% not bugfree :)

(ql:quickload :clog)
(ql:quickload :yason)

(defpackage #:orm-weihnachten
  (:use #:cl #:clog)
  (:export start))

(in-package :orm-weihnachten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       reactive lib                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this lib is a reimplementation of a cells like library.

(defparameter ctrack NIL)

; this is the core object
(defstruct (ref-object (:conc-name ro-))
  value
  setter
  fun
  update
  observers
  dependencies)

; constructor
(defun ref (val)
  (make-ref-object :value val :observers '())
)

; this is the setter. It updates the value and calls all listeners nothing new.
(defun setr (ref val)
  (let ((old (ro-value ref)) (setter (ro-setter ref)))
    (if (not (equal val old))
      (progn
       (if setter
         (funcall setter val) ; update val if fun is not defined? Not sure/needed yet
         ;else
         (setf (ro-value ref) val))
       (dolist (observer (ro-observers ref))
               (funcall observer old val)))
      )
    (ro-value ref)
  )
)

; this is a magic getter. It tracks the access and adds the accessor to the listeners. This is very cool.
(defun getr (ref)
  (if ctrack
    (let ((tr ctrack)
          (cb cback))
      (push cb (ro-observers ref))
      (push ref (ro-dependencies tr))
    )
  )
  (ro-value ref)
)

; this is another constructor but instead of a value we can add a function.
; Together with the getter it tracks which object was accessed and adds it to the dependencies.
(defun computed (f &optional (setter NIL))
  (let ((co (make-ref-object :value nil :setter setter :fun f :observers '() :update NIL :dependencies '())))
    (let ((cb #'(lambda (old new) (funcall (ro-update co)))))
      (setf (ro-update co) (lambda ()
                                   ; clear dependencies
                                   (dolist (dep (ro-dependencies co))
                                           (setf (ro-observers dep) (remove cb (ro-observers dep))))
                                   (setf (ro-dependencies co) '())
                                   (let ((old (ro-value co)))
                                     (defparameter ctrack co)
                                     (defparameter cback cb)
                                     ; update dependencies and store new value without using setter
                                     (setf (ro-value co) (funcall (ro-fun co)))
                                     (defparameter ctrack NIL)
                                     (defparameter cback NIL)
                                     ; we have to update observers manual as we did not
                                     (if (not (equal (ro-value co) old))
                                       (dolist (observer (ro-observers co))
                                               (funcall observer old (ro-value co))))
                                     )
                                   co))
      (funcall (ro-update co))
    )
  )
)

; similar to computed but allows to untrack
(defun watch (f)
  (let ((co (make-ref-object :value nil :fun f :observers '() :update NIL :dependencies '())))
    (let ((cb #'(lambda (old new) (funcall (ro-update co)))))
      (setf (ro-update co) (lambda ()
                                   ; clear dependencies
                                   (dolist (dep (ro-dependencies co))
                                           (setf (ro-observers dep) (remove cb (ro-observers dep))))
                                   (setf (ro-dependencies co) '())
                                   (defparameter ctrack co)
                                   (defparameter cback cb)
                                   ; update dependencies and store new value
                                   (setf (ro-value co) (funcall (ro-fun co)))
                                   (defparameter ctrack NIL)
                                   (defparameter cback NIL)
                                   co))
      (funcall (ro-update co))
      ; unwatch
      (lambda ()
              (dolist (dep (ro-dependencies co))
                         (setf (ro-observers dep) (remove cb (ro-observers dep))))
              (setf (ro-dependencies co) '())
              (makunbound 'co)
      )
    )
  )
)

; just a helper function. I heard you like to copy variables. This is how to copy a ref:
(defun copy (ref)
  (computed (lambda () (getr ref))
            (lambda (val) (setr ref val))
  )
)


;;;;;; tutorial

; define a ref object
(setf x (ref 10))
; you can get and set it
(getr x)
(setr x 20)
(getr x)

; we can define new derived ref objects
(setf square (computed (lambda () (* (getr x) (getr x)))))
(getr square)
(setr x 4)

; all the magic is automatically done
(getr square)

(getr x)
; although not recommended we can also define a setter for a computed value
(setf twice (computed (lambda () (* 2 (getr x)))
                      (lambda (val) (setr x (/ val 2)))))
(getr twice)
(setr twice 10)
(getr twice)
(getr x)

(setf debugF (ref NIL))
; watch is just a way to track a bunch of variables:
(setf unwatch (watch (lambda ()
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       clog part                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; little clog extension to allow simple data transport js -> lisp

(defmethod set-on-data ((obj clog-obj) handler)
  (clog::set-event obj "data"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-data-event data))))
             :call-back-script data-event-script))

(defparameter data-event-script
 "+ JSON.stringify(data)")

(defun parse-data-event (data)
   (yason:parse data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; clog extension to integrate reactive with clog

(defvar *bindings* (make-hash-table :test 'equal))

(defstruct (binding (:conc-name b-))
  ref
  attr
  elist
  map
  unwatch)

(defun bind-var-to-attr (name refvar attr &optional (map (lambda (val) val)))
  (if (not (gethash name *bindings*))
    (progn
     (setf (gethash name *bindings*) (make-binding :ref refvar :attr attr :elist '() :map map :unwatch '()))
     (setf (b-unwatch (gethash name *bindings*)) (watch (lambda ()
                                                                (let ((val (getr refvar))
                                                                      (l (b-elist (gethash name *bindings*))))
                                                                  (dolist (obj l)
                                                                          (setf (attribute obj attr) val)
                                                                          ))
                                                                )
                                                  )
      )
     )
  )
  (gethash name *bindings*)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; custom element also defined in js:

(defun create-o-knob (parent binding min max step)
  (let ((var (b-ref binding))
        (attr (b-attr binding))
        (element (create-child parent (format NIL "<o-knob min=\"~a\" max=\"~a\" step=\"~a\" value=\"~a\"></o-knob>" min max step (getr (b-ref binding))))))
    (push element (b-elist binding))
    (set-on-data element
                 (lambda (obj data)
                         (if (gethash "close" data)
                           (progn
                            (setf (b-elist binding) (remove element (b-elist binding))))
                           ; else
                           (progn
                            (setr var (gethash attr data)))
                           )
                         )))
)

; this is just how I would structure such a dynamic website. With a 12 column layout with collection of input elements
(defun create-collection (parent width)
  (create-child parent (format NIL "<div data-width='~a' class='collection'></div>" width))
)

;; Define our CLOG application
(defun new-window (body)
  "On-new-window handler."

  (setf (title (html-document body)) "Frohe Weihnachten")
  (let ((collection (create-collection body "1/2")))
    (create-o-knob collection (bind-var-to-attr "x-value" x "value") -50 50 1)
    (create-o-knob collection (bind-var-to-attr "y-value" y "value") -50 50 0.25)
    (create-o-knob collection (bind-var-to-attr "sum-value" sum "value") -100 100 0.25)
    )
  )


;; I did not want to restart the server everytime I changed the new-window fun thats why I had this proxy.
(defun on-new-window (body)
  (new-window body)
)


(defun start ()
  ;; Initialize the CLOG system with my boot file and my static files (I have no idea if this is the right way).
  (initialize #'on-new-window :static-root "./www/" :boot-file "/start.html")

  ;; Open a browser to http://127.0.0.1:8080 - the default for CLOG apps
  (open-browser))

; we need to define our variables
; (setf x (ref 5.)) is already defined
(setf y (ref 10.))
(setf sum (computed (lambda () (+ (getr y) (getr x)))
                  (lambda (val) (setr x (float (/ val 2))) (setr y (float (/ val 2))))))

; that should start a webserver with three knobs that get change their value by drag and moving in x direction (maybe not the most intuitive way).
(start)
