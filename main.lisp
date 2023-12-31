;;; This is due to time constrains just a POC. It is very similar to your current
;;; implementation but it has some nice addons but also lacks some features.
;;; 100% not bugfree :)

(in-package :orm-weihnachten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       reactive lib                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this lib is a reimplementation of a cells like library.

(defparameter *ctrack* nil)
(defparameter *cback* nil)

;;; this is the core object
(defstruct (ref-object (:conc-name ro-))
  value
  setter
  fun
  update
  listeners
  dependencies)

;;; constructor
(defun ref (val)
  (make-ref-object :value val :listeners '()))

;;; this is the setter. It updates the value and calls all listeners nothing new.
(defun setr (ref val &optional src)
  (let ((old (ro-value ref)) (setter (ro-setter ref)))
    (unless (equal val old)) ;;; unless is the opposite of when; when/unless
			     ;;; don't need progn, because ther is no
			     ;;; else clause.
       (if setter
         (funcall setter val) ; update val if fun is not defined? Not sure/needed yet
         (setf (ro-value ref) val))
       (dolist (listener (ro-listeners ref))
         (unless (eql src listener)
           (funcall listener old val src))))
  (ro-value ref))

;;; this is a magic getter. It tracks the access and adds the accessor
;;; to the listeners. This is very cool.

(defun getr (ref)
  (when *ctrack*
    (pushnew *cback* (ro-listeners ref))
    (pushnew ref (ro-dependencies *ctrack*)))
  (ro-value ref))

;;; this is another constructor but instead of a value we can add a function.
;;; Together with the getter it tracks which object was accessed and adds it to the dependencies.

(defun clear-dependencies (co cb)
  (dolist (dep (ro-dependencies co))
    (setf (ro-listeners dep) (remove cb (ro-listeners dep))))
  (setf (ro-dependencies co) '()))

(defun computed (f &optional (setter nil))
  "define/create a ref variable using f to calculate its value with
automagic update whenever any value in f changes."
;;; let* is sequential to avoid nesting of multiple let
  (let* ((co (make-ref-object :value nil :setter setter :fun f :listeners '() :update nil :dependencies '()))
	 (cb (lambda (&optional old new src) (declare (ignorable old new src)) (funcall (ro-update co) src))))
    (setf (ro-update co)
          (lambda (&optional src) ;;; this update function is closed over co and cb
            (clear-dependencies co cb)
	    (let ((old (ro-value co))) ;;; memorize last value
	      (let ((*ctrack* co)
		    (*cback* cb))
;;; update dependencies and store new value without using setter.
;;;
;;; the funcall of f (the argument of computed) is closed over dynamic
;;; bindings of co and cb (being a funcall of this update fn); if f
;;; executes any getr functions, their ref gets pushed into the
;;; ro-dependencies of co and this ro-update function gets pushed into
;;; the listeners of ref.
		(setf (ro-value co) (funcall f)))
	      (let ((*ctrack* nil)
		    (*cback* nil))
;;; we have to update listeners manually as we did not use setr to set the ro-value three lines above.
		(unless (equal (ro-value co) old)
		  (dolist (listener (ro-listeners co))
		    (unless (eql listener src)
                      (funcall listener old (ro-value co) src))))))
	    co))
    (funcall (ro-update co))))

;;; similar to computed but allows to untrack/unwatch
(defun watch (f)
  (let* ((co (make-ref-object :value nil :fun f :listeners '() :update nil :dependencies '()))
	 (cb (lambda (&optional old new src)
	       (declare (ignorable old new))
	       (funcall (ro-update co) src))))
    (setf (ro-update co)
          (lambda (&optional src)
	    (clear-dependencies co cb)
	    (let ((*ctrack* co)
		  (*cback* cb))
;;; update dependencies and store new value
	      (setf (ro-value co) (funcall (ro-fun co) src)))
	    co))
    (funcall (ro-update co))
;;; unwatch
    (lambda ()
      (clear-dependencies co cb)
      (makunbound 'co))))

;;; just a helper function. I heard you like to copy variables. This is how to copy a ref:
(defun copy (ref)
  (computed (lambda () (getr ref))
            (lambda (val) (setr ref val))))

; clog extension to integrate reactive with clog

(defvar *bindings* (make-hash-table :test 'equal))

(defun clear-bindings ()
  (setf *bindings* (make-hash-table :test 'equal)))

(defstruct (binding (:conc-name b-))
  ref
  attr
  elist
  map
  unwatch)

(defun bind-var-to-attr (name refvar attr &optional (map (lambda (val) val)))
  "bind a ref to an attr of a html element. This"
  (or (gethash name *bindings*) ;;; or returns the first non-nil argument and skips evaluating the rest of its args.
      (let ((new (make-binding :ref refvar :attr attr :elist '() :map map :unwatch '())))
        (setf (b-unwatch new)
              (watch ;;; watch registers an on-update function
               (lambda (src)
                 (let ((val (getr refvar)))
                   (dolist (obj (b-elist new)) ;;; iterate through all bound html elems
                     (if *debug* (format t "~&watch update: ~a -> ~a ~a~%" src obj val))
                     (setf (attribute obj attr) val))))))
        (setf (gethash name *bindings*) new)))) ;;; (setf (gethash...) ) returns the value which got set (new in this case).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       clog part                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; little clog extension to allow simple data transport js -> lisp

(defparameter *data-event-script*
  "+ JSON.stringify(data)")

(defun parse-data-event (data)
  (yason:parse data))

(defmethod set-on-data ((obj clog-obj) handler)
   (clog::set-event
    obj "data"
    (when handler
      (lambda (data)
        (funcall handler obj (parse-data-event data))))
    :call-back-script *data-event-script*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; custom element also defined in js:

(defun create-o-knob (parent binding min max step &key (unit ""))
  (let ((var (b-ref binding))
        (attr (b-attr binding))
        (element (create-child
                  parent
                  (format nil "<o-knob min=\"~a\" max=\"~a\" step=\"~a\" value=\"~a\" unit=\"~a\"></o-knob>"
                          min max step (getr (b-ref binding)) unit)))) ;;; the getr automagically registers the ref
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
		   (declare (ignore obj))
                   (if *debug* (format t "~&clog event from ~a ~a~%" element (or (if (gethash "close" data) "close")
                                                                                 (gethash attr data))))
                   (if (gethash "close" data)
                       (setf (b-elist binding) (remove element (b-elist binding))) ;;; cleanup: unregister elem.
                       (setr var (gethash attr data)))))))

(defparameter *test* nil)

; this is just how I would structure such a dynamic website. With a 12 column layout with collection of input elements
(defun create-collection (parent width)
  (create-child parent (format nil "<div data-width='~a' class='collection'></div>" width)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :orm-weihnachten)

;;; we need to define our variables
(defparameter x (ref 5.))
(defparameter y (ref 10.))
(defparameter sum (computed (lambda () (+ (getr y) (getr x)))
                  (lambda (val) (setr x (float (/ val 2))) (setr y (float (/ val 2))))))

(defun rms->db (amp)
  (if (zerop amp)
      -100
      (* 20 (log amp 10))))

(defun db->rms (db)
  (expt 10 (/ db 20)))

(defparameter *debug* nil)

(defparameter x-db
  (computed
   (lambda () ;;; referred val or vals->this
     (progn
       (if *debug* (format t "~&recalc x->dB~%"))
       (min 0 (max -40 (rms->db (getr x))))))
   (lambda (val) ;;; this->referred val or vals
     (progn
       (if *debug* (format t "~&recalc dB->x: ~a~%" val))
       (setr x (max 0 (min 1 (db->rms val))))))))

;;; set the path for the current working directory
(uiop:chdir (asdf:system-relative-pathname :orm-weihnachten ""))

;;; Define our CLOG application
(defun new-window (body)
  "On-new-window handler."
  (setf (title (html-document body)) "Frohe Weihnachten")
  (let ((collection (create-collection body "1/2")))
    (clear-bindings) ;;; start from scratch
    (create-o-knob collection (bind-var-to-attr "x-value" x "value") 0 1 0.01)
    (create-o-knob collection (bind-var-to-attr "x-value" x "value") 0 1 0.01)
    (create-o-knob collection (bind-var-to-attr "x-dB" x-db "value") -40 0 1 :unit "dB")
;;    (create-o-knob collection (bind-var-to-attr "sum-value" sum "value") -100 100 0.25)
    ))

;;; I did not want to restart the server everytime I changed the new-window fun thats why I had this proxy.
(defun on-new-window (body)
  (new-window body))


(defun start ()
  ;; Initialize the CLOG system with my boot file and my static files (I have no idea if this is the right way).
  (initialize #'on-new-window
              :static-root (merge-pathnames "www/" (asdf:system-source-directory :orm-weihnachten))
              :boot-file "/start.html")
  ;; Open a browser to http://127.0.0.1:8080 - the default for CLOG apps
  (open-browser))

; that should start a webserver with three knobs that get change their value by drag and moving in x direction (maybe not the most intuitive way).
;;; (start)
