;;; This is due to time constrains just a POC. It is very similar to your current
;;; implementation but it has some nice addons but also lacks some features.
;;; 100% not bugfree :)

(in-package :orm-weihnachten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       reactive lib                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this lib is a reimplementation of a cells like library.

(defparameter *ref-id* '(0))

(defun next-id (ref-id)
  "Generate unique ids for use in scripts."
  (atomics:atomic-incf (car ref-id)))

(defun get-ref-id ()
  (format nil "ref~a" (next-id *ref-id*)))

(defparameter *curr-ref* nil)
(defparameter *curr-callback* nil)
(defparameter *refs-seen* nil) ;;; keep track of already set refs to
                              ;;; avoid duplicate recalculation and
                              ;;; handle circular dependencies
                              ;;; elegantly.

;;; this is the core object

(defclass ref-object ()
  ((id :initform (get-ref-id) :reader ref-id)
   (value :initarg :value :accessor ref-value)
   (setter :initarg :setter :initform '() :reader ref-setter)
   (fun :initarg :fun  :initform '() :reader ref-fun)
   (update :initarg :update  :initform '() :accessor ref-update)
   (listeners :initarg :listeners :initform '() :accessor ref-listeners)
   (dependencies :initarg :dependencies :initform '() :accessor ref-dependencies)))

;;; constructor
(defun make-ref (val &rest args)
  (apply #'make-instance 'ref-object :value val args))

;;; this is the setter. It updates the value and calls all listeners.
(defun %set-val (ref val)
  "don't use this directly in the top-level, rather use set-val.
%set-val sets the ref to val and triggers the computation of related
objects in the context of the dynamic variable *refs-seen*, which
tracks the references already updated to avoid unnecessary
recalcuations and problems with circular dependencies."
  (let ((old (ref-value ref)) (setter (ref-setter ref)))
    (unless (equal val old) ;;; unless is the opposite of when; when/unless
;;; don't need progn, because ther is no
;;; else clause.
      (if *debug* (format t "~&%set-val called: ~a~%" (obj-print *refs-seen*)))
      (if setter
          (progn
            (push setter *refs-seen*)
            (funcall setter val)) ; update val if fun is not defined? Not sure/needed yet
          (setf (ref-value ref) val))
      (dolist (listener (ref-listeners ref))
        (unless (member listener *refs-seen*)
          (push listener *refs-seen*)
          (funcall listener old val)))))
  (ref-value ref))

(defun set-val (ref value)
  "set the value of ref in the context of a freshly cleared *refs-seen*."
  (let ((*refs-seen* nil))
    (%set-val ref value))
  (setf *refs-seen* nil) ;;; just to make sure that the global variable is cleared (not really necessary).
  value)

;;; this is a magic getter. It tracks the access and adds the accessor
;;; to the listeners. This is very cool.

(defun get-val (ref)
  (when *curr-ref*
    (pushnew *curr-callback* (ref-listeners ref))
    (pushnew ref (ref-dependencies *curr-ref*)))
  (ref-value ref))


(defun clear-dependencies (co cb)
  "clear all dependencies of a computed ref object."
  (dolist (dep (ref-dependencies co))
    (setf (ref-listeners dep) (remove cb (ref-listeners dep))))
  (setf (ref-dependencies co) '()))

;;; this is another constructor but instead of a value we can add a function.
;;; Together with the getter it tracks which object was accessed and adds it to the dependencies.

(defun make-computed (f &optional (setter nil))
  "define/create a ref variable using f to calculate its value with
automagic update whenever any value in f changes."
;;; let* is sequential to avoid nesting of multiple let
  (let* ((new-ref (make-ref nil :fun f :setter setter))
	 (update-callback (lambda (&optional old new) (declare (ignorable old new)) (funcall (ref-update new-ref)))))
    (setf (ref-update new-ref)
          (lambda () ;;; this update function is closed over new-ref and update-callback
            (clear-dependencies new-ref update-callback)
	    (let ((old (ref-value new-ref))) ;;; memorize last value
	      (let ((*curr-ref* new-ref)
		    (*curr-callback* update-callback))
;;; update dependencies and store new value without using setter.
;;;
;;; the funcall of f (the argument of computed) is closed over dynamic
;;; bindings of new-ref and update-callback (being a funcall of this update fn); if f
;;; executes any get-val functions, their ref gets pushed into the
;;; ref-dependencies of new-ref and this ref-update function gets pushed into
;;; the listeners of ref.
		(setf (ref-value new-ref) (funcall f)))
	      (let ((*curr-ref* nil)
		    (*curr-callback* nil))
;;; we have to update listeners manually as we did not use %set-val to set the ref-value three lines above.
		(unless (equal (ref-value new-ref) old)
		  (dolist (listener (ref-listeners new-ref))
		    (unless (member listener *refs-seen*)
                      (push listener *refs-seen*)
                      (funcall listener old (ref-value new-ref)))))))
	    new-ref))
    (funcall (ref-update new-ref))))

;;; watch is similar to make-computed. In contrast to make-computed it
;;; mainly implements behaviour. Like computed it uses a ref-object
;;; internally but its main purpose is to trigger behaviour by calling
;;; its supplied f, whenever reference values accessed within that
;;; function change.
;;;
;;; Note: This function doesn't return the new ref-object, but an
;;; unwatch function to remove the ref-object and all its
;;; dependencies.


(defun watch (f)
  (let* ((new-ref (make-ref nil :fun f))
	 (update-callback (lambda (&optional old new)
	       (declare (ignorable old new))
	       (funcall (ref-update new-ref)))))
    (setf (ref-update new-ref)
          (lambda ()
	    (clear-dependencies new-ref update-callback)
	    (let ((*curr-ref* new-ref)
		  (*curr-callback* update-callback)) ;;; establish a dynamic context for the funcall below
;;; update dependencies and store new value
;;;
;;; Note: storing the new value doesn't seem to make sense as the
;;; object's value isn't supposed to be read anywhere. Watch is rather
;;; used for its side effects only.
	      (setf (ref-value new-ref) (funcall (ref-fun new-ref))))
	    new-ref))
    (funcall (ref-update new-ref)) ;;; call the update function once to
                              ;;; register a call to it in all
                              ;;; ref-objects read in <f>.

    (lambda () ;;; unwatch
      (clear-dependencies new-ref update-callback)
      (makunbound 'new-ref))))

;;; just a helper function. I heard you like to copy variables. This is how to copy a ref:
(defun copy (ref)
  (make-computed (lambda () (get-val ref))
            (lambda (val) (%set-val ref val))))

;;; clog extension to integrate reactive with clog.
;;;
;;; A binding establishes a relation between a ref object <refvar> and
;;; an attribute name <attr> using #'bind-var-to-attr. This function
;;; creates the binding and calls the watch function, which creates a
;;; ref object containing the refvar in its update fn. Adding a clog
;;; element to the b-elist of the binding will result in an update of
;;; the attribue whenever the state of the <refvar> object changes.
;;;
;;; Note: There is no handle to the update function created by watch,
;;; but the watch function returns an unwatch function which removes
;;; the ref object created in #'bind-var-to-attr and all its
;;; dependencies. This function is stored in the unwatch slot of the
;;; binding to facilitate unbinding.

; On initialization of a clog gui element, a
; the element is pushed to the elist of the binding.

(defvar *bindings* (make-hash-table :test 'equal))

(defun clear-bindings ()
  (maphash (lambda (name binding) ;;; remove watch functions
             (declare (ignore name))
             (funcall (b-unwatch binding)))
           *bindings*)
  (setf *bindings* (make-hash-table :test 'equal)))

(defclass binding ()
  ((ref :initarg :ref :accessor b-ref)
   (attr :initarg :attr :accessor b-attr)
   (elist :initarg :elist :accessor b-elist)
   (map :initarg :map :accessor b-map)
   (unwatch :initarg :unwatch :accessor b-unwatch)))

(defun binding-name (refvar attr)
  (concatenate 'string (ref-id refvar) "-" attr))

(defun make-binding (&rest args)
  (apply #'make-instance 'binding args))

(defun bind-var-to-attr (refvar attr &optional (map (lambda (val) val)))
  "bind a ref to an attr of a html element. This will establish a watch
function, which will automatically set the attr of all registered html
elements on state change of the refvar. Registering html elements is
done by pushing the html element to the b-elist slot of the
binding (normally done in the creation function of the html element)."
  (let ((name (binding-name refvar attr)))
    (or (gethash name *bindings*) ;;; or returns the first non-nil argument and skips evaluating the rest of its args.
        (let ((new (make-binding :ref refvar :attr attr :elist '() :map map :unwatch '())))
          (setf (b-unwatch new)
                (watch ;;; watch registers an on-update function
                 (lambda ()
                   (let ((val (get-val refvar)))
                     (if *debug* (format t "~&~%elist: ~a~%" (b-elist new)))
                     (if *debug* (format t "~&~%seen: ~a~%" (obj-print *refs-seen*)))
                     (dolist (obj (b-elist new)) ;;; iterate through all bound html elems
                       (unless (member obj *refs-seen*)
                         (if *debug* (format t "~&~%watch update: ~a~%-> ~a ~a~%" (obj-print *refs-seen*) obj val))
                         (push obj *refs-seen*)
                         (setf (attribute obj attr) val)))))))
          (setf (gethash name *bindings*) new))))) ;;; (setf (gethash...) ) returns the value which got set (new in this case).

(defun obj-print (seq)
  (format nil "(~{~a~^ ~})"
          (mapcar (lambda (x)
                    (cond
                      ((functionp x) (format nil "<function {~a}>" (sb-kernel:get-lisp-obj-address x)))
                      (:else (format nil "~a" x))))
                  seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       clog part                                          ;;;
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
        (unless (string= data "undefined")
          (funcall handler obj (parse-data-event data)))))
    :call-back-script *data-event-script*))

(defmethod set-on-data2 ((obj clog-obj) handler)
   (clog::set-event
    obj "data"
    (when handler
      (lambda (data)
        (format t "data: ~S~%" data)
        (unless (string= data "undefined")
          (funcall handler obj (parse-data-event data)))))
    :call-back-script "+ JSON.stringify(data)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; creation functions for gui widgets
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; custom html element also defined in js:

(defun create-o-knob (parent binding min max step &key (unit ""))
  (let ((var (b-ref binding))
        (attr (b-attr binding))
        (element (create-child
                  parent
                  (format nil "<o-knob min=\"~a\" max=\"~a\" step=\"~a\" value=\"~a\" unit=\"~a\"></o-knob>"
                          min max step (get-val (b-ref binding)) unit)))) ;;; the get-val automagically registers the ref
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
		   (declare (ignore obj))
                   (let ((*refs-seen* (list element)))
                     (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
                                         (or (if (gethash "close" data) "close")
                                             (gethash attr data))))
                     (if (gethash "close" data) ;;; cleanup: unregister elem.
                         (setf (b-elist binding) (remove element (b-elist binding)))
                         (%set-val var (gethash attr data))))))))

(defun create-o-numbox (parent binding min max)
  (let ((var (b-ref binding))
        (attr (b-attr binding))
        (element (create-child
                  parent
                  (format nil "<input is=\"o-numbox\" min=\"~a\" max=\"~a\" value=\"~a\">"
                          min max (get-val (b-ref binding)))))) ;;; the get-val automagically registers the ref
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (set-on-data2 element ;;; react to changes in the browser page
                 (lambda (obj data)
		   (declare (ignore obj))
                   (let ((*refs-seen* (list element)))
                     (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
                                         (or (if (gethash "close" data) "close")
                                             (gethash attr data))))
                     (if (gethash "close" data)
                         (progn
                           (format t "closing numbox~%")
                           (setf (b-elist binding) (remove element (b-elist binding)))) ;;; cleanup: unregister elem.
                         (%set-val var (gethash attr data))))))))

;;; this is just how I would structure such a dynamic website. With a 12 column layout with collection of input elements

(defun create-collection (parent width)
  (create-child parent (format nil "<div data-width='~a' class='collection'></div>" width)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :orm-weihnachten)


(defun rms->db (amp)
  (if (zerop amp)
      -100
      (* 20 (log amp 10))))

(defun db->rms (db)
  (expt 10 (/ db 20)))

;;; we need to define our variables

(defparameter *debug* nil)

(defparameter x (make-ref 5.))
(defparameter x-db
  (make-computed
   (lambda () ;;; referred val or vals->this
     (progn
       (if *debug* (format t "~&recalc x->dB~%"))
       (min 0 (max -40 (round (rms->db (get-val x)))))))
   (lambda (val) ;;; this->referred val or vals
     (progn
       (if *debug* (format t "~&recalc dB->x: ~a~%" val))
       (%set-val x (max 0 (min 1 (float (if (<= val -40) 0 (db->rms val))))))))))

(progn
  (clear-bindings)
  (setf x (make-ref 0.5))
  (setf x-db
        (make-computed
         (lambda () ;;; referred val or vals->this
           (progn
             (if *debug* (format t "~&recalc x->dB~%"))
             (min 0 (max -40 (round (rms->db (get-val x)))))))
         (lambda (val) ;;; this->referred val or vals
           (progn
             (if *debug* (format t "~&recalc dB->x: ~a~%" val))
             (%set-val x (max 0 (min 1 (float (if (<= val -40) 0 (db->rms val))))))))))
  nil)


;;; set the path for the current working directory
(uiop:chdir (asdf:system-relative-pathname :orm-weihnachten ""))

;;; Define our CLOG application
(defun new-window (body)
  "On-new-window handler."
  (setf (title (html-document body)) "Frohe Weihnachten")
  (let ((collection (create-collection body "1/2")))
    (create-o-numbox collection (bind-var-to-attr x "value") 0 1)
    (create-o-knob collection (bind-var-to-attr x "value") 0 1 0.01)
    (create-o-knob collection (bind-var-to-attr x "value") 0 1 0.01)
    (create-o-knob collection (bind-var-to-attr x-db "value") -40 0 1 :unit "dB")
;;    (create-o-knob collection (bind-var-to-attr "sum-value" sum "value") -100 100 0.25)
    ))

;;; I did not want to restart the server everytime I changed the new-window fun thats why I had this proxy.
(defun on-new-window (body)
  (new-window body))


(defun start ()
  ;; Initialize the CLOG system with my boot file and my static files (I have no idea if this is the right way).
  (clear-bindings) ;;; start from scratch
  (initialize #'on-new-window
              :static-root (merge-pathnames "www/" (asdf:system-source-directory :orm-weihnachten))
              :boot-file "/start.html")
  ;; Open a browser to http://127.0.0.1:8080 - the default for CLOG apps
  (open-browser))

; that should start a webserver with three knobs that get change their value by drag and moving in x direction (maybe not the most intuitive way).
;;; (start)
