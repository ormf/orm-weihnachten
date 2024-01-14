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

(defparameter *update-deps* nil)
;;; (defparameter *update-deps* t)
(defparameter *curr-ref* nil)
(defparameter *curr-callback* nil)
(defparameter *refs-seen* nil) ;;; keep track of already set refs to
                              ;;; avoid duplicate recalculation and
                              ;;; handle circular dependencies
                              ;;; elegantly.

;;; this is the core object

(defclass ref-object-super ()
  ((id :initform (get-ref-id) :reader ref-id)
   (listeners :initarg :listeners :initform '() :accessor ref-listeners)
   (dependencies :initarg :dependencies :initform '() :accessor ref-dependencies)))

(defclass bang-object (ref-object-super)
  ((value :initarg :value :initform nil :reader ref-value)))

(defclass ref-object (ref-object-super)
  ((value :initarg :value :accessor ref-value)
   (setter :initarg :setter :initform '() :reader ref-setter)
   (fun :initarg :fun  :initform '() :reader ref-fun)
   (update :initarg :update  :initform '() :accessor ref-update)))

(defmethod print-object ((obj ref-object) stream)
  (format stream "#<ref ~a>" (ref-value obj)))

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
  "get the value of ref, updating listeners and dependencies of the
listener's ref if *curr-ref* is set in the dynamic scope of the call
to get-val (see #'make-computed and #'watch)."
  (when *curr-ref*
    (pushnew *curr-callback* (ref-listeners ref))
    (pushnew ref (ref-dependencies *curr-ref*)))
  (ref-value ref))

(defmethod print-object ((obj bang-object) stream)
  (format stream "#<bang>"))

(defun make-bang (&optional fn)
  (make-instance 'bang-object :listeners (if fn (list fn))))

(defun %trigger (obj)
  (map nil #'funcall (ref-listeners obj)))

(defgeneric trigger (obj)
  (:method ((obj bang-object))
    (let ((*refs-seen* nil)) (map nil #'funcall (ref-listeners obj)))))

(defun clear-dependencies (co cb)
  "clear all dependencies of a computed ref object."
  (dolist (dep (ref-dependencies co))
    (setf (ref-listeners dep) (remove cb (ref-listeners dep))))
  (setf (ref-dependencies co) '()))

(defmacro with-updating-deps (&body body)
  "evaluate body in the dynamic context of *update-defs* set to T"
  `(let ((*update-deps* t))
     ,@body))

(defmacro on-deps-update (&rest body)
  "return body if *update-deps* is non-nil, otherwise return nil."
  `(when *update-deps* ,@body))
 
;;; this is another constructor but instead of a value we can add a function.
;;; Together with the getter it tracks which object was accessed and adds it to the dependencies.

(defun make-computed (fn &optional (setter nil))
  "define/create a ref variable using fn to calculate its value with
automagic update whenever any ref value in fn changes."
;;; let* is sequential to avoid nesting of multiple let
  (let* ((new-ref (make-ref nil :fun fn :setter setter))
         (update-callback (lambda (&optional old new) (declare (ignorable old new)) (funcall (ref-update new-ref)))))
    (with-updating-deps
      (setf (ref-update new-ref)
            (lambda () ;;; this update function is closed over new-ref and update-callback
              (on-deps-update (clear-dependencies new-ref update-callback))
              (let ((old (ref-value new-ref))) ;;; memorize last value
                (let ((*curr-ref* (on-deps-update new-ref))
                      (*curr-callback* update-callback))
;;; update dependencies and store new value without using setter.
;;;
;;; the funcall of fn (the first argument of computed) is closed over
;;; dynamic bindings of new-ref and update-callback (being a funcall
;;; of this update fn); if fn executes any get-val functions, their
;;; ref gets pushed into the ref-dependencies of new-ref and the
;;; update-callback gets pushed into the listeners of the get-val
;;; ref. This update-callback will therefore get called whenver the
;;; ref is changed using set-ref.
                  (setf (ref-value new-ref) (funcall fn)))
                (let ((*curr-ref* nil)
                      (*curr-callback* nil))
;;; we have to update listeners manually as we did not use %set-val to
;;; set the ref-value three lines above.
                  (unless (equal (ref-value new-ref) old)
                    (dolist (listener (ref-listeners new-ref))
                      (unless (member listener *refs-seen*)
                        (push listener *refs-seen*)
                        (funcall listener old (ref-value new-ref)))))))
              new-ref))
      ;; call the update function once to register all callbacks.
      (funcall (ref-update new-ref)))))

;;; watch is similar to make-computed. In contrast to make-computed it
;;; mainly implements behaviour. Like computed it uses a ref-object
;;; internally but its main purpose is to trigger behaviour by calling
;;; its supplied f, whenever reference values accessed within that
;;; function change. Watch returns its cleanup function, removing the
;;; ref cell and its listeners.
;;;
;;; Note: This function doesn't return the new ref-object, but an
;;; unwatch function to remove the ref-object and all its
;;; dependencies.

(defun watch (f)
  (let* ((new-ref (make-ref nil :fun f))
         (update-callback (lambda (&optional old new)
                            (declare (ignorable old new))
                            (funcall (ref-update new-ref)))))
    (with-updating-deps
      (setf (ref-update new-ref)
            (lambda ()
              (on-deps-update (clear-dependencies new-ref update-callback))
              ;; the let below establishes a dynamic context for the
              ;; funcall in its body. If *curr-ref* is non-nil, any
              ;; #'get-val access to a ref in ref-fun will push the
              ;; update-callback to the listeners of the ref and the
              ;; ref to the dependencies of the new-ref. In the first
              ;; call to watch, *update-deps* is set to T to ensure
              ;; all dependencies/listeners get registered. Subsequent
              ;; calls to ref-update depend on the value of
              ;; *update-deps* in the dynamic context in which they
              ;; are called to avoid unnecessary duplicate registering
              ;; of the dependencies/listeners on each update
              ;; triggered by value changes in the observed refs.
              (let ((*curr-ref* (on-deps-update new-ref)) 
                    (*curr-callback* update-callback))
                ;; Note: storing the new value doesn't seem to make sense as the
                ;; object's value isn't supposed to be read anywhere. Watch is rather
                ;; used for its side effects only.
                (setf (ref-value new-ref) (funcall (ref-fun new-ref))))
              new-ref))
      ;; call the update function once to register a call to it in all
      ;; ref-objects read in <f>.
      (funcall (ref-update new-ref)))
    (lambda () ;;; return the unwatch/cleanup fn
      (clear-dependencies new-ref update-callback)
      (makunbound 'new-ref))))
#|
(defun remove-watch (ref)
  (when (ref-cleanup ref) (funcall (ref-cleanup ref))))
|#

;;; just a helper function. I heard you like to copy variables. This is how to copy a ref:
(defun copy (ref)
  (make-computed (lambda () (get-val ref))
                 (lambda (val) (%set-val ref val))))

;;; clog extension to integrate reactive with clog.
;;;
;;; A binding establishes a relation between a ref object <refvar> and
;;; an attribute name <attr> using #'bind-ref-to-attr. This function
;;; creates the binding and calls the watch function, which creates a
;;; ref object containing the refvar in its update fn. Adding a clog
;;; element to the b-elist of the binding will result in an update of
;;; the attribue whenever the state of the <refvar> object changes.
;;;
;;; Note: There is no handle to the update function created by watch,
;;; but the watch function returns an unwatch function which removes
;;; the ref object created in #'bind-ref-to-attr and all its
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
   (elist :initarg :elist :initform '() :accessor b-elist)
   (map :initarg :map :accessor b-map)
   (unwatch :initarg :unwatch :initform '() :accessor b-unwatch)))

(defun binding-name (refvar attr)
  (concatenate 'string (ref-id refvar) "-" attr))

(defun make-binding (&rest args)
  (apply #'make-instance 'binding args))

;;; (trigger x-bang)

(defgeneric define-watch (refvar attr new)
  (:method ((refvar ref-object) attr new)
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
  (:method ((refvar bang-object) attr new)
    (watch ;;; watch registers an on-update function
     (lambda ()
       (let ((val (get-val refvar))) ;; we read val only to register
                                      ;; the watch function in the
                                      ;; listeners of the bang
       (declare (ignore val))
       (if *debug* (format t "~&~%elist: ~a~%" (b-elist new)))
       (if *debug* (format t "~&~%seen: ~a~%" (obj-print *refs-seen*)))
       (dolist (obj (b-elist new)) ;;; iterate through all bound html elems
         (unless (member obj *refs-seen*)
           (if *debug* (format t "~&~%watch update: ~a~%-> ~a~%" (obj-print *refs-seen*) obj))
           (push obj *refs-seen*)
           (format t "clicking: ~A" (script-id obj))
           (js-execute obj (format nil "~A.bang()" (script-id obj)))
           )))))))

(defvar *test* (make-bang (lambda () (+ (get-val x) 3))))

(typep *test* 'ref-object-super)

(defgeneric bind-ref-to-attr (refvar attr &optional map)
  (:method ((refvar ref-object-super) attr &optional (map (lambda (val) val)))
    (let ((name (binding-name refvar attr)))
      (or (gethash name *bindings*) ;;; or returns the first non-nil argument and skips evaluating the rest of its args.
          (let ((new (make-binding :ref refvar :attr attr :map map)))
            (setf (b-unwatch new) (define-watch refvar attr new))
            (setf (gethash name *bindings*) new)))))
  (:method ((refvar-array simple-array) attr &optional (map (lambda (val) val)))
    (let ((binding-array (make-array (length refvar-array))))
      (loop for refvar across refvar-array
            for idx from 0
            do (setf (aref binding-array idx)
                     (let ((name (binding-name refvar attr)))
                       (or (gethash name *bindings*) ;;; or returns the first non-nil argument and skips evaluating the rest of its args.
                           (let ((new (make-binding :ref refvar :attr attr :map map)))
                             (setf (b-unwatch new) (define-watch refvar attr new))
                             (setf (gethash name *bindings*) new)))))
            finally (return binding-array))))
  (:documentation "bind a ref (or an array of refs) to an attr of a html element. This
will establish a watch function, which will automatically set the attr
of all registered html elements on state change of the
refvar. Registering html elements is done by pushing the html element
to the b-elist slot of the binding (normally done in the creation
function of the html element)."))

 ;;; (setf (gethash...) ) returns the value which got set (new in this case).

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; creation functions for gui widgets
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; custom html element also defined in js:

(defun create-o-knob (parent binding min max step &key (unit "") (precision 2))
  (let ((var (b-ref binding))
        (attr (b-attr binding))
        (element (create-child
                  parent
                  (format nil "<o-knob min=\"~a\" max=\"~a\" step=\"~a\" value=\"~a\" precision=\"~a\" unit=\"~a\"></o-knob>"
                          min max step (get-val (b-ref binding)) precision unit)))) ;;; the get-val automagically registers the ref
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
		   (declare (ignore obj))
                   (let ((*refs-seen* (list element)))
                     (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
                                         (or (if (gethash "close" data) "close")
                                             (gethash attr data))))
                     (if (gethash "close" data)
                         (setf (b-elist binding) (remove element (b-elist binding))) ;;; cleanup: unregister elem.
                         (%set-val var (gethash attr data)) ;; otherwise set value.
                         ))))))

(defun create-o-numbox (parent binding min max &key (precision 2))
  (let ((var (b-ref binding))
        (attr (b-attr binding))
        (element (create-child
                  parent
                  (format nil "<input is=\"o-numbox\" min=\"~a\" max=\"~a\" value=\"~a\" precision=\"~a\">"
                          min max (get-val (b-ref binding)) precision)))) ;;; the get-val automagically registers the ref
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
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

(defmacro option-main (option)
  `(if (listp ,option)
       (first ,option)
       ,option))

(defmacro option-second (option)
  `(if (listp ,option)
       (or (second ,option) (first ,option))
       ,option))

(defun opt-format-attr (attr val)
  (when val (format nil "~a='~(~a~)'" attr val)))

(defun create-o-bang (parent binding &key label (background '("transparent" "orange")) color flash-time)
  (let ((var (b-ref binding))
        (attr (b-attr binding))
        (element (create-child
                  parent
                  (format nil "<o-bang ~{~@[~a ~]~}>~@[~a~]</o-bang>"
                          (list
                           (opt-format-attr "label-off" (option-main label))
                           (opt-format-attr "label-on" (option-second label))
                           (opt-format-attr "background-off" (option-main background))
                           (opt-format-attr "background-on" (option-second background))
                           (opt-format-attr "color-off" (option-main color))
                           (opt-format-attr "color-on" (option-second color))
                           (opt-format-attr "flash-time" flash-time))
                          (or (option-main label) "")))))
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
                   (let ((*refs-seen* (list element)))
                     (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
                                         (or (if (gethash "close" data) "close")
                                             (gethash attr data))))
                     (cond ((gethash "close" data)
                            (progn
                              (format t "closing bang~%")
                              (setf (b-elist binding) (remove element (b-elist binding))))) ;;; cleanup: unregister elem.
                           ((gethash "bang" data)
                            (let ((*refs-seen* (list obj)))
                              (%trigger var)))))))))

(defun array->attr (arr)
  (format nil "[~{~a~^, ~}]" (coerce arr 'list)))



(defun create-o-toggle (parent binding &key label (background '("transparent" "orange")) color flash-time values)
  (let ((var (b-ref binding))
        (attr (b-attr binding))
        (element (create-child
                  parent
                  (format nil "<o-toggle ~{~@[~a ~]~}>~@[~a~]</o-toggle>"
                          (list
                           (opt-format-attr "value" (or (first values) 0))
                           (opt-format-attr "label-off" (option-main label))
                           (opt-format-attr "label-on" (option-second label))
                           (opt-format-attr "background-off" (option-main background))
                           (opt-format-attr "background-on" (option-second background))
                           (opt-format-attr "color-off" (option-main color))
                           (opt-format-attr "color-on" (option-second color))
                           (opt-format-attr "flash-time" flash-time)
                           (opt-format-attr "value-off" (or (first values) 0))
                           (opt-format-attr "value-on" (or (second values) 1)))
                          (or (option-main label) "")))))
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
                   (declare (ignore obj))
                   (let ((*refs-seen* (list element)))
                     (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
                                         (or (if (gethash "close" data) "close")
                                             (gethash attr data))))
                     (cond ((gethash "close" data)
                            (progn
                              (format t "closing toggle~%")
                              (setf (b-elist binding) (remove element (b-elist binding)))))
                           (t (%set-val var (read-from-string (gethash attr data))))
                           ))))))

(defun create-o-radio (parent binding &key labels label (background '("transparent" "orange")) color flash-time values
                                        (num 8) (direction :right))
  (declare (type (member :up :right :down :left) direction))
  (let ((var (b-ref binding))
        (attr (b-attr binding)) ;;; format nil "~{~a~^,~}"
        (element (create-child
                  parent
                  (format nil "<o-radio ~{~@[~a ~]~}>~@[~a~]</o-radio>"
                          (list
                           (opt-format-attr "value" (or (first values) 0))
                           (opt-format-attr "label-off" (if (option-main labels)
                                                            (format nil "~{~a~^,~}" (option-main labels))))
                           (opt-format-attr "label-on" (if (option-second labels)
                                                           (format nil "~{~a~^,~}" (option-second labels))))
                           (opt-format-attr "background-off" (if (option-main background)
                                                                 (format nil "~{~a~^,~}" (option-main background))))
                           (opt-format-attr "background-on" (if (option-second background)
                                                                (format nil "~{~a~^,~}" (option-second background))))
                           (opt-format-attr "color-off" (if (option-main color)
                                                            (format nil "~{~a~^,~}" (option-main color))))
                           (opt-format-attr "color-on" (if (option-second color)
                                                           (format nil "~{~a~^,~}" (option-second color))))
                           (opt-format-attr "flash-time" flash-time)
                           (opt-format-attr "value-off" (or (first values) 0))
                           (opt-format-attr "value-on" (or (second values) 1))
                           (opt-format-attr "data-num" (or num 8))
                           (opt-format-attr "direction" direction))
                          (or (option-main label) "")))))
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
                   (declare (ignore obj))
                   (let ((*refs-seen* (list element)))
                     (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
                                         (or (if (gethash "close" data) "close")
                                             (gethash attr data))))
                     (cond ((gethash "close" data)
                            (progn
                              (format t "closing radio~%")
                              (setf (b-elist binding) (remove element (b-elist binding)))))
                           (t (%set-val var (gethash attr data)))))))))

(defun create-o-slider (parent binding &key (direction :up) (value 0) (min 0) (max 1)
                                         label background thumb-color bar-color
                                         (mapping :lin) (clip-zero nil))
  (declare (type (member :lin :log) mapping)
           (type (member :up :right :down :left) direction))
  (let ((var (b-ref binding))
        (attr (b-attr binding)) ;;; format nil "~{~a~^,~}"
        (element (create-child
                  parent
                  (format nil "<o-slider ~{~@[~a ~]~}>~@[~a~]</o-slider>"
                          (list
                           (opt-format-attr "direction" direction)
                           (opt-format-attr "value" value)
                           (opt-format-attr "min" min)
                           (opt-format-attr "max" max)
                           (opt-format-attr "label" label)
                           (opt-format-attr "background" (or background "white"))
                           (opt-format-attr "thumb-color" (or thumb-color "black"))
                           (opt-format-attr "bar-color" (or bar-color "transparent") )
                           (opt-format-attr "mapping" mapping )
                           (opt-format-attr "clip-zero" clip-zero ))
                          (or (option-main label) "")))))
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
                   (declare (ignore obj))
                   (let ((*refs-seen* (list element))) ;;; set context for %set-val below
                     (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
                                         (or (if (gethash "close" data) "close")
                                             (gethash attr data))))
                     (cond ((gethash "close" data)
                            (progn
                              (format t "closing slider~%")
                              (setf (b-elist binding) (remove element (b-elist binding)))))
                           (t (%set-val var (gethash attr data)))))))))

;;; min, max, mapping, clip-zero, thumb-color, bar-color

(defun create-o-multislider (parent binding-array
                             &key (direction :up) (value 0) (min 0) (max 1)
                               label background colors
                               (mapping :lin) (clip-zero nil))
  (declare (type (member :lin :log) mapping)
           (type (member :up :right :down :left) direction))
  (let ((attr (b-attr (aref binding-array 0)))
        (element (create-child
                  parent
                  (format nil "<o-multislider ~{~@[~a ~]~}>~@[~a~]</o-multislider>"
                          (list
                           (opt-format-attr "num-sliders" (length binding-array))
                           (opt-format-attr "direction" direction)
                           (opt-format-attr "value" value)
                           (opt-format-attr "min" min)
                           (opt-format-attr "max" max)
                           (opt-format-attr "label" label)
                           (opt-format-attr "background" (or background "white"))
                           (opt-format-attr "colors" (if colors (format nil "~{~a~^,~}" colors)))
                           (opt-format-attr "mapping" mapping )
                           (opt-format-attr "clip-zero" clip-zero ))
                          (or (option-main label) "")))))
    (loop for binding across binding-array
          do (push element (b-elist binding))) ;;; register the
                                               ;;; browser page's html
                                               ;;; elem in all refs
                                               ;;; for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (let ((var-array (coerce
                                   (loop
                                     for binding across binding-array
                                     collect (b-ref binding))
                                   'vector)))
                   (lambda (obj data)
                     (declare (ignore obj))
                   ;;; set context for %set-val below
                     (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
                                         (or (if (gethash "close" data) "close")
                                             (gethash attr data))))
                     (cond ((gethash "close" data)
                            (progn
                              (format t "closing multislider~%")
                              (loop for binding across binding-array do
                                  (setf (b-elist binding) (remove element (b-elist binding))))))
                           (t (let* ((idx (gethash "idx" data))
                                     (value (gethash attr data))
                                     (*refs-seen* (list (aref var-array idx)))) 
                                (%set-val (aref var-array idx) value)))))))))



;; array as attribute: <div id="demo" data-stuff='["some", "string", "here"]'></div>

;; <div id="storageElement" data-storeIt="stuff,more stuff"></div> and use string.split.

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


(defparameter x-bang nil)

(defparameter x nil)
(defparameter x-db nil)
(defparameter radio nil)
(defparameter mslider nil)

(progn
  (clear-bindings)
  (setf x (make-ref 0.5))
  (setf x-bang (make-bang (lambda () (set-val x 0))))
  (setf x-db
        (make-computed
         (lambda () (min 0 (max -40 (round (rms->db (get-val x))))))
         (lambda (val) (%set-val x (max 0 (min 1 (float (if (<= val -40) 0 (db->rms val)))))))))
  (setf radio
        (make-computed
         (lambda () (round (/ (+ 40 (get-val x-db)) 40/7)))
         (lambda (val) (%set-val x-db (- (float (* val 40/7)) 40)))))
  (setf mslider (make-array 8 :initial-contents (loop repeat 8 collect (make-ref 0))))
  (setf (aref mslider 0) x)
  nil)

;;; Define our CLOG application
(defun new-window (body)
  "On-new-window handler."
  (setf (title (html-document body)) "Gui Test")
  (let ((collection (create-collection body "1/2")))
    (create-o-numbox collection (bind-ref-to-attr x "value") 0 1 :precision 2)
    (create-o-knob collection (bind-ref-to-attr x "value") 0 1 0.01)
    (create-o-knob collection (bind-ref-to-attr x "value") 0 1 0.01)
    (create-o-knob collection (bind-ref-to-attr x-db "value") -40 0 1 :unit "dB" :precision 0)
    (create-o-bang collection (bind-ref-to-attr x-bang "bang"))
    (create-o-toggle collection (bind-ref-to-attr x "value"))
    (create-o-radio collection (bind-ref-to-attr radio "value")
                    :direction :up
                    :background '(("#444" "#888") ("orange"))
                    :labels (list (loop for num below 8 collect num)))
    (create-o-slider collection (bind-ref-to-attr x "value") :background "transparent")
    (create-o-multislider collection (bind-ref-to-attr mslider "value") :colors '("#8f8" "#f88" "#44f") :background "transparent")
;;    (create-o-knob collection (bind-ref-to-attr "sum-value" sum "value") -100 100 0.25)
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

;;; that should start a webserver with three knobs that get change their
;;; value by drag and moving in x direction (maybe not the most
;;; intuitive way).
;;; (start)
