(in-package :granolin)

(defmacro let-cond (&body forms)
  (let ((tmp-var (gensym)))
    `(let (,tmp-var)
       (cond
         ,@(loop :for (var test . body) :in forms
                 :if (eq var t)
                   :collect (list* t (cons test body))
                 :else
                   :collect `((setf ,tmp-var ,test)
                              (let ((,var ,tmp-var))
                                ,@body)))))))

(defmacro let-when ((var test) &body body)
  `(let ((,var ,test))
     (when ,test ,@body)))

(defmacro let-if ((var test) then &optional else)
  `(let ((,var ,test))
     (if ,test ,then ,else)))

(defmacro getob (ob key &rest keys)
  "OB should be a nested PLIST, KEYS are lists of keys into that PLIST. Gets the
  result of nested GETF calls into the list. This form is SETF-able."
  (let ((form `(getf ,ob ,key)))
    (dolist (k keys)
      (setf form `(getf ,form ,k)))
    form))


(defmacro def-json-wrap (name &rest field-specs)
  "Defines a struct named the value of NAME, a symbol, with a single slot called
   DATA. DATA holds a PLIST as returned by JONATHAN:PARSE.

   Each FIELD-SPEC is a list of the form (METHOD-NAME KEY1 ... KEYN)

   For each FIELD-SPEC, a method called METHOD-NAME will be defined as a reader
   that accesses a value, the path to which is formed of the KEY values.

   E.g. If a JSON value `ob` has a descendent at `ob.x.y.z` then the FIELD-SPEC
   could be (get-z :|x| :|y| :|z|)
  "
  `(progn
     (defstruct ,name data)
     ,@(loop :for (method . keys) :in field-specs :collect
             `(defmethod ,method ((ob ,name))
                (with-slots (data) ob
                  (getob data ,@keys))))))

(defmacro def-timeline-event-pred (name etype mtype)
  `(defun ,name (event)
     (and (equal ,etype (getob event :|type|))
          (equal ,mtype (getob event :|content| :|msgtype|)))))
