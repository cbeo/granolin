(in-package :granolin )

(defun string->json-key (s)
  (read-from-string (format nil ":|~a|" s)))

(defun readable-username (s)
  (let ((stop-index (search ":" s)))
    (if (and stop-index
             (equal #\@ (aref s 0)))
        (subseq s 1 stop-index)
        s)))

(defun probably-plistp (ls)
  (and (consp ls) (keywordp (car ls))))

(defun update-json-plist (old new)
  "Recursively update the plist OLD with the plist NEW. Destructively alters
OLD, and returns it. Never removes any values, but can overwrite values whose
keys are present in NEW "
  (loop :for (key val . more) :on new :by #'cddr :do
    (cond
      ;; if the value is a plist, recurse
      ((probably-plistp val)
       (setf (getf old key)
             (update-json-plist (getf old key) val)))
      ;; otherwise the val is non "object", and so can just be updated if
      ;; different
      ((not (equal val (getf old key)))
       (setf (getf old key) val))))
  ;; return the updated old value
  old)
