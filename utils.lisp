(in-package :granolin )

(defun string->json-key (s)
  (format nil ":|~a|" s))
