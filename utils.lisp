(in-package :granolin )

(defun string->json-key (s)
  (read-from-string (format nil ":|~a|" s)))
