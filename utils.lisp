(in-package :granolin )

(defun string->json-key (s)
  (read-from-string (format nil ":|~a|" s)))

(defun readable-username (s)
  (let ((stop-index (search ":" s)))
    (if (and stop-index
             (equal #\@ (aref s 0)))
        (subseq s 1 stop-index)
        s)))
