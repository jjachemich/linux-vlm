
(defmacro unlock-package (pack)
  `(eval-when (:compile-toplevel)
	      (setf *locked-package-saved-value* (ext:package-lock ,pack)
		    (ext:package-lock ,pack) nill)))
