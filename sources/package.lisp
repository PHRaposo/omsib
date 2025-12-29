(IN-PACKAGE :CL-USER)

(let ((om-version (read-from-string (subseq om::*version-str* 0 3))))
 (print (concatenate 'string "Openmusic version: " om::*version-str*))
 (unless (>= om-version 7.7)
  (om::om-message-dialog "ERROR: OMSIB library requires Openmusic 7.7 or higher.")
  (om::om-abort)))
  
(defpackage :om-sibelius
 (:nicknames :omsib)
 (:use :cl))

(in-package :om-sibelius)
