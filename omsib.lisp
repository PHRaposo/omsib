;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OMSIB LIBRARY
;;; 2024 - by Paulo Raposo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(in-package :om)

;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *omsib-files* nil)
(setf  *omsib-files* (list
   		      (om-relative-path '("sources") "package")						 
	 	      (om-relative-path '("sources") "sib-instruments")
                      (om-relative-path '("sources") "sib-lines")
                      (om-relative-path '("sources") "omsib-preferences")
                      (om-relative-path '("sources") "parse-number")						 
                      (om-relative-path '("sources") "omsib-gen")		 							 
                          ))
						 
;--------------------------------------------------
;Loading files 
;--------------------------------------------------
(mapc #'compile&load *omsib-files*)

;--------------------------------------------------
;Fill library 
;--------------------------------------------------


(fill-library '( ("Export" Nil Nil (omsib::om->sib) Nil)
 		 ("Doc" Nil Nil (omsib::show-sib-instruments omsib::search-sib-instrument omsib::show-sib-articulations omsib::show-sib-lines omsib::show-sib-technique-texts) Nil)
	         ("Extras" Nil Nil (omsib::add-sib-articulations omsib::add-extra-vel omsib::mk-line omsib::split-voice-by-clef) Nil)
                 ))
 				
(print (format nil "
OMSIB LIBRARY
by Paulo Raposo"
))
