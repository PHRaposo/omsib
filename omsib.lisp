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
						 (om::om-relative-path '("sources") "package")						 
			             (om::om-relative-path '("sources") "sib-instruments")
                         (om::om-relative-path '("sources") "sib-lines")
                         (om::om-relative-path '("sources") "omsib-gen")						 							 					 		 					 							 
                          ))
						 
;--------------------------------------------------
;Loading files 
;--------------------------------------------------
(mapc #'compile&load *omsib-files*)

;--------------------------------------------------
;Fill library 
;--------------------------------------------------


(fill-library '( ("Export" Nil Nil (omsib::om->sib) Nil)
 		         ("Doc" Nil Nil (omsib::show-sib-instruments omsib::search-sib-instrument omsib::show-sib-articulations omsib::show-sib-lines) Nil)
				 ;("Extras" Nil Nil (omsib::add-extra-text omsib::add-extra-vel) Nil)
                 ))
 				
(print (format nil "
OMSIB LIBRARY
by Paulo Raposo"
))
