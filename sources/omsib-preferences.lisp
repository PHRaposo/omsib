;========================================================================================================================
;         PREFERENCES PANEL MODULE
;=========================================================================================================================
(in-package :om)

(defvar *sib-dyn-on* nil)
(defvar *sib-dyn-on* t)

(defvar *sib-tit-on* nil)
(defvar *sib-tit-on* t)

(defvar *sib-comp-on* nil)
(defvar *sib-comp-on* t)

(defmethod get-def-vals ((iconID (eql :omsib)))
   (list 
    :sib-dyn-on t
    :sib-tit-on t	
    :sib-comp-on t
    ))

(defmethod put-preferences ((iconID (eql :omsib)))

  (let* ((modulepref (find-pref-module iconID)))
    (setf *sib-dyn-on* (get-pref modulepref :sib-dyn-on))
    (setf *sib-tit-on* (get-pref modulepref :sib-tit-on))	
    (setf *sib-comp-on* (get-pref modulepref :sib-comp-on))	
    ))
	
(defmethod save-pref-module ((iconID (eql :omsib)) item)
   (list iconID `(list 
                  :sib-dyn-on ,*sib-dyn-on*
                  :sib-tit-on ,*sib-tit-on*
                  :sib-comp-on ,*sib-comp-on*				  				  
                  ) *om-version*))



(defmethod make-new-pref-scroll  ((num (eql :omsib)) modulepref)
  (let ((thescroll (om-make-view 'preference-pane
                                 :pref-id num
                                 :name "omsib"
                                 :size (get-pref-scroll-size)
                                 :position (om-make-point 0 0)
                                 :font *controls-font* 
                                ;:scrollbars :v 
                                ;:retain-scrollbars t
                                 :bg-color *om-light-gray-color*
                                 ))
        (l1 50)
	;(l2 (round (om-point-h (get-pref-scroll-size)) 2)) <== not used in omsib
	;(l3 (- (om-point-h (get-pref-scroll-size)) 60)) <== not used in omsib
        (i 40)
        (posy 0)
	;(dy 40) <== not used in omsib
    ;outtxt tmptxt  <== not used in omsib
		)
    
    (om-add-subviews thescroll 
         			  (om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy 5)) (om-make-point 200 30) "Export:"
                              :font *om-default-font4b*)

                      (om-make-dialog-item 'om-static-text (om-make-point 50 i) (om-make-point 120 20) "Title"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-check-box (om-make-point 165 i) (om-make-point 30 20) ""
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :sib-tit-on)
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :sib-tit-on (om-checked-p item))))

                   (om-make-dialog-item 'om-static-text (om-make-point 50 (incf i 40)) (om-make-point 120 20) "Composer"
                                       :font *controls-font*)

                  (om-make-dialog-item 'om-check-box (om-make-point 165 i) (om-make-point 30 20) ""
                                       :font *controls-font*
                                       :checked-p (get-pref modulepref :sib-comp-on)
                                       :di-action (om-dialog-item-act item 
                                                    (set-pref modulepref :sib-comp-on (om-checked-p item))))
													
                  (om-make-dialog-item 'om-static-text (om-make-point 50 (incf i 40)) (om-make-point 120 20) "Dynamics"
                                    :font *controls-font*)

                 (om-make-dialog-item 'om-check-box (om-make-point 165 i) (om-make-point 30 20) ""
                                    :font *controls-font*
                                    :checked-p (get-pref modulepref :sib-dyn-on)
                                    :di-action (om-dialog-item-act item 
                      (set-pref modulepref :sib-dyn-on (om-checked-p item))))																									   
                     )

    (setf posy 0)
          
    thescroll))
	
;set and load tab in om preferences panel 
(pushr :omsib *pref-order*)

(defun add-sib-preferences ()
(push-pref-module (list :omsib (get-def-vals :omsib))))

(add-sib-preferences)