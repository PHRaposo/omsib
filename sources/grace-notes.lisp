(in-package :om)

;***************
;GRACE-NOTE-EXTRA
;***************

(defclass! grace-note-extra (extra-objet)
   ((thehead :initform (head-1/4) :initarg :thehead :accessor thehead)
    (extra-grace-notes :initform '(6000) :initarg :extra-grace-notes :accessor extra-grace-notes))
   (:icon 303))

(defmethod grace-extra-p ((self grace-note-extra)) t)
(defmethod grace-extra-p ((self t)) nil)


(defmethod draw-obj-in-rect ((self  grace-note-extra) x x1 y y1 edparams  view)
  (let* ((fontsize 24)
         (thefont (om-make-music-font *extras-font* fontsize))
         (sizetext (round (get-name-size (thehead self) thefont) 2)))
    (om-with-font thefont
                  (om-draw-string (round (- (+ x (/ (- x1 x) 2)) sizetext)) (round (+ y (/ (- y1 y) 2))) (thehead self))
                  )))

(defmethod add-new-extra-drag (self where obj (mode (eql 'accent)) dc)
  (when obj
  (let ((newextra (make-instance 'grace-note-extra :object obj)))
     (setf (thehead newextra) (string (car (get-extra-param *extramanager* (edit-mode *extramanager*)))))
     (push newextra (extra-obj-list obj))
     (update-panel self t))))

(defclass grap-extra-grace (grap-extra-objet) ())

(defmethod make-graph-extra-obj ((self grace-note-extra) gobj) 
  (let ((rep (make-instance 'grap-extra-grace
                            :reference  self
                            :gobject gobj)))
    (setf (graphic-frame self) rep)))
   
(defmethod draw-graph-extra-obj ((self grap-extra-grace) view size staff)
  (let* ((grap-obj (gobject self))
         (object (reference self))
         (rect (rectangle grap-obj))
         (fontsize size)
         (text (thehead (reference self)))
         (thefont (om-make-music-font *extras-font* fontsize))
         (ls (round size 4))
         (sizetext (get-name-size text thefont))
         points x y)
    (setf points (convert-delta-to-points grap-obj 
                                          (list (om-make-point (deltax (reference self)) (deltay (reference self)))) 
                                          size))
    (setf y (om-point-v (car points)))
    (setf x (om-point-h (car points)))
    (om-with-font thefont
                  (om-draw-string (om-point-h (car points)) (om-point-v (car points)) text))
    (setf (rectangle self) (list x (+  ls (- y fontsize)) (+ x sizetext) (+ ls y)))))

(defmethod filtre-extra-p ((self extra-objet) test)
   (cond
    ((string-equal test "all") t)
    ((string-equal test "head") (head-extra-p self))
    ((string-equal test "vel") (vel-extra-p self))
    ((string-equal test "char") (char-extra-p self))
    ((string-equal test "text") (text-extra-p self))
    ((string-equal test "pict") (pict-extra-p self))
    ((string-equal test "line") (line-extra-p self))
    ((string-equal test "grace") (grace-extra-p self))
))

(defmethod kant-grace-p ((self note))
 (when self
            (equal '(1.0 0.0 0.0) (color2list (mus-color self)))))

(defmethod kant-grace-p ((self t)) nil)

;--------------------

