;;; ====================================================================
;;;
;;;     OMSIB LIBRARY
;;;
;;;     2024 - by Paulo Raposo
;;;
;;;      ADAPTED FROM: OM2Lily 2.0 v1.1
;;;      © 2005 IRCAM - Karim Haddad
;;;
;;; ====================================================================

(in-package :om-sibelius)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL VARIABLES
  
(defvar *approx-midic* nil)
(setf *approx-midic* om::*global-midi-approx*)

;(defvar *sib-chan-on* nil)
(defvar *sib-dyn-on* t)
(defvar *tempdyn* nil)
(defvar *custom-dyn* nil)
(defvar *chars-for-extras* nil)

(defvar *score-number-of-measures* nil)
(defvar *treeratios* '())
(defvar *voice-note-positions* '())
(defvar *voice-note-durations* '())
(defvar *measure-note-positions* '())
(defvar *measure-note-durations* '())
(defvar *chords-and-cont* '())
(defvar *voice-num* 0)
(defvar *mem-mes* nil)
(defvar *mesure-num* 0)
(defvar *tuplet-note* nil)
(defvar *tuplet-position* nil)
(defvar *tuplet-depth* nil)
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXTRAS

(setf *chars-for-extras* ;<=== omextras font
;;;dynamics
 '(("ffff" . "i") ("fff" . "h") ("ff" . "g") ("f" . "f") ("mf" . "F") ("sfz" . "e") ("mp" . "P") ("p" . "p") ("pp" . "Q") ("ppp" . "R") ("pppp" . "S")
;;;pauses
  ("(." . "}") (".)" . "|")
;;;piano pedal
 ("Ped." . "q") ("*" . ":")
;;;articulations
 (".>" . "y") ("._" . "u") (("_.") . "v") ("v" . "{") ("^" . "w") ("V" . "r") ("[" . "s") (">" . ">") ("o" . "/") ("+" . "+") ("_" . "_")
))

(defun get-dyn-from-om (elmt)
(let (res)
  (loop 
   for i in *custom-dyn*
   do (if (and (<= (caar i) elmt) (>= (second (car i)) elmt))
          (push i res)))
  (second (car res))))

  ;(get-dyn-from-om 127)

(setf *custom-dyn* 
    (list (list (list 0 20)  "ppp")
          (list (list 21 40)  "pp")
          (list (list 41 55)  "p")
          (list (list 56 60)  "mp")
          (list (list 61 85)  "mf")
          (list (list 86 100)  "f")
          (list (list 101 115)  "ff")
          (list (list 116 127)  "fff")
          ))

  ;;; ARTICULATIONS

(defun print-articulation-numbers ()
 (om::om-show-output-lines 
"Custom1Artic = 0
StaccatoArtic = 1
StaccatissimoArtic = 2
WedgeArtic = 3
TenutoArtic = 4
AccentArtic = 5
MarcatoArtic = 6
HarmonicArtic = 7
PlusArtic = 8
UpBowArtic = 9
DownBowArtic = 10
Custom2Artic = 11
SquarePauseArtic = 12
PauseArtic = 13
TriPauseArtic = 14
Custom3Artic = 15"
"ARTICULATIONS"
))

  (defvar *sib-articulations* nil)
  (setf *sib-articulations* '(("c1" 0)("." 1)("'" 2)("," 3) ("_" 4)(">" 5)("^" 6) ("v" 6)
  ("o" 7)("+" 8) ("V" 9) ("[" 10) ("c2" 11) ("[." 12) ("(." 13) ("<." 14) ("c3" 15)))

  (defvar *sib-articulations-hash* (make-hash-table :test #'equal))

  (defun fill-sibelius-articulations-hash ()
  "This function fills a articulations Hash Table, which contains the articulation [string](key)
  and the articulation number."
  (let ((articulations *sib-articulations*))
  (clrhash *sib-articulations-hash*)
  		 (dolist (a articulations)
  		  (setf (gethash (first a) *sib-articulations-hash*) 
  			     (second a)))))
				 
  (fill-sibelius-articulations-hash)

(defun get-articulation-num (art)
 (car (om::list! (gethash art omsib::*sib-articulations-hash*))))
   
(defvar *artnum-str* nil)
(setf *artnum-str* '((7 "o") (2 "'") (8 "+") (6 "v") (3 ",") (1 ".") (9 "V") 
 (13 "(.") (10 "[") (15 "c3") (6 "^") (4 "_") (11 "c2") (5 ">") (0 "c1") (12 "[.") (14 "<.")))
    
(defun artnum->str (num)
(car (cassq num *artnum-str*)))

;;; INSTRUMENTS

(defvar *SIB-instruments-file* (namestring
                             (make-pathname :directory (pathname-directory *load-pathname*)
                                            :name "sib-instruments.lisp")))		  
	  
(defvar *sib-instruments-hash* (make-hash-table :test #'equal))

(defun fill-sibelius-instruments-hash ()
"This function fills a instruments Hash Table, which contains the name of the instrument(key)
and the instrument type accepted by Sibelius Manuscript Language."
(let (instruments)
(with-open-file (in *SIB-instruments-file* :direction :input)
 (setq instruments (eval (read in))))
(clrhash *sib-instruments-hash*)
	 (dolist (i instruments)
	  (setf (gethash (first i) *sib-instruments-hash*) 
		     (second i)))))
			 
(fill-sibelius-instruments-hash)

(defun get-sib-instrument (instrument-name)
"This function returns a instrument type (Manuscript Language) from a insrument longname (string)." 
(string-downcase (car (om::list!(gethash (string-downcase instrument-name) *sib-instruments-hash*)))))

(defun print-sib-instruments ()
(let (instruments)
(maphash #'(lambda (x y) (setq instruments (if (null instruments) 
				                       (format nil "~a" x)
						       (format nil (concatenate 'string instruments "~%~a") x))))
*sib-instruments-hash*)
(om::om-show-output-lines (format nil "~a" instruments) "INSTRUMENTS")))

(defun list-sib-instruments (list)
"This function returns a list with three elements: instrument type, instrument longname and instrument shortname. 
The input argument can be a single list with two elements (strings), instrument longname and instrument shortname, or
a list of list with these two arguments."
(if (and (listp list) (every #'atom list))
    (let ((instid (get-sib-instrument (first list))))
   (om::x-append instid (instid-to-longname (first list)) (second list)))
   (loop for el in list
            collect  (let ((instid (get-sib-instrument (first el))))
                         (om::x-append instid (instid-to-longname (first el)) (second el))))))

(defun get-user-instrument (prompt &key (initial-string "") owner 
                                 (size (om::om-make-point 600 100))
                                 (position (om::om-make-point 200 140)))
 (om-api::prompt-for-string  prompt :initial-value initial-string :title "Search Instrument"))
	 
(defun search-sib-instruments (&optional name)
 (let ((str (if name 
	 	        name
	 	       (get-user-instrument "Type an instrument name:")))
	   (found '()))
  (maphash #'(lambda (name id) 
 	          (when (search str name) 
				    (setq found (append (list name) found)) 
					))
  *sib-instruments-hash*)
  found))

(defun instid-to-longname (instid)
 (let ((posn1 (search "[" instid))
         (posn2 (search "(" instid)))
(cond ((and posn1 posn2)
           (subseq instid 0 (1- (min posn1 posn2))))
          ((and posn1 (null posn2))
           (subseq instid 0 (1- posn1)))
          ((and (null posn1) posn2)
           (subseq instid 0 (1- posn2)))
          (t instid))))
     
;;; LINES

(defvar *SIB-lines-file* (namestring
                              (make-pathname :directory (pathname-directory *load-pathname*)
                                             :name "sib-lines.lisp")))		  
		  
(defvar *sib-lines-hash* (make-hash-table :test #'equal))

(defun fill-sibelius-lines-hash ()
"This function fills a lines Hash Table, which contains the name of the line (key)
and the line style accepted by Sibelius Manuscript Language."
(let (lines)
(with-open-file (in *SIB-lines-file* :direction :input)
  (setq lines (eval (read in))))
(clrhash *sib-lines-hash*)
		 (dolist (l lines)
		  (setf (gethash (first l) *sib-lines-hash*) 
			     (second l)))))
				 
(fill-sibelius-lines-hash)

(defun get-sib-line (line)
 "This function returns a line style (Manuscript Language) from a string(line). " 
(car (om::list! (gethash line *sib-lines-hash*))))

(defun print-sib-lines ()
(let (lines)
(maphash #'(lambda (x y) (setq lines (if (null lines) 
										 (format nil "~a" x)
										 (format nil (concatenate 'string lines "~%~a") x))))
 *sib-lines-hash*)
(om::om-show-output-lines (format nil "~a" lines) "LINES")))
			 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;Utilities;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;this function is for getting the symbolic note figure 
;;;;;;of an object
;;;;;; important for stemming


(defmethod get-note-figure ((self t))
  (remove nil (om::flat (getdembeams self nil nil))))

(defmethod getdembeams ((self om::poly) listclefs freestore)
  (let ((rep nil) 
        (voices (om::inside self)))
    (loop for staff in voices
          for i = 0 then (+ i 1) do
          (setf rep (append rep (getdembeams staff (nth i listclefs) freestore))))
    
    rep))

(defmethod getdembeams ((self om::voice) clef freestore)
  (let* ((rep nil)
         (mesures (om::inside self))
         (lastmes nil))
    (loop for mes in mesures
          for i = 1 then (+ i 1) do
          (setf rep (append rep (getdembeams mes lastmes freestore)))
          (setf lastmes mes))
    rep))

(defmethod getdembeams ((self om::measure) lastmes chiffrage)
  (let* ((inside (om::inside self))
         (tree (om::tree self))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
         (rep nil))
    
    (loop for obj in inside do
          (setf rep (list rep 
                            (let* ((dur-obj-noire (/ (om::extent obj) (om::qvalue obj)))
                                   (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
                                   (exp (getdembeams obj (* symb-beat-val factor) (car (om::tree self)))))
                              exp
                              )
                            )))
    rep))

(defmethod getdembeams ((self om::group) dur ratio)
  (let* ((durtot (if (listp dur) (car dur) dur))
         (cpt (if (listp dur) (cadr dur) 0))
         (num (or (om::get-group-ratio self)  (om::extent self)))
         (denom (om::find-denom num durtot))
         (num (if (listp denom) (car denom) num))
         (denom (if (listp denom) (second denom) denom))
         (unite (/ durtot denom))
         (inside (om::inside self))
         (sympli (/ num denom))
         (rep nil) (val nil))
   
    (cond
     ((not (om::get-group-ratio self)) 
      (loop for obj in inside
            do (setf rep (list (om::list! rep) (let* ((dur-obj (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                        (/ (om::extent self) (om::qvalue self)))))
                                       (getdembeams obj (* dur-obj durtot) ratio))))))
     ((= sympli 1)
      (loop for obj in inside
            do (setf rep (list (om::list! rep) (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                          (/ (om::extent self) (om::qvalue self))))
                                            (dur-obj (numerator (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                                   (/ (om::extent self) (om::qvalue self))))))
                                       (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                       (getdembeams obj (* dur-obj unite) ratio)))))
      
      )
     

     (t 

(let ((pos (length rep))
            (depth 0))
        (loop for obj in inside do
              (setf rep (list (om::list! rep) (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                         (/ (om::extent self) (om::qvalue self))))
                                           (dur-obj (numerator operation))
                                           exp tmp)
                                      (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                      
                                      (setf tmp (multiple-value-list 
                                                 (getdembeams obj (list (* dur-obj unite) cpt) ratio)))
                                      
                                      
                                      (setf exp (car tmp))
                                      (when (and (cadr tmp) (> (cadr tmp) depth))
                                        (setf depth (cadr tmp)))
                                      exp
                                      ;(list exp)
                                      ))))
        (setf val (+ depth 1))
        
        
        )

      )

     ) 
    (values rep val)))

(defmethod getdembeams ((self om::chord) dur ratio)
  ;(print dur)
  (if (listp dur) (car dur) dur))

(defmethod getdembeams ((self om::rest) dur ratio)
  (if (listp dur) (car dur) dur)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;DA CODE;;;;;;;;;;;;;;;
;;;;where everything is transcribed

;;;;;;;;;;;;;;Utilities section

(defun cassq (sym l)
  (cdr (assoc sym l)))

(defun print-sib-text (voice &optional articulations lines)
  (setf *mesure-num* 0)
  (setf *voice-num* 1)
  (setf *score-number-of-measures* (om::list-max (get-number-of-measures voice)))
  (when (> *approx-midic* 4) (setf *approx-midic* 4))
  (loop for elt in (cons-sib-text voice (list "unnamed (treble staff)" nil nil) (list articulations lines)) do (print elt)))


;;;for extras
;(defun massq (item list)
;(format nil "~S" (cdr (assoc item list :test 'equal))))  

(defun massq (item list)
(cdr (assoc item list :test 'equal)))

(defun get-char-extra-from-string (item)
 (massq item *chars-for-extras*))

(defmethod get-number-of-measures ((object om::voice))
 (length (om::inside object)))	
	 
(defmethod get-number-of-measures ((object om::poly))
  (mapcar #'get-number-of-measures (om::inside object)))

(defmethod cons-sib-text ((self om::poly) instruments articulations-lines)
  (let ((rep '())
         (voices (om::inside self))
        (articulations-lines (om::mat-trans articulations-lines)))	 

  (when (> *approx-midic* 4) (setf *approx-midic* 4))
   		 
  (setf *score-number-of-measures* (om::list-max (get-number-of-measures self)))
  
    (setf *voice-num* 0)
    (loop for staff in voices
          for i = 0 then (+ i 1) 
          do
          (progn 
          (setf *voice-num* (incf *voice-num*))
          (setf rep (append rep (cons-sib-text staff (nth i instruments) (nth i articulations-lines))))))
   
   (om::save-data (mapcar #'list (om::flat rep)))))

(defun cons-voice-positions-and-durations (voice)
 (let* ((tree (om::tree voice))
       (ratios (om::flat (om::tree-to-ratios tree)))
	   (rests-posn (loop for obj in (om::collect-chords voice)
		   	             for x from 0 
						 when (om::rest-p obj)
						 collect x))
	(sibelius-durs (om::om* ratios 1024))
       (onsets (butlast (om::dx->x 0 (om::om-abs ratios))))
       (sibelius-positions (om::om* onsets 1024)))
 (progn (setf *voice-note-positions* (om::remove-nth sibelius-positions rests-posn))
        (setf *voice-note-durations* (om::remove-nth sibelius-durs rests-posn)))
   ))

(defun format-lines (positions-no-rests lines) 
(if (null lines) 
   nil
(loop for line in lines
     collect (let* ((notes-posn (om::posn-match positions-no-rests (butlast line)))
                        (line-posn (first notes-posn))
                        (line-dur (- (second notes-posn) line-posn)))
                  (list (format nil "P~a" (round line-posn))
                        (format nil "D~a" (round line-dur))
                        (format nil "L~a" (get-sib-line (third line))))))))
							
(defun measure-tree-to-ratio (measure)
"This function returns a list of ratios from a measure object without ties."
(om::flat (om::tree-to-ratios (list 1 (list (om::tree measure))))))
	
(defun cons-measure-positions-and-durations (measure)
 (let* ((ratios (om::flat (measure-tree-to-ratio measure)))
	   (sibelius-durs (om::om* ratios 1024))
       (onsets (butlast (om::dx->x 0 (om::om-abs ratios))))
       (sibelius-positions (om::om* onsets 1024)))
 (progn 
  (setf *measure-note-durations* sibelius-durs)
   sibelius-positions)))
	   
(defmethod cons-sib-text ((self om::voice) instrument articulations-lines)
  (setf *mesure-num* 0)
  (setf *tempdyn* nil)
  (let* ((rep '())
         (art (if (first articulations-lines)
		 	      (loop for a in (first articulations-lines)
                        collect (if (listp (second a)) 
							        (om::subs-posn a '(1) (format nil "~{~A~^ ~}" (loop for num in (second a) collect (artnum->str num))))
							        (om::subs-posn a '(1) (artnum->str (second a)))))))
        (new-self (if art (add-extra-text self (mapcar #'second art) (mapcar #'first art))
			               self)) 
        (mesures (om::inside new-self))
        (inst (if (equal (get-sib-instrument (first instrument)) "nil")
                    (progn (om::om-message-dialog (format nil "~A is not a valid Sibelius instrument for voice number ~d.~%Instrument set to unnamed treble staff." instrument *voice-num*)) 
                           (list (get-sib-instrument "unnamed (treble staff)") nil nil))
                  (list-sib-instruments instrument)))
		(lin (second articulations-lines))
        (lastmes nil))
	  (cons-voice-positions-and-durations self)	

      (setf rep (append rep  (list (format nil "i~a" (first inst))
       			 			(if (second inst) (format nil "l~a" (second inst)) "l ")
        					(if (third inst) (format nil "s~a" (third inst)) "s "))))
												
(cond ((= 1 *voice-num*)
          (setf *mem-mes* nil)
          (loop for mes in mesures
                for i = 1 then (+ i 1) do				            
                (setf *mesure-num* (incf *mesure-num*))
		(if (and (> *score-number-of-measures* 5) (= *mesure-num* 1))
                    (setf rep (append rep (list (format nil "B~d" (- *score-number-of-measures* 5))))))	
                (setf rep (append rep (list (format nil "b~d" *mesure-num*))))
                (let ((tempo (if (atom (om::qtempo mes)) 
                                 (om::qtempo mes)
                                 (cadar (om::qtempo mes))))
                        )
                  ;(print tempo) ;;this is to check the tempo....
                  (if (not (equal tempo *mem-mes*))
                      (progn (setf *mem-mes* tempo) 
                                  (setf tempo (list (format nil "u~a" 256) (format nil "e~a" tempo))))
                      (setf tempo nil))
                (setf rep (append rep (cons-sib-text mes lastmes tempo)))
				(setf rep (append rep (format-lines *voice-note-positions* lin)))
                (setf lastmes mes)
                )))

      (t (loop for mes in mesures
            for i = 1 then (+ i 1) do
            (setf *mesure-num* (incf *mesure-num*))
            (setf rep (append rep (list (format nil "b~d" *mesure-num*))))
            (setf rep (append rep (cons-sib-text mes lastmes nil)))
			(setf rep (append rep (format-lines *voice-note-positions* lin)))
            (setf lastmes mes))
      ))
 (if *score-number-of-measures*
	 (progn (setf *score-number-of-measures* nil)
             rep)
	 rep)))

;symb-beat-val= For a key signature equivalent to 3//3 will be the half note (blanche)
;real-beat-val= For the same key sign, this will be the halfnote of a triplet (blanche de triolet)
;These refer to the beats in a measure, and for special cases using non-standard key signature

(defmethod cons-sib-text ((self om::measure) lastmes tempo)
  (setf *chords-and-cont* (om::collect-chords  self))
  (setf *treeratios* (get-note-figure self))
  (setf *measure-note-positions* (cons-measure-positions-and-durations self))
  (let* ((inside (om::inside self))
         (tree (om::tree self))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
         (rep nil))

  ;(print tempo) ;;this is to check the tempo....
    (setf rep (if tempo 
			     (list (format nil "S~d" *mesure-num*) tempo)
                 (list (format nil "S~d" *mesure-num*))))
				 
 (unless (and lastmes (equal (first tree) (first (om::tree lastmes))) (= *voice-num* 1))
   (setf rep (append rep (list (format nil "g~a" (om::fnumerator (first tree)))
                               (format nil "f~a" (om::fdenominator (first tree)))))))
	  
    (loop for obj in inside do
          (setf rep (append rep 
                            (let* ((dur-obj-noire (/ (om::extent obj) (om::qvalue obj)))
                                   (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
                                   (exp (cons-sib-text obj (* symb-beat-val factor) nil)))
                              exp
                              )
                            )))
    rep))

(defun get-sibnote-durs (list)
 (loop for el in list
	  when (not (every #'stringp el))	 
	  collect (if (= (length el) 2)
	              (second el)
	              (parse-integer (subseq (third el) 1)))))

(defmethod cons-sib-text ((self om::group) dur tempo)
  (let* ((durtot (if (listp dur) (car dur) dur))
         (cpt (if (listp dur) (cadr dur) 0))
         (num (or (om::get-group-ratio self)  (om::extent self)))
         (denom (om::find-denom num durtot))
         (num (if (listp denom) (car denom) num))
         (denom (if (listp denom) (second denom) denom))
         (unite (/ durtot denom))
         (inside (om::inside self))
         (sympli (/ num denom))
         (rep nil) (val nil))
    ;(print durtot)
    (cond
     ((not (om::get-group-ratio self))
      (loop for obj in inside
            do (setf rep (append rep (let* ((dur-obj (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                        (/ (om::extent self) (om::qvalue self)))))
                                       (cons-sib-text obj (* dur-obj durtot) nil))))))
     ((= sympli 1)
      (loop for obj in inside
            do (setf rep (append rep (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                          (/ (om::extent self) (om::qvalue self))))
                                            (dur-obj (numerator (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                                   (/ (om::extent self) (om::qvalue self))))))
                                       (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                       (cons-sib-text obj (* dur-obj unite) nil))))))
     
     (t
      (let ((pos (length rep))
            (depth 0) 
            (tree (om::tree self))
             nested?)
        (if (om::ratiop (car tree))
            (progn (setf nested? t) 
                       (incf *tuplet-depth*))
            (setf *tuplet-depth* 0))

        (setf rep (append rep  (list 
                                (if (= *tuplet-depth* 0)
                                    (list (format nil "o~d" (first *measure-note-positions*))
				          (format nil "w~d" num)
				          (format nil "q~d" denom)
				          (format nil "c~d" (* unite 1024))
                                          (format nil "T~d" *tuplet-depth*))
                                  (list "?"
				          (format nil "z~d" num)
				          (format nil "j~d" denom)
				          (format nil "k~d" (* unite 1024))
                                          (format nil "T~d" *tuplet-depth*))
                                  ))))
          
        (loop for obj in inside do
              (setf *tuplet-note* t)
              (setf rep (append rep (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                         (/ (om::extent self) (om::qvalue self))))
                                           (dur-obj (numerator operation))
                                           exp tmp)
                                      (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                      (setf tmp (multiple-value-list 
                                                 (cons-sib-text obj (list (* dur-obj unite) cpt) nil)))
                                      (setf exp (car tmp))                                              
                                      (when (and (cadr tmp) (> (cadr tmp) depth))
                                                 (progn (setf depth (cadr tmp))
                                                   (decf *tuplet-depth*)))
                                      exp
                                      )))
               (setf *tuplet-note* nil))

        (setf val (+ depth 1))

        (let* ((tuplet-durs (if (= depth 0)
                                        (get-sibnote-durs rep)
                                        (om::om* 1024 (loop for obj in inside
                                                                          collect (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                                                                                  (/ (om::extent self) (om::qvalue self))))
                                                                                             (dur-obj (numerator operation))
                                                                                              exp tmp)
                                                                                      (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                                                                       (* dur-obj unite))))))
              (tuplet-positions (mapcar #'first rep))
              (tuplet-positions (om::om- (butlast (om::dx->x (second tuplet-positions) tuplet-durs)) (second tuplet-positions)))
              )
          (setf rep (let ((n-nested (apply #'+ (loop for el in rep when (equal "?" (car el)) collect 1)))
                               (count 1))
                         (loop for el in rep 
                                   if (= (length el) 2)
                                       collect (pop tuplet-positions) 
                                       into rests
                                   else
                                       collect (cond ((every #'stringp el)
                                                              (cond ((equal (first el) "?")
                                                                         (if (= depth 0)
                                                                              el
                                                                             (if (= count n-nested)
                                                                                 (om::subs-posn el '(0) (format nil "n~d" (pop tuplet-positions)))
                                                                                 (progn (incf count) el))))
                                                                         (t el)))
                                                             (t (om::subs-posn el '(0) (format nil "p~d" (pop tuplet-positions)))))
                                       into args
                                  finally (return args))))
          )
          
        )
      ))
    (values rep val)
))

(defun get-extra-text (liste)
  (remove 'nil 
  (loop for i in liste
        collect (if (om::text-extra-p i) (om::thetext i)))))

;not needed anymore ?
(defun get-extra-vel (liste)
  (remove 'nil 
  (loop for i in liste
        collect (if (om::vel-extra-p i) (om::thechar i)))))
	 
(defmethod cons-sib-text ((self om::chord) dur tempo)
  (let* ((notes (om::inside self))
         (extra (car (mapcar #'om::extra-obj-list notes)))
         (text (get-extra-text extra))
         (velex (if (om::vel-extra-p (car extra))
                    (om::thechar (car extra))))
         (durtot (if (listp dur) (car dur) dur))
         (inside (om::inside self))
         (vel (car (om::lvel self)))
         (dyn (get-dyn-from-om vel))
         (chans (om::lchan self))
         (sib-posn (pop *measure-note-positions*))
         (sib-dur (pop *measure-note-durations*))
         (tup? *tuplet-note*)
         rep) 

     (setf rep (om::x-append  (if tup? sib-posn (format nil "p~d" sib-posn))
                    (if (= 1 (length notes)) 
                    (format nil "m~d" (let ((midi (/ (om::approx-m (om::midic (car notes)) *approx-midic*)  100))) 
                                                (if (integerp midi) midi (float midi))))
                        (format nil "m~d" (format nil "~{~A~^ ~}" 
                                                 (loop for note in inside collect (let ((midi (/ (om::approx-m (om::midic note) *approx-midic*)  100))) 
                                                                                                 (if (integerp midi) midi (float midi)))))))
                   (format nil "d~d" (if tup? (* durtot 1024) sib-dur))
                    (if (or (and (not (om::cont-chord-p self))
                                     (om::cont-chord-p (om::next-container self '(om::chord))))
                              (and (om::cont-chord-p self)
                                      (om::cont-chord-p (om::next-container self '(om::chord))))
                        )
                    (format nil "t~d" 1)  
                    (format nil "t~d" 0))
                    (when (not (equal dyn *tempdyn*))
                        (progn (setf *tempdyn* dyn)
                          (list (format nil "y~d" sib-posn) 
                                (format nil "v~a" dyn))))
                     (if tup? "xTrue" "xFalse")
                     (if tup? (format nil "X~a" *tuplet-depth*) "XFalse")
                      (when text 
                              (let* ((art-num (mapcar #'get-articulation-num text)))
                                 (format nil "a~a" (format nil "~{~A~^ ~}" (loop for num in art-num collect (format nil "~a" num))))))
                    ))
(list rep)
))

(defmethod cons-sib-text ((self om::rest) dur tempo)
 (let ((durtot (if (listp dur) (car dur) dur))
	 (sib-dur (pop *measure-note-durations*))
 	(sib-posn (pop *measure-note-positions*))
        (tup? *tuplet-note*))
  (when tup? (list (list sib-posn (* durtot 1024))))))	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OM-INTERFACE

;;; EXTRAS 

(om::defmethod! add-extra-text ((self om::voice) 
                        (liste list) &optional
                        (positions nil) (modify nil))
 :initvals (list om::t '(0 1) nil nil) 
 :indoc '("voice" "list-of-extras" "plc-in-voice")
 :icon 99
 :doc "The text list is a list of strings."
(let* ((clone (om::clone self))
       (chords (om::collect-chords clone))
       (flt-chrds 
        (remove nil
                (loop for i in chords
                      collect (if 
                                  (and (om::chord-p i)
                                       (not (om::cont-chord-p i)))
                                  i))))
       (posn-chrds (if positions (remove nil (om::posn-match flt-chrds positions))
                                  flt-chrds))
       (extras (loop for i in liste
                     collect (let* ((art (om::split-sequence " " i))
                                         ;(stem-up? (if (equal om::*chord-stem-dir* "up") t nil)) 
				        (init-deltay 1) ;(if stem-up? 1 -3))
				        )
				           (if (= (length art) 1)
				  	           (make-instance 'om::text-extra
					  	                      :deltax -0.1
                                                 :deltay init-deltay
                                                 :thetext (car art))
								(loop for a in art 
									  for x from 0 by 1.5
									  collect (make-instance 'om::text-extra
					  	                      :deltax -0.1
                                                 :deltay (+ init-deltay x) ;(if stem-up? (+ init-deltay x) (- init-deltay x))
                                                 :thetext a)))))))
  (loop for chrd in posn-chrds
        for ext in extras do    
       (if (om::text-extra-p ext)
	   (om::add-extra-list chrd ext "exact" nil)
             (loop for e in ext do (om::add-extra-list chrd e "exact" nil))))
  clone
  ))

		
(om::defmethod! add-extra-vel ((self om::voice) &optional (modify nil))
 :initvals (list om::t nil) 
 :indoc '("voice" nil)
 :icon 99
 :doc "Adds a vel-extra for each chord in voice."

 (setf *tempdyn* nil)

 (let* ((clone (om::clone self))
       (chords (om::collect-chords clone))
       (flt-chrds 
       (remove nil
                (loop for i in chords
                     collect (if 
                                 (and (om::chord-p i)
                                      (not (om::cont-chord-p i)))
                                 i))))
    (liste (loop for chord in flt-chrds collect (om::get-dyn-from-vel (car (om::lvel chord)))))
      (extras (loop for i in liste
                    collect (let ((deltax (cond ((equal i :p) 0) 	
 					 	                      ((or (equal i :pp) (equal i :f) (equal i :mf)) -0.1)
 											  ((equal i :ff) -0.2)												  
											  ((or (equal i :ppp) (equal i :fff)) -0.3)
											  (t 0))))						 
				          (make-instance 'om::vel-extra
				  	                      :deltax deltax
                                            :deltay 1.5
                                            :dynamics i)))))
 (loop for chrd in flt-chrds 
       for ext in extras
       do (let ((dynamics (om::dynamics ext)))
		(when (not (equal dynamics *tempdyn*))
			   (progn (om::add-extra-list chrd ext "exact" nil)
	   				    (setf *tempdyn* dynamics))		 
						 )))				
 (progn (setf *tempdyn* nil)
  clone)
 ))

(om::defmethod! add-extra-char ((self om::voice) 
                        (liste list) &optional
                        (positions nil))
 :initvals (list om::t '(0 1)) 
 :indoc '("voice" "plc-in-voice")
 :icon 99
 :doc "Adds a vel-extra for each chord in voice."
(let* ((clone (om::clone self))
       (chords (om::collect-chords clone))
       (flt-chrds 
        (remove nil
                (loop for i in chords
                      collect (if 
                                  (and (om::chord-p i)
                                       (not (om::cont-chord-p i)))
                                  i))))
       (posn-chrds (if positions (remove nil (om::posn-match flt-chrds positions))
                     flt-chrds))
       (extras (loop for i in liste
                     collect (let ((char (om::split-sequence " " i))
				                 ;(stem-up? ===> [???????])
				  		         ;(init-deltay (if stem-up? 1 -3))
				  		         )
				           (if (= (length char) 1)
				  	           (make-instance 'om::char-extra
					  	                      :deltax -0.1
                                                 :deltay 1 ;init-deltay
                                                 :thechar (get-char-extra-from-string (car char))
								(loop for ch in char 
									  for x from 0 by 1.5
									  collect (make-instance 'om::char-extra
					  	                      :deltax -0.1
                                                 :deltay (1+ x) ;(if stem-up? (+ init-deltay x) (- init-deltay x))
                                                 :thechar (get-char-extra-from-string ch)))))))))
  (loop for chrd in posn-chrds
        for ext in extras
        do (if (om::char-extra-p ext)
	 	    (om::add-extra-list chrd ext "exact" nil)
			(loop for e in ext do (om::add-extra-list chrd e "exact" nil))))
  clone
  ))

 ;;; EXPORT 

(om::defmethod! om->sib ((score-object om::voice) &optional (instruments nil) (articulations nil) (lines nil))
 :initvals '( nil nil nil nil)
 :indoc '("voice or poly" "list" "list of lists" "list of lists")
 :icon 99 
 :doc "This functions exports a voice or poly object to a text file (.txt) that should be import into Sibelius using the Import Data from OpenMusic plugin (included in the omsib/resources/ folder).
ARGUMENTS:
<INPUT 0> Voice or poly object
<INPUT 1> List of lists of sibelius instruments. For each voice this argument must include a list with two elements: the instrument longname and instrument shortname.
<INPUT 2> List of lists of articulations. For each voice this argument must include a list of lists with two elements: a note position (starting on zero) and articulation number.
<INPUT 3> List of lists of lines. For each voice this argument must include a list of lists containing three elements: the starting note, ending note and Line Style." 
(let ((poly (om::make-instance 'om::poly :voices (list score-object))))
 (om->sib poly (list instruments) (list articulations) (list lines))))

(om::defmethod! om->sib ((score-object om::poly) &optional (instruments nil) (articulations nil) (lines nil))
 :initvals '( nil nil nil nil)
 :indoc '("voice or poly" "list of lists" "list of lists" "list of lists")
 :icon 99 
 :doc "This functions exports a voice or poly object to a text file (.txt) that should be import into Sibelius using the Import Data from OpenMusic plugin (included in the omsib/resources/ folder).
ARGUMENTS:
<INPUT 0> Voice or poly object
<INPUT 1> List of lists of sibelius instruments. For each voice this argument must include a list with two elements: the instrument longname and instrument shortname.
<INPUT 2> List of lists of articulations. For each voice this argument must include a list of lists with two elements: a note position (starting on zero) and articulation number.
<INPUT 3> List of lists of lines. For each voice this argument must include a list of lists containing three elements: the starting note, ending note and Line Style." 
 (cons-sib-text score-object instruments (list articulations lines)))

;;; DOC 

(om::defmethod! show-sib-instruments ()
 :icon 100 
 :doc "This function opens a documentation window containing all available instrument names."
 (print-sib-instruments))
 
 (om::defmethod! search-sib-instrument (&optional (name nil))
  :initvals '(nil)
  :icon 100 
  :doc "This function searches for an a instrument long name (or instruments longn names) which contains the argument <name>."
 (search-sib-instruments name))

(om::defmethod! show-sib-articulations ()
 :icon 99 
 :doc "This function opens a documentation window containing all available articulations."
 (print-articulation-numbers))
 
 (om::defmethod! show-sib-lines ()
  :icon 101 
  :doc "This function opens a documentation window containing all available lines."
  (print-sib-lines))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
