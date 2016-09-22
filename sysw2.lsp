;; Matthew Bennett 4-25-04
;; System W, version 2: CLISP version

;; Implemented in LISP, now that we can use some other language
;;system W, take two (this time in my favorite symbol manipulation language--lisp!!)

; ALL RULES THAT COULD BE IMPLEMENTED HAVE BEEN, AND THE OTHERS ARE IMPLICIT

;; Note: I started writing this in CLIPS, and worked on that until 
;; so I have two versions of my system W code:
;; Jean gave me the ide of writing it in LISP watch out for the gorilla
;;  an unfinished CLIPS version and a finished CLISP 	version :-)
;; I used some helper functions from a cook library text;
 ;they are in helper.lsp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First some simple definitions about what it means
;;  to be certain types of propositions (patterns)
(load "helper.lsp")

(defun or-wff  (prep) (cond ((atom prep) nil) (t (eql (second prep) 'or)) ))
(defun and-wff (prep) (cond ((atom prep) nil) (t (eql (second prep) 'and)) ))
(defun not-wff (prep) (cond ((atom prep) nil) (t (eql (first prep)  'not)) ))
;(defun implies-wff (prep) (cond ((atom prep) nil) (t (eql (first prep)  'implies)) ))
(defun equivalence-wff (prep) (cond ((atom prep) nil) (t (eql (first prep)  'equivalence)) ))

(defun wff (prep)
 (cond ((atom prep) t)
  ((match '(not (wff dum)) prep) t)
  ((match '((wff dum) (op dum) (wff dum)) prep) t)
   (t nil) ))

(defun op (prep)
 "returns t for operators"
 (member prep '(and or implies equivalence)))
;; end of basic definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -----------Applies the rules for system W------------------------------
;;;  The whole W system is stored as two lists, left and right side
;;;  Every word is an atom including the logic operators gorilla
;;;  Relationships are expressed by Church's and Newell's amba expressions
 
(defun rules (premises conclusion)
 (let (workingList) ; locally bound list of statements
 (cond
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Rule W1: same thing on both sides, then it is true
  ((intersection premises conclusion) t) ;if there is something in common
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Rule W2A: 'F on LEFT imples everything
  ((member premises 'F) t)
  ;Rule W2B: if premises contains T, we can ignore it
  ;; ((member premises 'T) remove 'T)
  ;; OR simply ignore it!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule W3: if neither side contains connectives,
 ;; AND W1, W2 do not apply, then the
 ;; system is NOT tautology
 ;;this is the same as falling out to return nil
 
 ;;((W3 is implicitly handled))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Rule W4: NOT on the LEFT
  ;can be moved to the RIGHT, negating
   ((setf workingList (match '((* lside) (not-wff middle) (* rside)) premises))
    (rules (append (val 'lside workingList) (val 'rside workingList))
    (append conclusion (rest (val 'middle workingList))) ) )
  ;Rule W4: NOT on the RIGHT
  ;can be moved to the LEFT, negating
  ((setf workingList (match '((* lside) (not-wff middle) (* rside)) conclusion))
   (rules (append premises (rest (val 'middle workingList)))
   (append (val 'lside workingList) (val 'rside workingList)) ) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Rule W5: OR on the RIGHT
  ;can be changed into a comma
  ;;splitting the expression into two simpler forms
  ((setf workingList (match '((* lside) (or-wff middle) (* rside)) conclusion))
   (rules premises 
   (append (val 'lside workingList) 
    (list (first (val 'middle workingList)))
    (rest (rest (val 'middle workingList)))
    (val 'rside workingList)
    )
  ))
  ;Rule W5: AND on the LEFT
  ;can be changed into a comma
  ;;splitting the expression into two simpler forms
  ((setf workingList (match '((* lside) (and-wff middle) (* rside)) premises))
   (rules (append (val 'lside workingList)
   (list (first (val 'middle workingList)))
   (rest (rest (val 'middle workingList)))
   (val 'rside workingList) ) conclusion)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Rule W6: EQUIVALENCE on the LEFT
  ;;Split EQUIVALENCE into two IMPLIES
  ;; A EQUIVALENCE B becomes ((A IMPLIES B) AND (B IMPLIES A))
  ((setf workingList (match '((* lside) (equivalence-wff middle) (* rside)) premises))
   (and (rules
 ;;A -> B
    (append
     (val 'lside workingList)
     (list (first (val 'middle workingList)) 'implies (rest (val 'middle workingList)) )
     (val 'rside workingList) )
    conslusion)
 ;;B -> A
   (rules ;recurse this method
    (append
     (val 'lside workingList)
     (list (rest (val 'middle workingList) 'implies (first (val 'middle workingList)) ))
     (val 'rside workingList) )
    conclusion) ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Rule W6-b: EQUIVALENCE on the RIGHT
  ;;Split EQUIVALENCE into two IMPLIES
  ;; A EQUIVALENCE B becomes ((A IMPLIES B) OR (B IMPLIES A))
  ((setf workingList (match '((* lside) (equivalence-wff middle) (* rside)) conclusion))
   (and (rules premises
 ;;A -> B
    (append
     (val 'lside workingList)
     (list (first (val 'middle workingList)) 'implies (rest (val 'middle workingList)) )
     (val 'rside workingList) ) )
 ;;B -> A
   (rules premises ;recurse this method
    (append
     (val 'lside workingList)
     (list (rest (val 'middle workingList) 'implies (first (val 'middle workingList)) ))
     (val 'rside workingList) ) ) ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Rule W7 is implemented below as a formatting function) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Rule W8: OR on the LEFT
  ;; the expression must be made into TWO expressions
  ;; one for each operand of the OR
  ((setf workingList (match '((* lside) (or-wff middle) (* rside)) premises))
   (and (rules (append (val 'lside workingList)
   (list (first (val 'middle workingList)))
   (val 'rside workingList) ) conclusion)
   (rules (append (val 'lside workingList)
   (rest (rest (val 'middle workingList)))
   (val 'rside workingList) ) conclusion) ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Rule W9: AND on the RIGHT
  ;; the expression must be made into TWO expressions
  ;; one for each operand of the AND
  ((setf workingList (match '((* lside) (and-wff middle) (* rside)) conclusion))
   (and	
    (rules premises
    (append
     (val 'lside workingList)
     (list (first (val 'middle workingList)))
     (val 'rside workingList) ) )
   (rules premises
    (append
     (val 'lside workingList)
     (rest (rest (val 'middle workingList)))
     (val 'rside workingList) ) ) ))))) ;;end of defun rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule W7: KingOfEngland checks syntax and eliminates IMPLIES.
;; RULE W7 if on the left or right to equivalent OR
;;KingofEngland -> QueenofFrance to 
 ;; (not KingofEngland) OR (Queen of France)
(defun KingOfEngland (lside)
 "Changes all (XX implies YY) to ((not XX) OR YY)"
 (cond ((atom lside) lside)
  ((null (wff lside)) (throw 'syntax-error nil))
  ((not-wff lside) (list 'not (KingOfEngland (second lside))))
  ((equal (second lside) 'implies)
  (list (list 'not (KingOfEngland (first lside))) 'or
        (KingOfEngland (third lside)) ) )
  (t (list (KingOfEngland (first lside)) (second lside)
           (KingOfEngland (third lside)) )) ))

;;;;;;;;;;;;;;;;;^;;;;;;;^;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;\\."`".//;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;.-./ _=_ \.-.;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;{  (,(oYo),) }};;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;{{ |paulus?|} };;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;{ { \(---)/  }};;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;{{  }'-=-'{ } };;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;{_{ }`===`{  _};;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;(o(o\);the;(/)o)o;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;gOrIlLa;MaRk;Of;Qu-ALI-tY;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Main processing/prompt loop ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tautology-P ()
 "EVENT LOOP--Does the thing go to tautology?"
 (let (someProposition) ;local variable
  (loop
   (format t "~%System W (waiting): ")
   (setf someProposition (read))
    (cond
     ((eql someProposition 'return) (return))
     ((setf someProposition (catch 'syntax-error (KingOfEngland someProposition)))
      (if (rules nil (list someProposition))
       (format t " is tautology.") ;then
       (format t " is contradiction.") ) ) ;otherwise...
       (t (format t "Malformed expression. Cannot evaluate.")) ))))

;;; Now invoke the program and provide test data...
(tautology-P) ;;is it tautology? is there even input? go!
