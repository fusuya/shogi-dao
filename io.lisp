(defparameter *name1* '("P" "L" "N" "S" "G" "B" "R" "K" "p" "l" "n" "s" "g" "b" "r" "k"))

(defun send_pv (turn maxdepth te1 score)
  (let ((koma 0))
    (if (= turn *black*)
	(setf koma (- (Te-koma te1) 15))
	(setf koma (Te-koma te1)))
    (format t "info ")
    (format t "depth ~d " maxdepth)
    (format t "nodes ~d~%" (aref *nodes* 0))
    (format t "info string depth=~d nodes=~d score=~d "
	    maxdepth (aref *nodes* 0) score)
    (if (= (Te-utu te1) 1)
	(progn
	  (format t "*")
	  (format t "~d~d" (mod (- 10 (Te-to te1)) 10) (floor (/ (Te-to te1) 10.0)))
	  (case (Te-koma te1)
	    (*SFU*
	     (format t "FU"))
	    (*SKY*
	     (format t "KY"))
	    (*SKE*
	     (format t "KE"))
	    (*SGI*
	     (format t "GI"))
	    (*SKI*
	     (format t "KI"))
	    (*SHI*
	     (format t "HI"))
	    (*SKA*
	     (format t "KA"))))
	(progn
	  (format t "~d~d" (mod (- 10 (Te-from te1)) 10) (floor (/ (Te-from te1) 10.0)))
	  (format t "~d~d" (mod (- 10 (Te-to te1)) 10) (floor (/ (Te-to te1) 10.0)))
	  (case koma
	    (*SFU*
	     (format t "FU"))
	    (*SKY*
	     (format t "KY"))
	    (*SKE*
	     (format t "KE"))
	    (*SGI*
	     (format t "GI"))
	    (*SKI*
	     (format t "KI"))
	    (*SHI*
	     (format t "HI"))
	    (*SKA*
	     (format t "KA"))
	    (*STO*
	     (format t "TO"))
	    (*SNY*
	     (format t "NY"))
	    (*SNE*
	     (format t "NE"))
	    (*SNG*
	     (format t "NG"))
	    (*SUM*
	     (format t "UM"))
	    (*SRY*
	     (format t "RY"))
	    (*SOU*
	     (format t "OU")))
	  (if (= (Te-pro te1) 1)
	      (format t "+"))))
    (format t "~%")))
	

(defun make_moves_from_usi (buf)
  (let ((from 0)
	(to 0)
	(nari 0)
	(utu 0)
	(turn 0))
    (cond
      ((null buf) 0)
      (t
       (loop for i from 0 to (- (length buf) 1)
	  do (let* ((te1 (make-Te))
		    (sashite (nth i buf))
		    (txt (coerce sashite 'list)))
	       (cond
		 ((= 42 (char-code (second txt)))
		  (setf utu 1
			from (+ (mod (position (first txt) *name1* :test #'equal) 8) 1)
			to (+ (- 9 (- (char-code (third txt)) 49))
			      (* (+ (- (char-code (fourth txt)) 97) 1) 10))))
		 ((> (length sashite) 4)
		  (setf nari 1
			from (+ (- 9 (- (char-code (first txt)) 49))
				(* (+ (- (char-code (second txt)) 97) 1) 10))
			to (+ (- 9 (- (char-code (third txt)) 49))
			      (* (+ (- (char-code (fourth txt)) 97) 1) 10))))
		 (t (setf from (+ (- 9 (- (char-code (first txt)) 49))
				  (* (+ (- (char-code (second txt)) 97) 1) 10))
			  to (+ (- 9 (- (char-code (third txt)) 49))
				(* (+ (- (char-code (fourth txt)) 97) 1) 10)))))
	       (if (= utu 1)
		   (setf (Te-koma te1) from)
		   (setf (Te-koma te1) (aref *board* from)))
	       (setf (Te-cap te1) (aref *board* to)
		     (Te-from te1) from
		     (Te-to te1) to
		     (Te-pro te1) nari
		     (Te-utu te1) utu)
	       (Move turn te1)
	       (setf turn (mod (+ turn 1) 2))))))
      turn))

(defun send_best (turn te1)
  (let ((koma 0))
    (format t "bestmove ")
    (if (= (Te-utu te1) 1)
	(progn
	 (case (Te-koma te1)
	  (*SFU*
	   (format t "P"))
	  (*SKY*
	   (format t "L"))
	  (*SKE*
	   (format t "N"))
	  (*SGI*
	   (format t "S"))
	  (*SKI*
	   (format t "G"))
	  (*SHI*
	   (format t "R"))
	  (*SKA*
	   (format t "B")))
	 (format t "*"))
	(progn
	  (if (= turn *black*)
	      (setf koma (- (Te-koma te1) 15))
	      (setf koma (Te-koma te1)))
	  (let ((suji (mod (- 10 (Te-from te1)) 10))
		(dan (floor (/ (Te-from te1) 10.0))))
	    (format t "~d" suji)
	    (case dan
	      (1
	       (format t "a"))
	      (2
	       (format t "b"))
	      (3
	       (format t "c"))
	      (4
	       (format t "d"))
	      (5
	       (format t "e"))
	      (6
	       (format t "f"))
	      (7
	       (format t "g"))
	      (8
	       (format t "h"))
	      (9
	       (format t "i"))))))
    (let ((suji (mod (- 10 (Te-to te1)) 10))
	  (dan (floor (/ (Te-to te1) 10.0))))
      (format t "~d" suji)
      (case dan
	(1
	 (format t "a"))
	(2
	 (format t "b"))
	(3
	 (format t "c"))
	(4
	 (format t "d"))
	(5
	 (format t "e"))
	(6
	 (format t "f"))
	(7
	 (format t "g"))
	(8
	 (format t "h"))
	(9
	 (format t "i")))
      (if (= (Te-pro te1) 1)
	  (format t "+"))
      (format t "~%"))))

  
