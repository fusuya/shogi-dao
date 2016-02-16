(load "eval.lisp" :external-format :utf-8)
(load "komamove.lisp" :external-format :utf-8)

(defparameter *tebuf* (make-array 600))
(defparameter *tenum* 0)
(defparameter *score* 0)
(defparameter *neweval* 0)
(defparameter *bestte* (make-Te))

(defun max1 (x y)
  (if (> x y)
      x
      y))

(defun swap (buf x y)
  (let ((temp (aref buf x)))
    (setf (aref buf x) (aref buf y))
    (setf (aref buf y) temp)))
  

;;------------指し手生成部分（駒を動かす手）------------------------------------------------------
(defun gen_move (turn tenum tebuf)
  (if (= turn 0)
      (do ((a 0 (1+ a)))
	  ((>= a 111))
	(if (and (> (aref *board* a) 0) (< (aref *board* a) 16))
	    (progn
	    (cond
	      ((or (= (aref *board* a) *SHI*) (= (aref *board* a) *SRY*))
	       (do ((b 0 (1+ b)))
		   ((>= b 4))
		 (do ((c 1 (1+ c)))
		     ((>= c 10))
		   (let ((To (+ a (* (aref *h_Direct* b) c)))) ;;
		     (cond
		       ((or (= (aref *board* To) *WAL*)
			    (and (> (aref *board* To) 0) (< (aref *board* To) 16))) ;;
			(return))
		       ((> (aref *board* To) 15) ;;
			(setf (Te-from (aref tebuf tenum)) a
			      (Te-to (aref tebuf tenum)) To
			      (Te-koma (aref tebuf tenum)) (aref *board* a)
			      (Te-cap (aref tebuf tenum)) (aref *board* To)
			      (Te-utu (aref tebuf tenum)) 0
			      (Te-pro (aref tebuf tenum)) 0)
			(incf tenum)
			(if (and (< To 40) (= (aref *board* a) *SHI*)) ;;
			    (progn
			      (setf (Te-from (aref tebuf tenum)) a
				    (Te-to (aref tebuf tenum)) To
				    (Te-koma (aref tebuf tenum)) (aref *board* a)
				    (Te-cap (aref tebuf tenum)) (aref *board* To)
				    (Te-utu (aref tebuf tenum)) 0
				    (Te-pro (aref tebuf tenum)) 1)
			      (incf tenum)))
			(return)))
		     
		     (setf (Te-from (aref tebuf tenum)) a
			   (Te-to (aref tebuf tenum)) To
			   (Te-koma (aref tebuf tenum)) (aref *board* a)
			   (Te-cap (aref tebuf tenum)) (aref *board* To)
			   (Te-utu (aref tebuf tenum)) 0
			   (Te-pro (aref tebuf tenum)) 0)
		     (incf tenum)
		     ;;
		     (if (and (< To 40) (= (aref *board* a) *SHI*))
			 (progn
			   (setf (Te-from (aref tebuf tenum)) a
				 (Te-to (aref tebuf tenum)) To
				 (Te-koma (aref tebuf tenum)) (aref *board* a)
				 (Te-cap (aref tebuf tenum)) (aref *board* To)
				 (Te-utu (aref tebuf tenum)) 0
				 (Te-pro (aref tebuf tenum)) 1)
			   (incf tenum)))))))
	      ((or (= (aref *board* a) *SKA*) (= (aref *board* a) *SUM*))
	       (do ((b 0 (1+ b)))
		   ((>= b 4))
		 (do ((c 1 (1+ c)))
		     ((>= c 10))
		   (let ((To (+ a (* (aref *k_Direct* b) c))))
		     (cond
		       ((or (= (aref *board* To) *WAL*)
			    (and (> (aref *board* To) 0) (< (aref *board* To) 16)))
			(return))
		       ((> (aref *board* To) 15)
			(setf (Te-from (aref tebuf tenum)) a
			      (Te-to (aref tebuf tenum)) To
			      (Te-koma (aref tebuf tenum)) (aref *board* a)
			      (Te-cap (aref tebuf tenum)) (aref *board* To)
			      (Te-utu (aref tebuf tenum)) 0
			      (Te-pro (aref tebuf tenum)) 0)
			(incf tenum)
			(if (and (< To 40) (= (aref *board* a) *SKA*))
			    (progn
			      (setf (Te-from (aref tebuf tenum)) a
				    (Te-to (aref tebuf tenum)) To
				    (Te-koma (aref tebuf tenum)) (aref *board* a)
				    (Te-cap (aref tebuf tenum)) (aref *board* To)
				    (Te-utu (aref tebuf tenum)) 0
				    (Te-pro (aref tebuf tenum)) 1)
			      (incf tenum)))
			(return)))
		     (setf (Te-from (aref tebuf tenum)) a
			   (Te-to (aref tebuf tenum)) To
			   (Te-koma (aref tebuf tenum)) (aref *board* a)
			   (Te-cap (aref tebuf tenum)) (aref *board* To)
			   (Te-utu (aref tebuf tenum)) 0
			   (Te-pro (aref tebuf tenum)) 0)
		     (incf tenum)
		     (if (and (< To 40) (= (aref *board* a) *SKA*))
			 (progn
			   (setf (Te-from (aref tebuf tenum)) a
				 (Te-to (aref tebuf tenum)) To
				 (Te-koma (aref tebuf tenum)) (aref *board* a)
				 (Te-cap (aref tebuf tenum)) (aref *board* To)
				 (Te-utu (aref tebuf tenum)) 0
				 (Te-pro (aref tebuf tenum)) 1)
			   (incf tenum)))))))
	      ((= (aref *board* a) *SKY*)
	       (do ((c 1 (1+ c)))
		   ((>= c 10))
		 (let ((To (+ a (* c -10))))
		   (cond
		     ((or (= (aref *board* To) *WAL*)
			  (and (> (aref *board* To) 0) (< (aref *board* To) 16)))
		      (return))
		     ((> (aref *board* To) 15)
		      (setf (Te-from (aref tebuf tenum)) a
			    (Te-to (aref tebuf tenum)) To
			    (Te-koma (aref tebuf tenum)) (aref *board* a)
			    (Te-cap (aref tebuf tenum)) (aref *board* To)
			    (Te-utu (aref tebuf tenum)) 0
			    (Te-pro (aref tebuf tenum)) 0)
		      (incf tenum)
		      (if (< To 40)
			  (progn
			    (setf (Te-from (aref tebuf tenum)) a
				  (Te-to (aref tebuf tenum)) To
				  (Te-koma (aref tebuf tenum)) (aref *board* a)
				  (Te-cap (aref tebuf tenum)) (aref *board* To)
				  (Te-utu (aref tebuf tenum)) 0
				  (Te-pro (aref tebuf tenum)) 1)
			    (incf tenum)))
		      (return)))
		   (setf (Te-from (aref tebuf tenum)) a
			 (Te-to (aref tebuf tenum)) To
			 (Te-koma (aref tebuf tenum)) (aref *board* a)
			 (Te-cap (aref tebuf tenum)) (aref *board* To)
			 (Te-utu (aref tebuf tenum)) 0
			 (Te-pro (aref tebuf tenum)) 0)
		   (incf tenum)
		   (if (< To 40)
		       (progn
			 (setf (Te-from (aref tebuf tenum)) a
			       (Te-to (aref tebuf tenum)) To
			       (Te-koma (aref tebuf tenum)) (aref *board* a)
			       (Te-cap (aref tebuf tenum)) (aref *board* To)
			       (Te-utu (aref tebuf tenum)) 0
			       (Te-pro (aref tebuf tenum)) 1)
			 (incf tenum)))))))
	    
	    (do ((b 0 (1+ b)))
		((>= b 12))
	      (tagbody 
		 (if (and (>= (aref *board* a) 0)
			   (= (aref *CanGo* b (aref *board* a)) 1))
		     (let ((To (+ a (aref *Direct* b))))
		       (if (or (= (aref *board* To) *WAL*)
			       (and (> (aref *board* To) 0) (< (aref *board* To) 16)))
			   (go hoge-tag))
		       (cond
			 ((or (= (aref *board* a) *SFU*) (= (aref *board* a) *SKY*))
			  (if (< To 40)
			      (if (< To 20)
				  (progn
				    (setf (Te-from (aref tebuf tenum)) a
					  (Te-to (aref tebuf tenum)) To
					  (Te-koma (aref tebuf tenum)) (aref *board* a)
					  (Te-cap (aref tebuf tenum)) (aref *board* To)
					  (Te-utu (aref tebuf tenum)) 0
					  (Te-pro (aref tebuf tenum)) 1)
				    (incf tenum)
				    (go hoge-tag))
				  (progn
				    (setf (Te-from (aref tebuf tenum)) a
					  (Te-to (aref tebuf tenum)) To
					  (Te-koma (aref tebuf tenum)) (aref *board* a)
					  (Te-cap (aref tebuf tenum)) (aref *board* To)
					  (Te-utu (aref tebuf tenum)) 0
					  (Te-pro (aref tebuf tenum)) 1)
				    (setf (Te-from (aref tebuf tenum)) a
					  (Te-to (aref tebuf tenum)) To
					  (Te-koma (aref tebuf tenum)) (aref *board* a)
					  (Te-cap (aref tebuf tenum)) (aref *board* To)
					  (Te-utu (aref tebuf tenum)) 0
					  (Te-pro (aref tebuf tenum)) 0)
				    (incf tenum)
				    (go hoge-tag)))
			      (progn
				(setf (Te-from (aref tebuf tenum)) a
				      (Te-to (aref tebuf tenum)) To
				      (Te-koma (aref tebuf tenum)) (aref *board* a)
				      (Te-cap (aref tebuf tenum)) (aref *board* To)
				      (Te-utu (aref tebuf tenum)) 0
				      (Te-pro (aref tebuf tenum)) 0)
				(incf tenum)
				(go hoge-tag))))
			 ((= (aref *board* a) *SKE*)
			  (if (< To 40)
			      (if (< To 30)
				  (progn
				    (setf (Te-from (aref tebuf tenum)) a
					  (Te-to (aref tebuf tenum)) To
					  (Te-koma (aref tebuf tenum)) (aref *board* a)
					  (Te-cap (aref tebuf tenum)) (aref *board* To)
					  (Te-utu (aref tebuf tenum)) 0
					  (Te-pro (aref tebuf tenum)) 1)
				    (incf tenum)
				    (go hoge-tag))
				  (progn
				    (setf (Te-from (aref tebuf tenum)) a
					  (Te-to (aref tebuf tenum)) To
					  (Te-koma (aref tebuf tenum)) (aref *board* a)
					  (Te-cap (aref tebuf tenum)) (aref *board* To)
					  (Te-utu (aref tebuf tenum)) 0
					  (Te-pro (aref tebuf tenum)) 1)
				    (incf tenum)
				    (setf (Te-from (aref tebuf tenum)) a
					  (Te-to (aref tebuf tenum)) To
					  (Te-koma (aref tebuf tenum)) (aref *board* a)
					  (Te-cap (aref tebuf tenum)) (aref *board* To)
					  (Te-utu (aref tebuf tenum)) 0
					  (Te-pro (aref tebuf tenum)) 0)
				    (incf tenum)
				    (go hoge-tag)))
			      (progn
				(setf (Te-from (aref tebuf tenum)) a
				      (Te-to (aref tebuf tenum)) To
				      (Te-koma (aref tebuf tenum)) (aref *board* a)
				      (Te-cap (aref tebuf tenum)) (aref *board* To)
				      (Te-utu (aref tebuf tenum)) 0
				      (Te-pro (aref tebuf tenum)) 0)
				(incf tenum)
				(go hoge-tag))))
			 (t
			  (if (< To 40)
			      (progn
				(setf (Te-from (aref tebuf tenum)) a ;; non promote
					 (Te-to (aref tebuf tenum)) To
					 (Te-koma (aref tebuf tenum)) (aref *board* a)
					 (Te-cap (aref tebuf tenum)) (aref *board* To)
					 (Te-utu (aref tebuf tenum)) 0
					 (Te-pro (aref tebuf tenum)) 0)
				   (incf tenum)
				   (if (= (aref *CanPromote* (aref *board* a)) 1)
				       (progn
					 (setf (Te-from (aref tebuf tenum)) a ;;promote
					       (Te-to (aref tebuf tenum)) To
					       (Te-koma (aref tebuf tenum)) (aref *board* a)
					       (Te-cap (aref tebuf tenum)) (aref *board* To)
					       (Te-utu (aref tebuf tenum)) 0
					       (Te-pro (aref tebuf tenum)) 1)
					 (incf tenum)
					 (go hoge-tag))))
				 (progn
				   (setf (Te-from (aref tebuf tenum)) a ;; non promote
					 (Te-to (aref tebuf tenum)) To
					 (Te-koma (aref tebuf tenum)) (aref *board* a)
					 (Te-cap (aref tebuf tenum)) (aref *board* To)
					 (Te-utu (aref tebuf tenum)) 0
					 (Te-pro (aref tebuf tenum)) 0)
				   (incf tenum)
				   (go hoge-tag)))))))
		 hoge-tag)))))
      (do ((a 0 (1+ a)))
	  ((>= a 111))
	(cond
	  ((> (aref *board* a) 15)
	   (cond
	     ((or (= (aref *board* a) *EHI*) (= (aref *board* a) *ERY*))
	      (do ((b 0 (1+ b)))
		  ((>= b 4))
		(do ((c 1 (1+ c)))
		    ((>= c 10))
		  (let ((To (+ a (* (aref *h_Direct* b) c))))
		    (cond
		      ((or (= (aref *board* To) *WAL*) (> (aref *board* To) 15))
		       (return))
		      ((and (< (aref *board* To) 16) (> (aref *board* To) 0))
		       (setf (Te-from (aref tebuf tenum)) a
			     (Te-to (aref tebuf tenum)) To
			     (Te-koma (aref tebuf tenum)) (aref *board* a)
			     (Te-cap (aref tebuf tenum)) (aref *board* To)
			     (Te-utu (aref tebuf tenum)) 0
			     (Te-pro (aref tebuf tenum)) 0)
		       (incf tenum)
		       (if (and (> To 69) (= (aref *board* a) *EHI*))
			   (progn
			     (setf (Te-from (aref tebuf tenum)) a
				   (Te-to (aref tebuf tenum)) To
				   (Te-koma (aref tebuf tenum)) (aref *board* a)
				   (Te-cap (aref tebuf tenum)) (aref *board* To)
				   (Te-utu (aref tebuf tenum)) 0
				   (Te-pro (aref tebuf tenum)) 1)
			     (incf tenum)))
		       (return)))
		    (setf (Te-from (aref tebuf tenum)) a
			  (Te-to (aref tebuf tenum)) To
			  (Te-koma (aref tebuf tenum)) (aref *board* a)
			  (Te-cap (aref tebuf tenum)) (aref *board* To)
			  (Te-utu (aref tebuf tenum)) 0
			  (Te-pro (aref tebuf tenum)) 0)
		    (incf tenum)
		    (if (and (> To 69) (= (aref *board* a) *EHI*))
			(progn
			  (setf (Te-from (aref tebuf tenum)) a
				(Te-to (aref tebuf tenum)) To
				(Te-koma (aref tebuf tenum)) (aref *board* a)
				(Te-cap (aref tebuf tenum)) (aref *board* To)
				(Te-utu (aref tebuf tenum)) 0
				(Te-pro (aref tebuf tenum)) 1)
			  (incf tenum)))))))
	     ((or (= (aref *board* a) *EKA*) (= (aref *board* a) *EUM*))
	      (do ((b 0 (1+ b)))
		  ((>= b 4))
		(do ((c 1 (1+ c)))
		    ((>= c 10))
		  (let ((To (+ a (* (aref *k_Direct* b) c))))
		    (cond
		      ((or (= (aref *board* To) *WAL*) (> (aref *board* To) 15))
		       (return))
		      ((and (< (aref *board* To) 16) (> (aref *board* To) 0))
		       (setf (Te-from (aref tebuf tenum)) a
			     (Te-to (aref tebuf tenum)) To
			     (Te-koma (aref tebuf tenum)) (aref *board* a)
			     (Te-cap (aref tebuf tenum)) (aref *board* To)
			     (Te-utu (aref tebuf tenum)) 0
			     (Te-pro (aref tebuf tenum)) 0)
		       (incf tenum)
		       (if (and (> To 69) (= (aref *board* a) *EKA*))
			   (progn
			     (setf (Te-from (aref tebuf tenum)) a
				   (Te-to (aref tebuf tenum)) To
				   (Te-koma (aref tebuf tenum)) (aref *board* a)
				   (Te-cap (aref tebuf tenum)) (aref *board* To)
				   (Te-utu (aref tebuf tenum)) 0
				   (Te-pro (aref tebuf tenum)) 1)
			     (incf tenum)))
		       (return)))
		    (setf (Te-from (aref tebuf tenum)) a
			  (Te-to (aref tebuf tenum)) To
			  (Te-koma (aref tebuf tenum)) (aref *board* a)
			  (Te-cap (aref tebuf tenum)) (aref *board* To)
			  (Te-utu (aref tebuf tenum)) 0
			  (Te-pro (aref tebuf tenum)) 0)
		    (incf tenum)
		    (if (and (> To 69) (= (aref *board* a) *EKA*))
			(progn
			  (setf (Te-from (aref tebuf tenum)) a
				(Te-to (aref tebuf tenum)) To
				(Te-koma (aref tebuf tenum)) (aref *board* a)
				(Te-cap (aref tebuf tenum)) (aref *board* To)
				(Te-utu (aref tebuf tenum)) 0
				(Te-pro (aref tebuf tenum)) 1)
			  (incf tenum)))))))
	     ((= (aref *board* a) *EKY*)
	      (do ((c 1 (1+ c)))
		  ((>= c 10))
		(let ((To (+ a (* c 10))))
		  (cond
		    ((or (= (aref *board* To) *WAL*) (> (aref *board* To) 15))
		     (return))
		    ((and (< (aref *board* To) 16) (> (aref *board* To) 0))
		     (setf (Te-from (aref tebuf tenum)) a
			   (Te-to (aref tebuf tenum)) To
			   (Te-koma (aref tebuf tenum)) (aref *board* a)
			   (Te-cap (aref tebuf tenum)) (aref *board* To)
			   (Te-utu (aref tebuf tenum)) 0
			   (Te-pro (aref tebuf tenum)) 0)
		     (incf tenum)
		     (if (> To 69)
			 (progn
			   (setf (Te-from (aref tebuf tenum)) a
				 (Te-to (aref tebuf tenum)) To
				 (Te-koma (aref tebuf tenum)) (aref *board* a)
				 (Te-cap (aref tebuf tenum)) (aref *board* To)
				 (Te-utu (aref tebuf tenum)) 0
				 (Te-pro (aref tebuf tenum)) 1)
			   (incf tenum)))
		     (return)))
		  (setf (Te-from (aref tebuf tenum)) a
			(Te-to (aref tebuf tenum)) To
			(Te-koma (aref tebuf tenum)) (aref *board* a)
			(Te-cap (aref tebuf tenum)) (aref *board* To)
			(Te-utu (aref tebuf tenum)) 0
			(Te-pro (aref tebuf tenum)) 0)
		  (incf tenum)
		  (if (> To 69)
		      (progn
			(setf (Te-from (aref tebuf tenum)) a
			      (Te-to (aref tebuf tenum)) To
			      (Te-koma (aref tebuf tenum)) (aref *board* a)
			      (Te-cap (aref tebuf tenum)) (aref *board* To)
			      (Te-utu (aref tebuf tenum)) 0
			      (Te-pro (aref tebuf tenum)) 1)
			(incf tenum)))))))
	      ;;(do ((b 0 (1+ b)))
	   ;;  ((>= b 12))
	   (dotimes (bb 12)
		(tagbody
		   (if (and (>= (aref *board* a) 0)
			    (= (aref *CanGo* bb (aref *board* a)) 1))
		       (let ((To (+ a (aref *Direct* bb))))
			 (if (or (> To 110) (= (aref *board* To) *WAL*) (> (aref *board* To) 15))
			     (go moge-tag))
			 (cond
			   ((or (= (aref *board* a) *EFU*) (= (aref *board* a) *EKY*))
			     (if (> To 69)
				 (if (> To 89)
				     (progn
				       (setf (Te-from (aref tebuf tenum)) a
					     (Te-to (aref tebuf tenum)) To
					     (Te-koma (aref tebuf tenum)) (aref *board* a)
					     (Te-cap (aref tebuf tenum)) (aref *board* To)
					     (Te-utu (aref tebuf tenum)) 0
					     (Te-pro (aref tebuf tenum)) 1)
				       (incf tenum)
				       (go moge-tag))
				     (progn
				       (setf (Te-from (aref tebuf tenum)) a
					     (Te-to (aref tebuf tenum)) To
					     (Te-koma (aref tebuf tenum)) (aref *board* a)
					     (Te-cap (aref tebuf tenum)) (aref *board* To)
					     (Te-utu (aref tebuf tenum)) 0
					     (Te-pro (aref tebuf tenum)) 1)
				       (incf tenum)
				       (setf (Te-from (aref tebuf tenum)) a
					     (Te-to (aref tebuf tenum)) To
					     (Te-koma (aref tebuf tenum)) (aref *board* a)
					     (Te-cap (aref tebuf tenum)) (aref *board* To)
					     (Te-utu (aref tebuf tenum)) 0
					     (Te-pro (aref tebuf tenum)) 0)
				       (incf tenum)
				       (go moge-tag)))
				 (progn
				   (setf (Te-from (aref tebuf tenum)) a
					 (Te-to (aref tebuf tenum)) To
					 (Te-koma (aref tebuf tenum)) (aref *board* a)
					 (Te-cap (aref tebuf tenum)) (aref *board* To)
					 (Te-utu (aref tebuf tenum)) 0
					 (Te-pro (aref tebuf tenum)) 0)
				   (incf tenum)
				   (go moge-tag))))
			   ((= (aref *board* a) *EKE*)
			    (if (> To 69)
				(if (> To 79)
				    (progn
				      (setf (Te-from (aref tebuf tenum)) a
					    (Te-to (aref tebuf tenum)) To
					    (Te-koma (aref tebuf tenum)) (aref *board* a)
					    (Te-cap (aref tebuf tenum)) (aref *board* To)
					    (Te-utu (aref tebuf tenum)) 0
					    (Te-pro (aref tebuf tenum)) 1)
				      (incf tenum)
				      (go moge-tag))
				    (progn
				      (setf (Te-from (aref tebuf tenum)) a
					    (Te-to (aref tebuf tenum)) To
					    (Te-koma (aref tebuf tenum)) (aref *board* a)
					    (Te-cap (aref tebuf tenum)) (aref *board* To)
					    (Te-utu (aref tebuf tenum)) 0
					    (Te-pro (aref tebuf tenum)) 1)
				      (incf tenum)
				      (setf (Te-from (aref tebuf tenum)) a
					    (Te-to (aref tebuf tenum)) To
					    (Te-koma (aref tebuf tenum)) (aref *board* a)
					    (Te-cap (aref tebuf tenum)) (aref *board* To)
					    (Te-utu (aref tebuf tenum)) 0
					    (Te-pro (aref tebuf tenum)) 0)
				      (incf tenum)
				      (go moge-tag)))
				(progn
				  (setf (Te-from (aref tebuf tenum)) a
					(Te-to (aref tebuf tenum)) To
					(Te-koma (aref tebuf tenum)) (aref *board* a)
					(Te-cap (aref tebuf tenum)) (aref *board* To)
					(Te-utu (aref tebuf tenum)) 0
					(Te-pro (aref tebuf tenum)) 0)
				  (incf tenum)
				  (go moge-tag))))
			   (t
			    (if (> To 69)
				(progn
				  (setf (Te-from (aref tebuf tenum)) a
					(Te-to (aref tebuf tenum)) To
					(Te-koma (aref tebuf tenum)) (aref *board* a)
					(Te-cap (aref tebuf tenum)) (aref *board* To)
					(Te-utu (aref tebuf tenum)) 0
					(Te-pro (aref tebuf tenum)) 0)
				  (incf tenum)
				  (if (= (aref *CanPromote* (aref *board* a)) 1)
				      (progn
					(setf (Te-from (aref tebuf tenum)) a
					      (Te-to (aref tebuf tenum)) To
					      (Te-koma (aref tebuf tenum)) (aref *board* a)
					      (Te-cap (aref tebuf tenum)) (aref *board* To)
					      (Te-utu (aref tebuf tenum)) 0
					      (Te-pro (aref tebuf tenum)) 1)
					(incf tenum)
					(go moge-tag))))
				(progn
				  (setf (Te-from (aref tebuf tenum)) a
					(Te-to (aref tebuf tenum)) To
					(Te-koma (aref tebuf tenum)) (aref *board* a)
					(Te-cap (aref tebuf tenum)) (aref *board* To)
					(Te-utu (aref tebuf tenum)) 0
					(Te-pro (aref tebuf tenum)) 0)
				  (incf tenum)
				  (go moge-tag)))))))
		   moge-tag))))))
  (list tenum tebuf))
		    
;;------------指し手生成部分（駒を打つ手）----------------------------------------------------------------
(defun gen_utu (turn tenum tebuf)
  (if (= turn 0)
      (progn
	(if (> (aref *hand* 0 1) 0)
	    (do ((a 1 (1+ a)))
		((>= a 10))
	      (let ((c 0))
		(do ((b 2 (1+ b))) ;;その筋に歩がないかのチェック（二歩防止）
		    ((>= b 10))
		  (if (= (aref *board* (+ a (* b 10))) *SFU*)
		      (progn
			(incf c)
			(return))))
		(if (= c 0)
		    (do ((b 2 (1+ b)))
			((>= b 10))
		      (if (= (aref *board* (+ a (* b 10))) *EMP*)
			  (progn
			    (setf (Te-from (aref tebuf tenum)) 0
				  (Te-koma (aref tebuf tenum)) 1
				  (Te-to (aref tebuf tenum)) (+ a (* b 10))
				  (Te-cap (aref tebuf tenum)) 0
				  (Te-utu (aref tebuf tenum)) 1
				  (Te-pro (aref tebuf tenum)) 0)
			    (incf tenum))))))))
	(if (> (aref *hand* 0 2) 0)
	    (do ((a 20 (1+ a)))
		((>= a 110))
		 (if (= (aref *board* a) *EMP*)
		     (progn
		       (setf (Te-from (aref tebuf tenum)) 0
			     (Te-koma (aref tebuf tenum)) 2
			     (Te-to (aref tebuf tenum)) a
			     (Te-cap (aref tebuf tenum)) 0
			     (Te-utu (aref tebuf tenum)) 1
			     (Te-pro (aref tebuf tenum)) 0)
		       (incf tenum)))))
		     ;;(progn (incf a)
			;;    (go 1-tag))))))
	(if (> (aref *hand* 0 3) 0)
	    (do ((a 30 (1+ a)))
		((>= a 110))
		 (if (= (aref *board* a) *EMP*)
		     (progn
		       (setf (Te-from (aref tebuf tenum)) 0
			     (Te-koma (aref tebuf tenum)) 3
			     (Te-to (aref tebuf tenum)) a
			     (Te-cap (aref tebuf tenum)) 0
			     (Te-utu (aref tebuf tenum)) 1
			     (Te-pro (aref tebuf tenum)) 0)
		       (incf tenum)))))
		     ;;(progn (incf a)
		;;	    (go 2-tag))))))
	(do ((a 4 (1+ a)))
	    ((>= a 8))
	  (if (> (aref *hand* 0 a) 0)
	      (do ((b 0 (1+ b)))
		  ((>= b 110))
		   (if (= (aref *board* b) *EMP*)
		       (progn
			 (setf (Te-from (aref tebuf tenum)) 0 
			       (Te-koma (aref tebuf tenum)) a
			       (Te-to (aref tebuf tenum)) b
			       (Te-cap (aref tebuf tenum)) 0
			       (Te-utu (aref tebuf tenum)) 1
			       (Te-pro (aref tebuf tenum)) 0)
			 (incf tenum)))))))
		       ;;(progn (incf b)
			 ;;     (go 3-tag))))))))
      (progn
	(if (> (aref *hand* 1 1) 0)
	    (do ((a 1 (1+ a)))
		((>= a 10))
	      (let ((c 0))
		(do ((b 1 (1+ b)))
		    ((>= b 9))
		  (if (= (aref *board* (+ a (* b 10))) *EFU*)
		      (progn (incf c)
			     (return))))
		(if (= c 0)
		    (do ((b 1 (1+ b)))
			((>= b 9))
		      (if (= (aref *board* (+ a (* b 10))) *EMP*)
			  (progn
			    (setf (Te-from (aref tebuf tenum)) 0 
				  (Te-koma (aref tebuf tenum)) 1
				  (Te-to (aref tebuf tenum)) (+ a (* b 10))
				  (Te-cap (aref tebuf tenum)) 0
				  (Te-utu (aref tebuf tenum)) 1
				  (Te-pro (aref tebuf tenum)) 0)
			    (incf tenum))))))))
		    ;;(progn (incf a)
		;;	   (go 4-tag)))))))
	(if (> (aref *hand* 1 2) 0)
	    (do ((a 20 (1+ a)))
		((>= a 110))
	      (if (= (aref *board* a) *EMP*)
		  (progn
		    (setf (Te-from (aref tebuf tenum)) 0 
			  (Te-koma (aref tebuf tenum)) 2
			  (Te-to (aref tebuf tenum)) a
			  (Te-cap (aref tebuf tenum)) 0
			  (Te-utu (aref tebuf tenum)) 1
			  (Te-pro (aref tebuf tenum)) 0)
		    (incf tenum)))))
		  ;;(progn (incf a)
		;;	 (go 5-tag))))))
	(if (> (aref *hand* 1 3) 0)
	    (do ((a 30 (1+ a)))
		((>= a 110))
	      (if (= (aref *board* a) *EMP*)
		  (progn
		    (setf (Te-from (aref tebuf tenum)) 0 
			  (Te-koma (aref tebuf tenum)) 3
			  (Te-to (aref tebuf tenum)) a
			  (Te-cap (aref tebuf tenum)) 0
			  (Te-utu (aref tebuf tenum)) 1
			  (Te-pro (aref tebuf tenum)) 0)
		    (incf tenum)))))
		  ;;(progn (incf a)
		;;	 (go 6-tag))))))
	(do ((a 4 (1+ a)))
	    ((>= a 8))
	  (if (> (aref *hand* 1 a) 0)
	      (do ((b 0 (1+ b)))
		  ((>= b 110))
		   (if (= (aref *board* b) *EMP*)
		       (progn
			 (setf (Te-from (aref tebuf tenum)) 0 
			       (Te-koma (aref tebuf tenum)) a
			       (Te-to (aref tebuf tenum)) b
			       (Te-cap (aref tebuf tenum)) 0
			       (Te-utu (aref tebuf tenum)) 1
			       (Te-pro (aref tebuf tenum)) 0)
			 (incf tenum))))))))
		       ;;(progn (incf b)
			 ;;     (go 7-tag)))))))))
  (list tenum tebuf))

;;1手進める
(defun Move (turn te1)
  (let ((get1 0))
  (if (= (Te-utu te1) 1)
      (progn
	(setf (aref *board* (Te-to te1)) (Te-koma te1))
	(if (= turn 1)
	    (setf (aref *board* (Te-to te1)) (+ (aref *board* (Te-to te1)) 15)))
	(decf (aref *hand* turn (Te-koma te1))))
      (if (= turn 0)
	  (progn
	    (if (/= (Te-cap te1) 0)
		(progn
		  (setf get1 (- (Te-cap te1) 15))
		  ;;(print turn)
		  ;;(print get1)
		  (if (> (Te-cap te1) 23)
			(setf get1 (- get1 8)))
		  (incf (aref *hand* 0 get1))))
	    (setf (aref *board* (Te-from te1)) *EMP*
		  (aref *board* (Te-to te1)) (Te-koma te1))
	    (if (= (Te-pro te1) 1)
		(setf (aref *board* (Te-to te1)) (+ (aref *board* (Te-to te1)) 8))))
	  (progn
	    (if (/= (Te-cap te1) 0)
		(progn
		  (setf get1 (Te-cap te1))
		  ;;(print (* get1 10))
		  (if (> (te-cap te1) 8)
		      (setf get1 (- get1 8)))
		  (incf (aref *hand* 1 get1))))
	    (setf (aref *board* (Te-from te1)) *EMP*
		  (aref *board* (te-to te1)) (Te-koma te1))
	    (if (= (Te-pro te1) 1)
		(setf (aref *board* (Te-to te1)) (+ (aref *board* (Te-to te1)) 8))))))))

;;一手戻る
(defun undo (turn te1)
  (let ((get1 0))
    (if (= (te-utu te1) 1)
	(progn (setf (aref *board* (Te-to te1)) *EMP*)
	       (incf (aref *hand* turn (Te-koma te1))))
	(progn
	  (setf (aref *board* (Te-to te1)) (Te-cap te1)
		(aref *board* (Te-from te1)) (Te-koma te1))
	  (if (/= (Te-cap te1) 0)
	      (if (= turn 0)
		  (progn
		    (setf get1 (- (Te-cap te1) 15))
		    (if (> (Te-cap te1) 23)
			(setf get1 (- get1 8)))
		    (decf (aref *hand* 0 get1)))
		  (progn
		    (setf get1 (Te-cap te1))
		    (if (> (Te-cap te1) 8)
			(setf get1 (- get1 8)))
		    (decf (aref *hand* 1 get1)))))))))
	    
;;王手放置のチェック
(defun check (turn)

  (let* ((tenum-v 0)
	 (tebuf-v (make-array 600))
	 (tenum 0)
	 (tebuf 0)
	 (hoge 0))
    (dotimes (i 600)
      (setf (aref tebuf-v i) (make-Te)))
    (setf hoge (gen_move turn tenum-v tebuf-v))
    (setf tenum (car hoge)
	  tebuf (cadr hoge))
    (do ((i 0 (1+ i)))
	((>= i tenum))
      (if (or (= (Te-cap (aref tebuf i)) *SOU*) (= (Te-cap (aref tebuf i)) *EOU*))
	  (return-from check 2))) ;
    0))

;;関数名はAlphaBetaになっているが内容はNegaAlpha
;;うまくうごかない。
(defun AlphaBeta (turn depth maxdepth alpha beta bestte)
  (if (= (check turn) 2)
      (return-from AlphaBeta (list *INFINITE*  bestte)))
  (if (= depth maxdepth) ;;深さが規定値に達した
      (return-from AlphaBeta (list (n-eval turn)  bestte)))
  
  (let* ((tenum 0) ;;探索手の数（自殺手も含む）
	 (tebuf (make-array 600)) ;;探索手
	 (hoge 0)
	 (retval (* *INFINITE* -1))
	 (neweval 0)
	 (chiledbest (make-Te)))
    (dotimes (i 600)
      (setf (aref tebuf i) (make-Te)))
    (setf hoge (gen_utu turn tenum tebuf));;打つ手の生成
    (setf tenum (car hoge)
	  tebuf (cadr hoge))
    (setf hoge (gen_move turn tenum tebuf));;動かす手の生成
    (setf tenum (car hoge)
	  tebuf (cadr hoge))
    (if (= tenum 0)
	(return-from AlphaBeta (list (* *INFINITE* -1)  bestte))) ;;指し手がない（Noviceの書き方だと必要ない？）
    (do ((i 0 (1+ i)))
	((>= i tenum ))
      (move turn (aref tebuf i)) ;;一手進める
      (setf neweval  (* (car (AlphaBeta (mod (+ turn 1) 2) (+ depth 1) maxdepth
				  (* beta -1) (* (max1 alpha retval) -1) chiledbest)) -1))
      (undo turn (aref tebuf i)) ;;一手戻す
      ;;(format t "~d ~d~%" tenum *neweval*)
      (if (> neweval retval)
	    (setf bestte (aref tebuf i) ;;評価値がBestなら指し手候補を更新
		  retval neweval))
      (if (>= retval beta)
	  (return-from AlphaBeta (list retval  bestte))))
    (list retval bestte)))

;;---------------------------------------------------------------------
;;ver2.0
(defun AlphaBeta-kai (turn depth maxdepth alpha beta bestte)
  (if (= (check turn) 2)
      (return-from AlphaBeta-kai (list *INFINITE*  bestte)))
  (if (>= depth maxdepth) ;;深さが規定値に達した
      (return-from AlphaBeta-kai (list (n-eval turn)  bestte)))
  
  (let* ((tenum 0) ;;探索手の数（自殺手も含む）
	 (tebuf (make-array 600)) ;;探索手
	 (hoge 0) (maxx 0) (maxid 0)
	 (retval (* *INFINITE* -1))
	 (neweval 0) (value (make-array 600))
	 (chiledbest (make-Te)))
    (dotimes (i 600)
      (setf (aref tebuf i) (make-Te)))
    (setf hoge (gen_utu turn tenum tebuf));;打つ手の生成
    (setf tenum (car hoge)
	  tebuf (cadr hoge))
    (setf hoge (gen_move turn tenum tebuf));;動かす手の生成
    (setf tenum (car hoge)
	  tebuf (cadr hoge))
    (if (= tenum 0)
	(return-from AlphaBeta-kai (list (* *INFINITE* -1)  bestte))) ;;指し手がない（Noviceの書き方だと必要ない？）
    ;;指し手を評価
    (do ((i 0 (1+ i)))
	((>= i tenum))
      (move turn (aref tebuf i))
      (setf (aref value i) (n-eval turn))
      (if (= turn 1)
	  (setf (aref value i) (* (aref value i) -1)))
      (undo turn (aref tebuf i)))
    ;;指した後の評価の高い順に手をソートする
    (do ((i 0 (1+ i)))
	((>= i (- tenum 1)))
      (setf maxx (aref value i)
	    maxid i)
      (do ((j (+ i 1) (1+ j)))
	  ((>= j tenum))
	(if (> (aref value j) maxx)
	    (setf maxx (aref value j)
		  maxid j)))
      (swap value i maxid)
      (swap tebuf i maxid))
    
    (do ((i 0 (1+ i)))
	((>= i tenum ))
      (move turn (aref tebuf i)) ;;一手進める
      (setf neweval  (* (car (AlphaBeta-kai (mod (+ turn 1) 2) (+ depth 1) maxdepth
				  (* beta -1) (* (max1 alpha retval) -1) chiledbest)) -1))
      (undo turn (aref tebuf i)) ;;一手戻す
      ;;(format t "~d ~d~%" tenum *neweval*)
      (if (> neweval retval)
	    (setf bestte (aref tebuf i) ;;評価値がBestなら指し手候補を更新
		  retval neweval))
      (if (>= retval beta)
	  (return-from AlphaBeta-kai (list retval  bestte))))
    (list retval bestte)))



(defun nega-alphabeta (turn alpha beta depth depthmax best)
  (if (= (check turn) 2)
      (return-from nega-alphabeta (list *INFInite* (aref best 0 0))))
  (if (>= depth depthmax)
      (if (= turn 0)
	  (return-from nega-alphabeta (list (n-eval turn) (aref best 0 0)))
	  (return-from nega-alphabeta (list (* (n-eval turn) -1) (aref best 0 0)))))
  (let* ((tenum 0) ;;探索手の数（自殺手も含む）
	 (tebuf (make-array 600)) ;;探索手
	 (hoge 0)
	 (retval (- (* *INFINITE* -1) 1))
	 (v 0))
    (dotimes (i 600)
      (setf (aref tebuf i) (make-Te)))
    (setf hoge (gen_utu turn tenum tebuf));;打つ手の生成
    (setf tenum (car hoge)
	  tebuf (cadr hoge))
    (setf hoge (gen_move turn tenum tebuf));;動かす手の生成
    (setf tenum (car hoge)
	  tebuf (cadr hoge))
    (if (= tenum 0)
	(return-from nega-alphabeta (list (* *INFINITE* -1) (aref best 0 0))))
    (do ((i 0 (1+ i)))
	((>= i tenum ))
      (move turn (aref tebuf i))
      (setf v (* (car (nega-alphabeta (mod (+ turn 1) 2) (* beta -1) (* alpha -1) (+ depth 1) depthmax best)) -1))
      ;;(undo turn (aref tebuf i))
      (if (> v retval)
	  (progn
	    (setf retval v
		  (aref best depth depth) (aref tebuf i))
	    (do ((i (+ depth 1) (1+ i)))
		((>= i depthmax ))
	      (setf (aref best depth i) (aref best (+ depth 1) i)))
	    (if (> retval alpha)
		(setf alpha retval))
	    (if (>= retval beta)
		(return-from nega-alphabeta (list retval (aref best 0 0)))))))
   (list retval (aref best 0 0))))
	  
;;-------------------------------------------------------------------------
(defun sikou-think (turn)
  (let* ((tenum 0) ;;探索手の数（自殺手も含む）
	 (tebuf (make-array 600)) ;;探索手
	 (hoge 0)
	 (best 0)
	 (kkvalue 0)
	 (bestval 0))
    (dotimes (i 600)
      (setf (aref tebuf i) (make-Te)))
    (setf hoge (gen_move turn tenum tebuf));;打つ手の生成
    (setf tenum (car hoge)
	  tebuf (cadr hoge))
    (if (= turn 0)
	(setf bestval -1000000)
	(setf bestval 1000000))
    (do ((i 0 (1+ i)))
	((> i (- tenum 1)))
      (move turn (aref tebuf i))
      (setf kkvalue (n-eval turn))
      (if (and (= turn 0) (<= bestval kkvalue))
	  (setf bestval kkvalue
		best i))
      (if (and (= turn 1) (> bestval kkvalue))
	  (setf bestval kkvalue
		best i)))
    (aref tebuf best)))
    
;;--------------------------------------------------------------------------------------------------
;;ランダムで差し手を選ぶ
(defun random-te (turn)
  (let* ((tenum 0)
	 (tebuf (make-array 600))
	 (hoge 0))
    (dotimes (i 600)
      (setf (aref tebuf i) (make-Te)))
    (setf hoge (gen_utu turn tenum tebuf))
    (setf tenum (car hoge)
	  tebuf (cadr hoge))
    (setf hoge (gen_move turn tenum tebuf))
    (setf tenum (car hoge)
	  tebuf (cadr hoge))
    (if (= tenum 0)
	0
	(aref tebuf (random tenum)))))

;;------------------------------------------------------------------------------
(defun deb-test ()
  (setf *tebuf* (make-array 600)
	*tenum* 0
	*score* 0
	*bestte* (make-Te))
  (dotimes (i 600)
    (setf (aref *tebuf* i) (make-Te)))
  (dotimes (i *DepthMax*)
    (dotimes (h *DepthMax*)
      (setf (aref *best* h i) (make-Te))))
  (init_board))
