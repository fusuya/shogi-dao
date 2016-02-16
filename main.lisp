(ql:quickload :cl-ppcre)
(load "define.lisp" :external-format :utf-8)
(load "kyokumen.lisp" :external-format :utf-8)
(load "search.lisp" :external-format :utf-8)
(load "io.lisp" :external-format :utf-8)

(defun usi ()
  (loop
     (let ((buf (car (ppcre:split #\space (read-line)))))
       (cond
	 ((equal buf "usi")
	  (format t "id name hagezou2~%")
	  (format t "id author mogezou~%")
	  (format t "usiok~%"))
	 ((equal buf "isready")
	  (format t "readyok~%"))
	 ((equal buf "usinewgame")
	  (return))))))
;;*hand*増えすぎ問題
(defun main ()
  (let ((turn 0))
  (usi)
  (loop
     (let* ((buf (ppcre:split #\space (read-line)))
	    (sashite (cdddr buf))
	    (score 0)
	    (alpha (+ (* *INFINITE* -1) 1))
	    (beta (- *INFINITE* 1))
	    (depth 0) 
	    (maxdepth 1) (bestte (make-Te))
	    (best  (make-array (list *DepthMax* *DepthMax*)))
	    (hage 0))
       (format t "~a~%" buf)
       (cond
	 ((equal (car buf) "position")
	  (if (equal (cadr buf) "startpos")
	      (progn
		(init_board)
		;;buf = ("position" "startopos" "moves" "~~~~")
		(setf turn (turn_set sashite))
		(dotimes (i (length sashite))
		  (test_make_moves_from_usi (nth i sashite)
					    (mod i 2))))))
	 ((equal (car buf) "go")
	  
	  (loop while (< maxdepth (+ *DepthMax* 1))
	     do
	       (setf (aref *nodes* 0) 0
		     hage (AlphaBeta-kai turn depth maxdepth alpha beta
				     bestte))
	       (setf score (car hage)
		     bestte (cadr hage))
	       (send_pv turn maxdepth bestte score)
	       (incf maxdepth))
	  (if (= score (* *INFINITE* -1))
	      (progn
		(format t "bestmove resign~%")
		(return)))
	  (send_best turn bestte))
	  
	  #|
	  (let ((best (make-array (list *DepthMax* *DepthMax*))))
	    (setf (aref *nodes* 0) 0 
		  hage (nega-alphabeta turn alpha beta depth *DepthMax* best))
	    (setf score (car hage)
		  bestte (cadr hage))
	    (send_pv turn maxdepth bestte score)
	    (if (=  score (* *INFINITE* -1))
		(progn
		  (format t "bestmove resign~%")
		  (return)))
	    (send_best turn bestte)))
	 |#
	 ((equal (car buf) "quite")
	  (return)))))))


	    
