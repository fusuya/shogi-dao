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
	  (format t "id name hagezou~%")
	  (format t "id author mogezou~%")
	  (format t "usiok~%"))
	 ((equal buf "isready")
	  (format t "readyok~%"))
	 ((equal buf "usinewgame")
	  (return))))))

(defun main ()
  (let ((turn 0))
    (usi)
    (loop
	 (let ((buf (ppcre:split #\space (read-line))))
	   (format t "~a~%" buf)
	   (cond
	     ((equal (car buf) "position")
	      (if (equal (cadr buf) "startpos")
		  (progn
		    (init_board)
		    ;;position startopos moves ~~~~
		    (setf turn (make_moves_from_usi (cdddr buf))))))
	     ((equal (car buf) "go")
	      (let ((bestte 0))
		#|
		(loop while (< maxdepth (+ *DepthMax* 1))
		     do
		     (setf (aref *nodes* 0) 0
			   score (AlphaBeta turn depth maxdepth alpha beta *bestte*))
		     (send_pv turn maxdepth *bestte* score)
		     (incf maxdepth))
		|#
		(setf bestte (random-te turn))
		(if (numberp bestte)
		    (progn (format t "bestmove resign~%")
			   (return)))
		(send_best turn bestte)))
	     ((equal (car buf) "quite")
	      (return)))))))
			   
		     
	    
