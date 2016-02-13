(defparameter *komavalue*
  (make-array 31 :initial-contents
	      '(0 87 232 257 369 444 569 642 15000
		534 489 510 495 0 827 945
		-87 -232 -257 -369 -444 -569 -642 -15000
		-534 -489 -510 -495 0 -827 -945)))

(defun n-eval (turn)
  (let ((score 0))
    (do ((a 1 (1+ a)))
	((>= a 8))
      (if (> (aref *hand* 0 a) 0)
	  (setf score (+ score (* (aref *komavalue* a) (aref *hand* 0 a)))))
      (if (> (aref *hand* 1 a) 0)
	  (setf score (- score (* (aref *komavalue* a) (aref *hand* 1 a))))))
    (do ((a 11 (1+ a)))
	((>= a 110))
	 (if (> (aref *board* a) 0)
	     (setf score (+ score (aref *komavalue* (aref *board* a))))))
    (setf (aref *nodes* 0) (+ (aref *nodes* 0) 1))
    (if (= turn *black*)
	(* -1 score)
	score)))
