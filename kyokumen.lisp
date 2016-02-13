(defparameter *board* (make-array 111))
(defparameter *hand* (make-array '(2 8)))
(defparameter *nodes* (make-array 1))

(defun init_board ()
  (let ((init_boards (make-array
		      111 :initial-contents
		      ;;壁有りの一次元配列
		      '(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1
			-1 17 18 19 20 23 20 19 18 17
			-1  0 22  0  0  0  0  0 21  0
			-1 16 16 16 16 16 16 16 16 16
			-1  0  0  0  0  0  0  0  0  0
			-1  0  0  0  0  0  0  0  0  0
			-1  0  0  0  0  0  0  0  0  0
			-1  1  1  1  1  1  1  1  1  1
			-1  0  6  0  0  0  0  0  7  0
			-1  2  3  4  5  8  5  4  3  2
			-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1))))
		      #|
		      '(*WAL* *WAL* *WAL* *WAL* *WAL* *WAL* *WAL* *WAL* *WAL* *WAL*
			*WAL* *EKY* *EKE* *EGI* *EKI* *EOU* *EKI* *EGI* *EKE* *EKY*
			*WAL* *EMP* *EHI* *EMP* *EMP* *EMP* *EMP* *EMP* *EKA* *EMP*
			*WAL* *EFU* *EFU* *EFU* *EFU* *EFU* *EFU* *EFU* *EFU* *EFU*
			*WAL* *EMP* *EMP* *EMP* *EMP* *EMP* *EMP* *EMP* *EMP* *EMP*
			*WAL* *EMP* *EMP* *EMP* *EMP* *EMP* *EMP* *EMP* *EMP* *EMP*
			*WAL* *EMP* *EMP* *EMP* *EMP* *EMP* *EMP* *EMP* *EMP* *EMP*
			*WAL* *SFU* *SFU* *SFU* *SFU* *SFU* *SFU* *SFU* *SFU* *SFU*
			*WAL* *EMP* *SKA* *EMP* *EMP* *EMP* *EMP* *EMP* *SHI* *EMP*
			*WAL* *SKY* *SKE* *SGI* *SKI* *SOU* *SKI* *SGI* *SKE* *SKY*
			*WAL* *WAL* *WAL* *WAL* *WAL* *WAL* *WAL* *WAL* *WAL* *WAL* *WAL*))))
		      |#
    ;;盤の初期化
    (do ((a 0 (1+ a)))
	((>= a 111))
      (setf (aref *board* a) (aref init_boards a)))
    (do ((a 0 (1+ a))) ;;持ち駒の初期化
	((>= a 2))
      (do ((b 0 (1+ b)))
	  ((>= b 8))
	(setf (aref *hand* a b) 0)))))
