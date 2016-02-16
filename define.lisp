(defvar *EMP* 0)
(defvar *black* 1)
(defvar *white* 0)
;;壁
(defvar *WAL* -1)
;;味方駒
(defvar *SFU* 1)
(defvar *SKY* 2)
(defvar *SKE* 3)
(defvar *SGI* 4)
(defvar *SKI* 5)
(defvar *SKA* 6)
(defvar *SHI* 7)
(defvar *SOU* 8)
;;味方成駒
(defvar *STO* 9)
(defvar *SNY* 10)
(defvar *SNE* 11)
(defvar *SNG* 12)
(defvar *SNK* 13)
(defvar *SUM* 14)
(defvar *SRY* 15)
;;敵駒
(defvar *EFU* 16)
(defvar *EKY* 17)
(defvar *EKE* 18)
(defvar *EGI* 19)
(defvar *EKI* 20)
(defvar *EKA* 21)
(defvar *EHI* 22)
(defvar *EOU* 23)
;;敵成駒
(defvar *ETO* 24)
(defvar *ENY* 25)
(defvar *ENE* 26)
(defvar *ENG* 27)
(defvar *ENK* 28)
(defvar *EUM* 29)
(defvar *ERY* 30)
;;探索深さの限界
(defvar *DepthMax* 3)
;;ある深さでの最善手順をしまう配列
(defvar *Best* (make-array (list *depthmax* *depthmax*)))
;;指し手内容の構造体
(defstruct Te
  From ;;どこから
  To   ;;どこに
  Cap  ;;取った駒(ないなら0)
  Koma ;;動かす駒
  Utu  ;;駒を打つか否か
  Pro) ;;成るか否か

(defvar *INFINITE* 99999999)
