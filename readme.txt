このプログラムはNovice_miniを非常に多くの点でパク.........参考にしています。
(URL: http://programroom.blog.fc2.com/blog-category-6.html)

まだ未完成で今のところ駒を動かせる指し手があればランダムで指すといった感じです。
反則のチェックもないのでバンバン反則負けします。

Clozure CLで

(ccl:save-application "hogehoge.exe"
                      :toplevel-function #'main
                      :prepend-kernel t)

でexeファイル作ったら将棋所で使えました。
sbclだとダメだった