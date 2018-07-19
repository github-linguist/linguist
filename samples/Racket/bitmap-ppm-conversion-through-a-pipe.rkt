(define (ppm->jpeg bitmap [jpg-file "output"] [quality 75])
  (define command (format "convert ppm:- -quality ~a jpg:~a.jpg" quality jpg-file))
  (match-define (list in out pid err ctrl)  (process command))
  (bitmap->ppm bitmap out)
  (close-input-port in)
  (close-output-port out))

(ppm->jpeg bm)
