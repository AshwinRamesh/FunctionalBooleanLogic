;;;; This is example queries and an example env

(setq query1 '(s-not (s-nand (s-var a) (s-var b))))
(setq query2 '(s-nand (s-var a) (s-var b)))
(setq query3 '(s-not (s-var a)))
(setq query4 '(s-var a))
(setq query5 '(s-nand (s-nand (s-var a) (s-var b)) (s-nand (s-var c) (s-var d))))
(setq query6 '(s-not (s-not (s-var a))))
(setq query7 '(s-not (s-not (s-not (s-not (s-var a))))))
(setq query8 '(s-not (s-nand (s-not (s-not (s-var a))) (s-var b))))
(setq query9 '(s-nand (s-var a) (s-var a)))
(setq query10 '(s-nand (s-nand (s-var a) (s-var b)) (s-nand (s-var a) (s-var b))))
(setq query11 '(s-not (s-nand (s-nand (s-var a) (s-var b)) (s-nand (s-var a) (s-var b)))))
(setq env '((a nil) (b T) (c T) (d nil)))
