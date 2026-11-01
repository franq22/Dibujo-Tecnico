;;; ============================================================
;;; DESPIECE RALLADOR DE NUEZ MOSCADA  (v2)
;;; Comando: DESPIECE
;;; Medidas aprox. en mm (90 x 40 x 35): ajustalas con tu pieza real
;;; ============================================================

;; ---------- basicas 2D ----------
(defun fr:pt (x y) (list (float x) (float y) 0.0))

(defun fr:layer (name col lt)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0) (cons 62 col) (cons 6 lt)))))

(defun fr:ltype ()
  (if (not (tblsearch "LTYPE" "DASHED"))
    (progn
      (vl-catch-all-apply
        '(lambda () (command "._-LINETYPE" "_L" "DASHED" "acadiso.lin" "")))
      (while (> (getvar "CMDACTIVE") 0) (command))))
  (if (tblsearch "LTYPE" "DASHED") "DASHED" "Continuous"))

(defun fr:line (x1 y1 x2 y2 lay)
  (entmake (list '(0 . "LINE") (cons 8 lay)
                 (cons 10 (fr:pt x1 y1)) (cons 11 (fr:pt x2 y2)))))

(defun fr:poly (pts lay closed)
  (entmake (append
             (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay)
                   '(100 . "AcDbPolyline") (cons 90 (length pts))
                   (cons 70 (if closed 1 0)))
             (mapcar '(lambda (p) (cons 10 (list (float (car p)) (float (cadr p))))) pts))))

(defun fr:rect (x1 y1 x2 y2 lay)
  (fr:poly (list (list x1 y1) (list x2 y1) (list x2 y2) (list x1 y2)) lay T))

(defun fr:circle (x y r lay)
  (entmake (list '(0 . "CIRCLE") (cons 8 lay)
                 (cons 10 (fr:pt x y)) (cons 40 (float r)))))

(defun fr:text (x y txt h lay)
  (entmake (list '(0 . "TEXT") (cons 8 lay)
                 (cons 10 (fr:pt x y)) (cons 40 (float h))
                 (cons 1 txt) '(50 . 0.0))))

;; rectangulo redondeado con ARCOS reales (bulge = tan(22.5 grados))
(defun fr:rrb (cx cy l w r lay / hx hy b vs)
  (setq hx (- (/ l 2.0) r) hy (- (/ w 2.0) r) b 0.414214)
  (setq vs (list
    (list (- cx hx) (- cy (/ w 2.0)) 0.0)
    (list (+ cx hx) (- cy (/ w 2.0)) b)
    (list (+ cx (/ l 2.0)) (- cy hy) 0.0)
    (list (+ cx (/ l 2.0)) (+ cy hy) b)
    (list (+ cx hx) (+ cy (/ w 2.0)) 0.0)
    (list (- cx hx) (+ cy (/ w 2.0)) b)
    (list (- cx (/ l 2.0)) (+ cy hy) 0.0)
    (list (- cx (/ l 2.0)) (- cy hy) b)))
  (entmake (append
    (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay)
          '(100 . "AcDbPolyline") (cons 90 8) '(70 . 1))
    (apply 'append
      (mapcar '(lambda (v) (list (cons 10 (list (car v) (cadr v))) (cons 42 (caddr v))))
              vs)))))

;; ---------- generacion de puntos (para la isometria) ----------
(defun fr:arcpts (cx cy r a0 a1 n / k res a)
  (setq k 0 res nil)
  (while (<= k n)
    (setq a (* (/ pi 180.0) (+ a0 (* (- a1 a0) (/ k (float n))))))
    (setq res (cons (list (+ cx (* r (cos a))) (+ cy (* r (sin a)))) res))
    (setq k (1+ k)))
  (reverse res))

(defun fr:circpts (cx cy r n / k res a)
  (setq k 0 res nil)
  (while (< k n)
    (setq a (* 2.0 pi (/ k (float n))))
    (setq res (cons (list (+ cx (* r (cos a))) (+ cy (* r (sin a)))) res))
    (setq k (1+ k)))
  (reverse res))

;; rect. redondeado = lista de 4 esquinas, en sentido antihorario
;; esquina 0 = (+x,-y), 1 = (+x,+y), 2 = (-x,+y), 3 = (-x,-y)
(defun fr:rrc (cx cy l w r n / hx hy)
  (setq hx (- (/ l 2.0) r) hy (- (/ w 2.0) r))
  (list (fr:arcpts (+ cx hx) (- cy hy) r -90 0 n)
        (fr:arcpts (+ cx hx) (+ cy hy) r 0 90 n)
        (fr:arcpts (- cx hx) (+ cy hy) r 90 180 n)
        (fr:arcpts (- cx hx) (- cy hy) r 180 270 n)))

(defun fr:rrpts (cx cy l w r n) (apply 'append (fr:rrc cx cy l w r n)))

;; ---------- proyeccion isometrica ----------
;; eje x -> (0.866, 0.5) ; eje y -> (-0.866, 0.5) ; eje z -> (0, 1)
(defun fr:iso (x y z)
  (list (+ *fr-ix* (* (- x y) 0.866025))
        (+ *fr-iy* z (* (+ x y) 0.5))
        0.0))

(defun fr:ipoly3 (pts lay closed)
  (fr:poly (mapcar (function (lambda (p) (fr:iso (car p) (cadr p) (caddr p)))) pts)
           lay closed))

(defun fr:ipoly (pts z lay closed)
  (fr:ipoly3 (mapcar (function (lambda (p) (list (car p) (cadr p) z))) pts)
             lay closed))

(defun fr:iline (x1 y1 z1 x2 y2 z2 lay / a b)
  (setq a (fr:iso x1 y1 z1) b (fr:iso x2 y2 z2))
  (fr:line (car a) (cadr a) (car b) (cadr b) lay))

(defun fr:icirc (cx cy z r lay)
  (fr:ipoly (fr:circpts cx cy r (if (> r 3) 24 8)) z lay T))

;; caja: solo las 3 caras visibles (arriba, frente -y, izquierda -x)
(defun fr:ibox (x1 y1 z1 x2 y2 z2 lay)
  (fr:ipoly3 (list (list x1 y1 z2) (list x2 y1 z2) (list x2 y2 z2) (list x1 y2 z2)) lay T)
  (fr:ipoly3 (list (list x1 y1 z1) (list x2 y1 z1) (list x2 y1 z2) (list x1 y1 z2)) lay T)
  (fr:ipoly3 (list (list x1 y1 z1) (list x1 y2 z1) (list x1 y2 z2) (list x1 y1 z2)) lay T))

;; globo con numero y linea de referencia
(defun fr:balloon (bx by n lx ly)
  (fr:circle bx by 5 "TEXTO")
  (fr:text (- bx 1.6) (- by 2.0) n 4 "TEXTO")
  (fr:line lx ly (- bx 5) by "TEXTO"))

;; ================= COMANDO PRINCIPAL =================
(defun c:DESPIECE ( / oldecho lt ox cy1 cy2 cy3 cb ct ci cp cl
                      i j off xt xb p1 p2 hp p bt zp zl)
  (setq oldecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; capas
  (setq lt (fr:ltype))
  (fr:layer "CONTORNO" 7 "Continuous")
  (fr:layer "DETALLE"  3 "Continuous")
  (fr:layer "TEXTO"    2 "Continuous")
  (fr:layer "EJE"      1 lt)
  (fr:layer "OCULTO"   8 lt)

  ;; ============ 1. ISOMETRICA EXPLOTADA ============
  (setq *fr-ix* 0.0 *fr-iy* 0.0)
  (setq zp 105 zl 175)

  ;; ---- POS 1: CUERPO BASE (z = 0 a 35) ----
  (setq cb (fr:rrc 0 0 80 32 5 6)
        ct (fr:rrc 0 0 90 40 6 6)
        ci (fr:rrc 0 0 84 34 3 6))
  (fr:ipoly (apply 'append ct) 35 "CONTORNO" T)
  (fr:ipoly (apply 'append ci) 35 "DETALLE" T)
  (fr:ipoly (append (nth 2 cb) (nth 3 cb) (nth 0 cb)) 0 "CONTORNO" nil)
  (foreach i '(2 3 0)
    (setq p1 (nth 3 (nth i cb)) p2 (nth 3 (nth i ct)))
    (fr:iline (car p1) (cadr p1) 0 (car p2) (cadr p2) 35 "CONTORNO"))
  (foreach xt '(-20 -14 14 20)
    (setq xb (* xt (/ 40.0 45.0)))
    (fr:iline xb -16 0 xt -20 35 "DETALLE"))
  (fr:ibox -12 13.5 20 -6 16 26 "DETALLE")
  (fr:ibox   6 13.5 20 12 16 26 "DETALLE")

  ;; ---- POS 2: PLACA RALLADORA (z = zp a zp+3) ----
  (setq cp (fr:rrc 0 0 90 40 6 6))
  (fr:ipoly (apply 'append cp) (+ zp 3) "CONTORNO" T)
  (fr:ipoly (append (nth 2 cp) (nth 3 cp) (nth 0 cp)) zp "CONTORNO" nil)
  (foreach i '(2 3 0)
    (setq p1 (nth 3 (nth i cp)))
    (fr:iline (car p1) (cadr p1) zp (car p1) (cadr p1) (+ zp 3) "CONTORNO"))
  (fr:ipoly (fr:rrpts -8 0 52 24 2 4) (+ zp 3) "DETALLE" T)
  (setq j 0)
  (repeat 5
    (setq off (if (= (rem j 2) 1) 2.75 0.0) i 0)
    (repeat (if (= (rem j 2) 1) 8 9)
      (fr:icirc (+ -30 off (* i 5.5)) (+ -8 (* j 4.0)) (+ zp 3) 1.1 "DETALLE")
      (setq i (1+ i)))
    (setq j (1+ j)))
  (fr:icirc 30 0 (+ zp 3) 9 "CONTORNO")
  (setq hp (fr:arcpts 30 0 9 -45 135 12))
  (fr:ipoly hp zp "CONTORNO" nil)
  (fr:iline (car (car hp)) (cadr (car hp)) zp
            (car (car hp)) (cadr (car hp)) (+ zp 3) "CONTORNO")
  (fr:iline (car (last hp)) (cadr (last hp)) zp
            (car (last hp)) (cadr (last hp)) (+ zp 3) "CONTORNO")

  ;; ---- POS 3: TAPA (z = zl a zl+5) ----
  (setq cl (fr:rrc 0 0 84 34 4 6))
  (fr:ipoly (apply 'append cp) (+ zl 3) "CONTORNO" T)
  (fr:ipoly (append (nth 2 cp) (nth 3 cp) (nth 0 cp)) zl "CONTORNO" nil)
  (foreach i '(2 3 0)
    (setq p1 (nth 3 (nth i cp)))
    (fr:iline (car p1) (cadr p1) zl (car p1) (cadr p1) (+ zl 3) "CONTORNO"))
  (fr:ipoly (apply 'append cl) (+ zl 5) "CONTORNO" T)
  (foreach i '(0 1 2 3)
    (setq p1 (nth 3 (nth i cp)) p2 (nth 3 (nth i cl)))
    (fr:iline (car p1) (cadr p1) (+ zl 3) (car p2) (cadr p2) (+ zl 5) "DETALLE"))
  (foreach p '((-35 -12) (35 -12) (-35 12) (35 12))
    (fr:icirc (car p) (cadr p) (+ zl 5) 2.5 "DETALLE"))

  ;; ---- lineas guia de despiece ----
  (foreach i '(2 3 0)
    (setq p1 (nth 3 (nth i ct)))
    (fr:iline (car p1) (cadr p1) 35 (car p1) (cadr p1) zp "EJE")
    (fr:iline (car p1) (cadr p1) (+ zp 3) (car p1) (cadr p1) zl "EJE"))

  ;; ---- globos y rotulos ----
  (fr:balloon 85  40 "1" 53  32)
  (fr:text 93  38 "POS 1: CUERPO BASE" 3.5 "TEXTO")
  (fr:balloon 85 128 "2" 53 120)
  (fr:text 93 126 "POS 2: PLACA RALLADORA" 3.5 "TEXTO")
  (fr:balloon 85 198 "3" 53 190)
  (fr:text 93 196 "POS 3: TAPA" 3.5 "TEXTO")
  (fr:text -50 -40 "VISTA ISOMETRICA EXPLOTADA" 5 "TEXTO")

  ;; ============ 2. PLANTAS ============
  (setq ox 260 cy1 20 cy2 75 cy3 130)

  ;; POS 1 planta
  (fr:rrb ox cy1 90 40 6 "CONTORNO")
  (fr:rrb ox cy1 84 34 3 "DETALLE")
  (fr:rrb ox cy1 80 32 5 "OCULTO")
  (fr:rrb ox cy1 74 26 2 "DETALLE")
  (fr:rect (+ ox -12) (+ cy1 14) (+ ox -6) (+ cy1 17) "DETALLE")
  (fr:rect (+ ox   6) (+ cy1 14) (+ ox 12) (+ cy1 17) "DETALLE")
  (fr:text (+ ox 55) (- cy1 2) "POS 1 (PLANTA)" 3.5 "TEXTO")

  ;; POS 2 planta
  (fr:rrb ox cy2 90 40 6 "CONTORNO")
  (fr:rrb ox cy2 84 34 4 "DETALLE")
  (fr:rrb (- ox 8) cy2 52 24 2 "DETALLE")
  (setq j 0)
  (repeat 5
    (setq off (if (= (rem j 2) 1) 2.75 0.0) i 0)
    (repeat (if (= (rem j 2) 1) 8 9)
      (fr:circle (+ ox -30 off (* i 5.5)) (+ cy2 -8 (* j 4.0)) 1.1 "DETALLE")
      (setq i (1+ i)))
    (setq j (1+ j)))
  (fr:circle (+ ox 30) cy2 9 "CONTORNO")
  (fr:text (+ ox 55) (- cy2 2) "POS 2 (PLANTA)" 3.5 "TEXTO")

  ;; POS 3 planta
  (fr:rrb ox cy3 90 40 6 "CONTORNO")
  (fr:rrb ox cy3 84 34 4 "DETALLE")
  (foreach p '((-35 -12) (35 -12) (-35 12) (35 12))
    (fr:circle (+ ox (car p)) (+ cy3 (cadr p)) 2.5 "DETALLE"))
  (fr:text (+ ox 55) (- cy3 2) "POS 3 (PLANTA)" 3.5 "TEXTO")

  ;; ============ 3. TABLA DE MATERIALES ============
  (setq bt -70)
  (fr:rect -60 (- bt 80) 120 bt "CONTORNO")
  (fr:line -60 (- bt 20) 120 (- bt 20) "CONTORNO")
  (fr:line -60 (- bt 40) 120 (- bt 40) "CONTORNO")
  (fr:line -60 (- bt 60) 120 (- bt 60) "CONTORNO")
  (fr:line -30 (- bt 80) -30 bt "CONTORNO")
  (fr:line  90 (- bt 80)  90 bt "CONTORNO")

  (fr:text -57 (- bt 15) "POS" 3.5 "TEXTO")
  (fr:text -27 (- bt 15) "DENOMINACION" 3.5 "TEXTO")
  (fr:text  93 (- bt 15) "CANT." 3.5 "TEXTO")
  (fr:text -52 (- bt 35) "1" 3 "TEXTO")
  (fr:text -27 (- bt 35) "Cuerpo base contenedor" 3 "TEXTO")
  (fr:text 100 (- bt 35) "1" 3 "TEXTO")
  (fr:text -52 (- bt 55) "2" 3 "TEXTO")
  (fr:text -27 (- bt 55) "Placa ralladora" 3 "TEXTO")
  (fr:text 100 (- bt 55) "1" 3 "TEXTO")
  (fr:text -52 (- bt 75) "3" 3 "TEXTO")
  (fr:text -27 (- bt 75) "Tapa superior transparente" 3 "TEXTO")
  (fr:text 100 (- bt 75) "1" 3 "TEXTO")

  (setvar "CMDECHO" oldecho)
  (command "._ZOOM" "_E")
  (princ "\n[OK] Despiece generado.")
  (princ)
)
(princ "\nEscribi DESPIECE para dibujar.")
(princ)
