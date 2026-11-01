;;; ============================================================
;;; DESPIECE RALLADOR DE NUEZ MOSCADA  (v3)
;;; Comando: DESPIECE
;;; Medidas en mm: largo 100, ancho 40 (se cambian en la seccion MEDIDAS)
;;; Sin capas ni colores: todo se dibuja en la capa actual
;;; ============================================================

;; ---------- basicas 2D ----------
(defun fr:pt (x y) (list (float x) (float y) 0.0))

(defun fr:line (x1 y1 x2 y2)
  (entmake (list '(0 . "LINE")
                 (cons 10 (fr:pt x1 y1)) (cons 11 (fr:pt x2 y2)))))

(defun fr:poly (pts closed)
  (entmake (append
             (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                   '(100 . "AcDbPolyline") (cons 90 (length pts))
                   (cons 70 (if closed 1 0)))
             (mapcar '(lambda (p) (cons 10 (list (float (car p)) (float (cadr p))))) pts))))

(defun fr:rect (x1 y1 x2 y2)
  (fr:poly (list (list x1 y1) (list x2 y1) (list x2 y2) (list x1 y2)) T))

(defun fr:circle (x y r)
  (entmake (list '(0 . "CIRCLE")
                 (cons 10 (fr:pt x y)) (cons 40 (float r)))))

(defun fr:text (x y txt h)
  (entmake (list '(0 . "TEXT")
                 (cons 10 (fr:pt x y)) (cons 40 (float h))
                 (cons 1 txt) '(50 . 0.0))))

;; rectangulo redondeado con ARCOS reales (bulge = tan(22.5 grados))
(defun fr:rrb (cx cy l w r / hx hy b vs)
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
    (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
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

;; elementos a..b de una lista
(defun fr:sub (lst a b / res k)
  (setq k a res nil)
  (while (<= k b)
    (setq res (cons (nth k lst) res) k (1+ k)))
  (reverse res))

;; tramo FRONTAL visible del contorno (izquierda -> frente -> derecha)
(defun fr:front (c n / m)
  (setq m (/ n 2))
  (append (fr:sub (nth 2 c) m n) (nth 3 c) (fr:sub (nth 0 c) 0 m)))

;; tramo TRASERO visible (pared interior del fondo)
(defun fr:back (c n / m)
  (setq m (/ n 2))
  (append (fr:sub (nth 0 c) m n) (nth 1 c) (fr:sub (nth 2 c) 0 m)))

;; ---------- proyeccion isometrica ----------
;; eje x -> (0.866, 0.5) ; eje y -> (-0.866, 0.5) ; eje z -> (0, 1)
(defun fr:iso (x y z)
  (list (+ *fr-ix* (* (- x y) 0.866025))
        (+ *fr-iy* z (* (+ x y) 0.5))
        0.0))

(defun fr:ipoly3 (pts closed)
  (fr:poly (mapcar (function (lambda (p) (fr:iso (car p) (cadr p) (caddr p)))) pts)
           closed))

(defun fr:ipoly (pts z closed)
  (fr:ipoly3 (mapcar (function (lambda (p) (list (car p) (cadr p) z))) pts)
             closed))

(defun fr:iline (x1 y1 z1 x2 y2 z2 / a b)
  (setq a (fr:iso x1 y1 z1) b (fr:iso x2 y2 z2))
  (fr:line (car a) (cadr a) (car b) (cadr b)))

(defun fr:icirc (cx cy z r n)
  (fr:ipoly (fr:circpts cx cy r n) z T))

;; caja: solo las 3 caras visibles (arriba, frente -y, izquierda -x)
(defun fr:ibox (x1 y1 z1 x2 y2 z2)
  (fr:ipoly3 (list (list x1 y1 z2) (list x2 y1 z2) (list x2 y2 z2) (list x1 y2 z2)) T)
  (fr:ipoly3 (list (list x1 y1 z1) (list x2 y1 z1) (list x2 y1 z2) (list x1 y1 z2)) T)
  (fr:ipoly3 (list (list x1 y1 z1) (list x1 y2 z1) (list x1 y2 z2) (list x1 y1 z2)) T))

;; conito con agujero arriba: base (r1), borde superior (r2), agujero (r3), alto h
(defun fr:icono (cx cy z r1 r2 r3 h / k)
  (setq k 0.707107)
  (fr:ipoly (fr:arcpts cx cy r1 135 315 8) z nil)
  (fr:icirc cx cy (+ z h) r2 12)
  (fr:icirc cx cy (+ z h) r3 8)
  (fr:iline (+ cx (* k r1)) (- cy (* k r1)) z
            (+ cx (* k r2)) (- cy (* k r2)) (+ z h))
  (fr:iline (- cx (* k r1)) (+ cy (* k r1)) z
            (- cx (* k r2)) (+ cy (* k r2)) (+ z h)))

;; globo con numero y linea de referencia
(defun fr:balloon (bx by n lx ly)
  (fr:circle bx by 5)
  (fr:text (- bx 1.6) (- by 2.0) n 4)
  (fr:line lx ly (- bx 5) by))

;; ================= COMANDO PRINCIPAL =================
(defun c:DESPIECE ( / oldecho lg an al lgg ang cxg fx
                      ct cb ci cw cl p1 p2 hp i j k off xt xb p
                      zp zg zl ox cy1 cy2 cy3 cy4 bt rows rw)
  (setq oldecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; ===== MEDIDAS (mm) =====
  (setq lg 100.0 an 40.0 al 35.0)        ; largo, ancho y alto del cuerpo
  (setq lgg 56.0 ang 26.0 cxg -10.0)     ; ventana / lamina ralladora (largo, ancho, posicion en x)
  (setq fx (- (/ lg 2.0) 17.0))          ; centro del agujero para el dedo
  (setq zp (+ al 73.0) zg (+ zp 68.0) zl (+ zg 58.0))   ; alturas del despiece

  (setq *fr-ix* 0.0 *fr-iy* 0.0)

  (setq cb (fr:rrc 0 0 (- lg 10) (- an 8) 5 6)   ; fondo exterior del cuerpo
        ct (fr:rrc 0 0 lg an 6 6)                ; contorno exterior (cuerpo, placa, tapa)
        ci (fr:rrc 0 0 (- lg 6) (- an 6) 3 6)    ; borde interior del cuerpo
        cw (fr:rrc cxg 0 lgg ang 2 4)            ; ventana / lamina
        cl (fr:rrc 0 0 (- lg 6) (- an 6) 4 6))   ; cara superior de la tapa

  ;; ============ 1. ISOMETRICA EXPLOTADA ============

  ;; ---- POS 1: CUERPO BASE ----
  (fr:ipoly (apply 'append ct) al T)
  (fr:ipoly (apply 'append ci) al T)
  (fr:ipoly (fr:front cb 6) 0 nil)
  (foreach i '(2 3 0)
    (setq p1 (nth 3 (nth i cb)) p2 (nth 3 (nth i ct)))
    (fr:iline (car p1) (cadr p1) 0 (car p2) (cadr p2) al))
  (foreach xt (list (* -0.22 lg) (* -0.16 lg) (* 0.16 lg) (* 0.22 lg))
    (setq xb (* xt (/ (- lg 10.0) lg)))
    (fr:iline xb (- (/ (- an 8.0) 2.0)) 0 xt (- (/ an 2.0)) al))
  (fr:ibox -12 (- (/ an 2.0) 6.5) 20 -6 (- (/ an 2.0) 4.0) 26)
  (fr:ibox   6 (- (/ an 2.0) 6.5) 20 12 (- (/ an 2.0) 4.0) 26)

  ;; ---- POS 2: PLACA CON VENTANA ----
  (fr:ipoly (apply 'append ct) (+ zp 3) T)
  (fr:ipoly (fr:front ct 6) zp nil)
  (foreach i '(2 3 0)
    (setq p1 (nth 3 (nth i ct)))
    (fr:iline (car p1) (cadr p1) zp (car p1) (cadr p1) (+ zp 3)))
  ;; ventana donde entra la lamina
  (fr:ipoly (apply 'append cw) (+ zp 3) T)
  (setq hp (fr:back cw 4))
  (fr:ipoly hp zp nil)
  (fr:iline (car (car hp)) (cadr (car hp)) zp
            (car (car hp)) (cadr (car hp)) (+ zp 3))
  (fr:iline (car (last hp)) (cadr (last hp)) zp
            (car (last hp)) (cadr (last hp)) (+ zp 3))
  ;; agujero circular para el dedo
  (fr:icirc fx 0 (+ zp 3) 9 24)
  (setq hp (fr:arcpts fx 0 9 -45 135 12))
  (fr:ipoly hp zp nil)
  (fr:iline (car (car hp)) (cadr (car hp)) zp
            (car (car hp)) (cadr (car hp)) (+ zp 3))
  (fr:iline (car (last hp)) (cadr (last hp)) zp
            (car (last hp)) (cadr (last hp)) (+ zp 3))

  ;; ---- POS 3: LAMINA RALLADORA (chapa de 1 mm con conitos) ----
  (fr:ipoly (apply 'append cw) (+ zg 1) T)
  (fr:ipoly (fr:front cw 4) zg nil)
  (foreach i '(2 3 0)
    (setq p1 (nth 2 (nth i cw)))
    (fr:iline (car p1) (cadr p1) zg (car p1) (cadr p1) (+ zg 1)))
  (setq j 0)
  (repeat 5
    (setq i 0
          off (if (= (rem j 2) 1) 2.75 0.0))
    (repeat (if (= (rem j 2) 1) 8 9)
      (fr:icono (+ cxg -22.0 off (* i 5.5)) (+ -8.8 (* j 4.4)) (+ zg 1) 1.5 0.9 0.5 2.0)
      (setq i (1+ i)))
    (setq j (1+ j)))

  ;; ---- POS 4: TAPA ----
  (fr:ipoly (apply 'append ct) (+ zl 3) T)
  (fr:ipoly (fr:front ct 6) zl nil)
  (foreach i '(2 3 0)
    (setq p1 (nth 3 (nth i ct)))
    (fr:iline (car p1) (cadr p1) zl (car p1) (cadr p1) (+ zl 3)))
  (fr:ipoly (apply 'append cl) (+ zl 5) T)
  (foreach i '(0 1 2 3)
    (setq p1 (nth 3 (nth i ct)) p2 (nth 3 (nth i cl)))
    (fr:iline (car p1) (cadr p1) (+ zl 3) (car p2) (cadr p2) (+ zl 5)))
  (foreach p (list (list (- 15.0 (/ lg 2.0)) (- 8.0 (/ an 2.0)))
                   (list (- (/ lg 2.0) 15.0) (- 8.0 (/ an 2.0)))
                   (list (- 15.0 (/ lg 2.0)) (- (/ an 2.0) 8.0))
                   (list (- (/ lg 2.0) 15.0) (- (/ an 2.0) 8.0)))
    (fr:icirc (car p) (cadr p) (+ zl 5) 2.5 12))

  ;; ---- lineas guia de despiece ----
  (foreach i '(2 3 0)                                    ; cuerpo -> placa
    (setq p1 (nth 3 (nth i ct)))
    (fr:iline (car p1) (cadr p1) al (car p1) (cadr p1) zp))
  (foreach i '(2 0)                                      ; placa -> tapa
    (setq p1 (nth 3 (nth i ct)))
    (fr:iline (car p1) (cadr p1) (+ zp 3) (car p1) (cadr p1) zl))
  (foreach i '(2 3 0)                                    ; ventana -> lamina
    (setq p1 (nth 2 (nth i cw)))
    (fr:iline (car p1) (cadr p1) (+ zp 3) (car p1) (cadr p1) zg))

  ;; ---- globos y rotulos ----
  (fr:balloon 90  44 "1" 58  35)
  (fr:text 98  42 "POS 1: CUERPO BASE" 3.5)
  (fr:balloon 90 134 "2" 58 126)
  (fr:text 98 132 "POS 2: PLACA CON VENTANA" 3.5)
  (fr:balloon 90 186 "3" 27 180)
  (fr:text 98 184 "POS 3: LAMINA RALLADORA" 3.5)
  (fr:balloon 90 258 "4" 58 252)
  (fr:text 98 256 "POS 4: TAPA" 3.5)
  (fr:text -50 -45 "VISTA ISOMETRICA EXPLOTADA" 5)

  ;; ============ 2. PLANTAS ============
  (setq ox 270 cy1 20 cy2 75 cy3 125 cy4 180)

  ;; POS 1 planta
  (fr:rrb ox cy1 lg an 6)
  (fr:rrb ox cy1 (- lg 6) (- an 6) 3)
  (fr:rrb ox cy1 (- lg 16) (- an 14) 2)
  (fr:rect (+ ox -12) (+ cy1 (- (/ an 2.0) 6.0)) (+ ox -6) (+ cy1 (- (/ an 2.0) 3.0)))
  (fr:rect (+ ox   6) (+ cy1 (- (/ an 2.0) 6.0)) (+ ox 12) (+ cy1 (- (/ an 2.0) 3.0)))
  (fr:text (+ ox (/ lg 2.0) 8) (- cy1 2) "POS 1 (PLANTA)" 3.5)

  ;; POS 2 planta
  (fr:rrb ox cy2 lg an 6)
  (fr:rrb ox cy2 (- lg 6) (- an 6) 4)
  (fr:rrb (+ ox cxg) cy2 lgg ang 2)
  (fr:circle (+ ox fx) cy2 9)
  (fr:text (+ ox (/ lg 2.0) 8) (- cy2 2) "POS 2 (PLANTA)" 3.5)

  ;; POS 3 planta (lamina con conitos)
  (fr:rrb (+ ox cxg) cy3 lgg ang 2)
  (setq j 0)
  (repeat 5
    (setq i 0
          off (if (= (rem j 2) 1) 2.75 0.0))
    (repeat (if (= (rem j 2) 1) 8 9)
      (fr:circle (+ ox cxg -22.0 off (* i 5.5)) (+ cy3 -8.8 (* j 4.4)) 1.5)
      (fr:circle (+ ox cxg -22.0 off (* i 5.5)) (+ cy3 -8.8 (* j 4.4)) 0.9)
      (fr:circle (+ ox cxg -22.0 off (* i 5.5)) (+ cy3 -8.8 (* j 4.4)) 0.5)
      (setq i (1+ i)))
    (setq j (1+ j)))
  (fr:text (+ ox (/ lg 2.0) 8) (- cy3 2) "POS 3 (PLANTA)" 3.5)

  ;; POS 4 planta
  (fr:rrb ox cy4 lg an 6)
  (fr:rrb ox cy4 (- lg 6) (- an 6) 4)
  (foreach p (list (list (- 15.0 (/ lg 2.0)) (- 8.0 (/ an 2.0)))
                   (list (- (/ lg 2.0) 15.0) (- 8.0 (/ an 2.0)))
                   (list (- 15.0 (/ lg 2.0)) (- (/ an 2.0) 8.0))
                   (list (- (/ lg 2.0) 15.0) (- (/ an 2.0) 8.0)))
    (fr:circle (+ ox (car p)) (+ cy4 (cadr p)) 2.5))
  (fr:text (+ ox (/ lg 2.0) 8) (- cy4 2) "POS 4 (PLANTA)" 3.5)

  ;; ============ 3. TABLA DE MATERIALES ============
  (setq bt -70)
  (fr:rect -60 (- bt 100) 120 bt)
  (fr:line -60 (- bt 20) 120 (- bt 20))
  (fr:line -60 (- bt 40) 120 (- bt 40))
  (fr:line -60 (- bt 60) 120 (- bt 60))
  (fr:line -60 (- bt 80) 120 (- bt 80))
  (fr:line -30 (- bt 100) -30 bt)
  (fr:line  90 (- bt 100)  90 bt)

  (fr:text -57 (- bt 15) "POS" 3.5)
  (fr:text -27 (- bt 15) "DENOMINACION" 3.5)
  (fr:text  93 (- bt 15) "CANT." 3.5)

  (setq rows '(("1" "Cuerpo base contenedor" "1")
               ("2" "Placa con ventana" "1")
               ("3" "Lamina ralladora (conitos)" "1")
               ("4" "Tapa superior transparente" "1"))
        k 1)
  (foreach rw rows
    (fr:text -52 (- bt (* k 20) 15) (car rw) 3)
    (fr:text -27 (- bt (* k 20) 15) (cadr rw) 3)
    (fr:text 100 (- bt (* k 20) 15) (caddr rw) 3)
    (setq k (1+ k)))

  (setvar "CMDECHO" oldecho)
  (command "._ZOOM" "_E")
  (princ "\n[OK] Despiece generado.")
  (princ)
)
(princ "\nEscribi DESPIECE para dibujar.")
(princ)
