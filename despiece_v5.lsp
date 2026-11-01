;;; ============================================================
;;; DESPIECE RALLADOR DE NUEZ MOSCADA  (v5)
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

;; polilinea cerrada con arcos: lista de vertices (x y bulge)
(defun fr:bpoly (vs)
  (entmake (append
    (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
          '(100 . "AcDbPolyline") (cons 90 (length vs)) '(70 . 1))
    (apply 'append
      (mapcar '(lambda (v) (list (cons 10 (list (car v) (cadr v))) (cons 42 (caddr v))))
              vs)))))

;; rectangulo redondeado con ARCOS reales (bulge = tan(22.5 grados))
(defun fr:rrb (cx cy l w r / hx hy b)
  (setq hx (- (/ l 2.0) r) hy (- (/ w 2.0) r) b 0.414214)
  (fr:bpoly (list
    (list (- cx hx) (- cy (/ w 2.0)) 0.0)
    (list (+ cx hx) (- cy (/ w 2.0)) b)
    (list (+ cx (/ l 2.0)) (- cy hy) 0.0)
    (list (+ cx (/ l 2.0)) (+ cy hy) b)
    (list (+ cx hx) (+ cy (/ w 2.0)) 0.0)
    (list (- cx hx) (+ cy (/ w 2.0)) b)
    (list (- cx (/ l 2.0)) (+ cy hy) 0.0)
    (list (- cx (/ l 2.0)) (- cy hy) b))))

;; contorno de la PLACA en planta: extremo izquierdo con esquinas redondeadas
;; y extremo derecho con DOS SEMICIRCULOS (bulge 1 = semicirculo)
(defun fr:plb (cx cy l w r / rl xe x0 yb yt b)
  (setq rl (/ w 4.0)
        xe (- (+ cx (/ l 2.0)) rl)
        x0 (- cx (/ l 2.0))
        yb (- cy (/ w 2.0))
        yt (+ cy (/ w 2.0))
        b  0.414214)
  (fr:bpoly (list
    (list (+ x0 r) yb 0.0)
    (list xe yb 1.0)
    (list xe cy 1.0)
    (list xe yt 0.0)
    (list (+ x0 r) yt b)
    (list x0 (- yt r) 0.0)
    (list x0 (+ yb r) b))))

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

;; PLACA: lista de 4 tramos = (esq. inf-izq, semicirculo inferior,
;; semicirculo superior, esq. sup-izq)
(defun fr:plc (cx cy l w r n nl / rl xe)
  (setq rl (/ w 4.0)
        xe (- (+ cx (/ l 2.0)) rl))
  (list (fr:arcpts (+ (- cx (/ l 2.0)) r) (+ (- cy (/ w 2.0)) r) r 180 270 n)
        (fr:arcpts xe (- cy rl) rl -90 90 nl)
        (fr:arcpts xe (+ cy rl) rl -90 90 nl)
        (fr:arcpts (+ (- cx (/ l 2.0)) r) (- (+ cy (/ w 2.0)) r) r 90 180 n)))

(defun fr:plfull (pc)
  (append (nth 0 pc) (nth 1 pc) (cdr (nth 2 pc)) (nth 3 pc)))

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
(defun c:DESPIECE ( / oldecho lg an al lgg ang cxg fx rh
                      ct cb ci cw cl pc gl p1 p2 hp i j k off xt xb x0 yt yb p
                      zp zg zl ox cy1 cy2 cy3 cy4 bt rows rw
                      rb nn hf hw fl s x1 x2 zc yw ywc ycn cn hl)
  (setq oldecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; ===== MEDIDAS (mm) =====
  (setq lg 100.0 an 40.0 al 35.0)        ; largo, ancho y alto del cuerpo
  (setq lgg 56.0 ang 26.0 cxg -10.0)     ; ventana / lamina ralladora (largo, ancho, posicion en x)
  (setq rh 8.0)                          ; radio del agujero para el dedo
  (setq fx (- (/ lg 2.0) (/ an 4.0) 11.0)) ; centro del agujero (queda dentro del extremo de semicirculos)
  (setq zp (+ al 73.0) zg (+ zp 68.0) zl (+ zg 58.0))   ; alturas del despiece

  ;; interior del cuerpo (segun las fotos): nervaduras y piso
  (setq rb '((-29.5 -9.5) (3.0 18.5))    ; posicion en x de las 2 nervaduras por pared (x1 x2)
        nn 9.0                            ; semiancho libre del piso entre nervaduras
        hf 37.0                           ; semilargo del piso
        hw 13.0)                          ; semiancho del piso

  ;; conitos de la lamina medidos de la foto: (x y) respecto del centro de la lamina
  (setq cn '((-20.5 9.2) (-19.4 -9.4) (-18.0 6.8) (-17.7 -2.7) (-17.7 2.0) (-17.3 -7.6)
          (-15.9 9.1) (-15.8 4.7) (-15.6 -0.4) (-15.5 -5.2) (-15.1 -9.9) (-11.7 9.1)
          (-11.0 -0.2) (-10.7 -9.9) (-7.3 9.5) (-7.2 4.5) (-7.0 -5.2) (-6.9 0.0)
          (-6.7 -9.9) (-5.1 -2.4) (-4.9 2.0) (-4.9 6.2) (-4.8 -7.1) (-3.0 9.5)
          (-2.8 4.6) (-2.6 -4.9) (-2.4 -9.3) (-1.1 6.2) (-0.3 -7.1) (1.2 4.5)
          (1.3 9.6) (1.7 -4.7) (2.1 -9.4) (3.2 2.2) (3.7 -2.0) (3.9 -7.0)
          (5.6 9.8) (5.7 5.1) (5.8 -4.7) (5.8 0.2) (6.4 -9.1) (9.8 0.1)
          (9.8 9.9) (10.5 -9.1) (14.2 10.2) (14.4 5.3) (14.5 0.3) (14.7 -4.5)
          (14.9 -8.8) (16.5 7.5) (16.8 -2.1) (16.8 2.5) (16.9 -6.5) (18.6 10.4)
          (19.0 -9.0)))
  ;; agujeros lisos (sin cono) de la lamina
  (setq hl '((-10.5 -4.5) (-11.0 5.0) (-0.4 0.3) (10.4 -4.1) (10.3 5.4)))

  (setq *fr-ix* 0.0 *fr-iy* 0.0)

  (setq cb (fr:rrc 0 0 (- lg 10) (- an 8) 5 6)   ; fondo exterior del cuerpo
        ct (fr:rrc 0 0 lg an 6 6)                ; contorno exterior (cuerpo y tapa)
        ci (fr:rrc 0 0 (- lg 6) (- an 6) 3 6)    ; borde interior del cuerpo
        cw (fr:rrc cxg 0 lgg ang 2 4)            ; ventana / lamina
        cl (fr:rrc 0 0 (- lg 6) (- an 6) 4 6)    ; cara superior de la tapa
        pc (fr:plc 0 0 lg an 6 6 8))             ; contorno de la placa (extremo con 2 semicirculos)

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
  ;; nervaduras (cunas) de la pared trasera: nacen en el borde interior (z = al)
  ;; y llegan hasta el piso; lo mas bajo queda tapado por la pared frontal
  (setq yt (/ (- an 6.0) 2.0)             ; semiancho en el borde interior
        yw (/ (- an 14.0) 2.0)            ; semiancho de la pared a nivel del piso
        zc 8.0)                           ; altura desde la que se ve por encima de la pared frontal
  (foreach s rb
    (setq x1 (car s) x2 (cadr s)
          ywc (+ yw (* (- yt yw) (/ (- zc 3.0) (- al 3.0))))
          ycn (+ nn (* (- yt nn) (/ (- zc 3.0) (- al 3.0)))))
    (fr:ipoly3 (list (list x1 yt al) (list x2 yt al) (list x2 ycn zc) (list x1 ycn zc)) T)
    (fr:ipoly3 (list (list x1 yt al) (list x1 ywc zc) (list x1 ycn zc)) T))

  ;; ---- POS 2: PLACA CON VENTANA ----
  (fr:ipoly (fr:plfull pc) (+ zp 3) T)
  (fr:ipoly (append (fr:sub (nth 3 pc) 3 6) (nth 0 pc) (fr:sub (nth 1 pc) 0 2)) zp nil)
  (fr:ipoly (fr:sub (nth 2 pc) 0 2) zp nil)
  (foreach p1 (list (nth 3 (nth 3 pc)) (nth 3 (nth 0 pc))
                    (nth 2 (nth 1 pc)) (nth 2 (nth 2 pc)))
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
  (fr:icirc fx 0 (+ zp 3) rh 24)
  (setq hp (fr:arcpts fx 0 rh -45 135 12))
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
  (foreach p cn
    (fr:icono (+ cxg (car p)) (cadr p) (+ zg 1) 1.5 0.9 0.5 2.0))
  (foreach p hl
    (fr:icirc (+ cxg (car p)) (cadr p) (+ zg 1) 1.4 12))

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
  ;; puntos comunes a cuerpo, placa y tapa: izquierda, frente y derecha
  (setq gl (list (nth 3 (nth 2 ct))
                 (nth 3 (nth 3 ct))
                 (list (- (/ lg 2.0) (/ an 4.0)) (- (/ an 2.0)))))
  (foreach p1 gl                                         ; cuerpo -> placa
    (fr:iline (car p1) (cadr p1) al (car p1) (cadr p1) zp))
  (foreach p1 (list (car gl) (nth 2 gl))                 ; placa -> tapa
    (fr:iline (car p1) (cadr p1) (+ zp 3) (car p1) (cadr p1) zl))
  (foreach i '(2 3 0)                                    ; ventana -> lamina
    (setq p1 (nth 2 (nth i cw)))
    (fr:iline (car p1) (cadr p1) (+ zp 3) (car p1) (cadr p1) zg))

  ;; ---- globos y rotulos ----
  (fr:balloon 90  44 "1" 58  35)
  (fr:text 98  42 "POS 1: CUERPO BASE" 3.5)
  (fr:balloon 90 134 "2" 56 126)
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
  ;; contorno del piso: ancho completo con una muesca donde entra cada nervadura
  (setq fl (list (list (- hf) (- hw))))
  (foreach s rb
    (setq fl (append fl (list (list (car s) (- hw)) (list (car s) (- nn))
                              (list (cadr s) (- nn)) (list (cadr s) (- hw))))))
  (setq fl (append fl (list (list hf (- hw)) (list hf hw))))
  (foreach s (reverse rb)
    (setq fl (append fl (list (list (cadr s) hw) (list (cadr s) nn)
                              (list (car s) nn) (list (car s) hw)))))
  (setq fl (append fl (list (list (- hf) hw))))
  (fr:poly (mapcar (function (lambda (p) (list (+ ox (car p)) (+ cy1 (cadr p))))) fl) T)
  ;; nervaduras: 2 por pared, desde el borde interior (|y| = 17) hasta el piso (|y| = 9)
  (foreach s rb
    (fr:rect (+ ox (car s)) (+ cy1 nn) (+ ox (cadr s)) (+ cy1 (/ (- an 6.0) 2.0)))
    (fr:rect (+ ox (car s)) (- cy1 (/ (- an 6.0) 2.0)) (+ ox (cadr s)) (- cy1 nn)))
  (fr:text (+ ox (/ lg 2.0) 8) (- cy1 2) "POS 1 (PLANTA)" 3.5)

  ;; POS 2 planta (extremo derecho con dos semicirculos)
  (fr:plb ox cy2 lg an 6)
  (fr:plb ox cy2 (- lg 6) (- an 6) 4)
  (fr:rrb (+ ox cxg) cy2 lgg ang 2)
  (fr:circle (+ ox fx) cy2 rh)
  (fr:text (+ ox (/ lg 2.0) 8) (- cy2 2) "POS 2 (PLANTA)" 3.5)

  ;; POS 3 planta (lamina con conitos)
  (fr:rrb (+ ox cxg) cy3 lgg ang 2)
  (foreach p cn
    (fr:circle (+ ox cxg (car p)) (+ cy3 (cadr p)) 1.5)
    (fr:circle (+ ox cxg (car p)) (+ cy3 (cadr p)) 0.9)
    (fr:circle (+ ox cxg (car p)) (+ cy3 (cadr p)) 0.5))
  (foreach p hl
    (fr:circle (+ ox cxg (car p)) (+ cy3 (cadr p)) 1.4))
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
