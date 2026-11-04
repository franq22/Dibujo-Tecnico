;;; ============================================================
;;; DESPIECE RALLADOR DE NUEZ MOSCADA  (v12)
;;; Comando: DESPIECE
;;; Medidas en mm: largo 100, ancho 40 (se cambian en la seccion MEDIDAS)
;;; Sin capas ni colores: todo se dibuja en la capa actual
;;; v11: nervaduras de 3 mm hacia adentro (antes 4), largo 13 mm;
;;;      semicirculo (mitad inferior) en la pared inclinada izquierda
;;; v12: lineas ocultas (tipo ACAD_ISO02W100) en las vistas de la base
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

;; ---------- lineas ocultas (discontinuas) ----------
;; tipo de linea ACAD_ISO02W100; la escala de cada linea esta en *fr-lts*
(setq *fr-hlt* "ACAD_ISO02W100" *fr-lts* 0.3)

(defun fr:hline (x1 y1 x2 y2)
  (entmake (list '(0 . "LINE") (cons 6 *fr-hlt*) (cons 48 *fr-lts*)
                 (cons 10 (fr:pt x1 y1)) (cons 11 (fr:pt x2 y2)))))

(defun fr:hpoly (pts closed)
  (entmake (append
             (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                   (cons 6 *fr-hlt*) (cons 48 *fr-lts*)
                   '(100 . "AcDbPolyline") (cons 90 (length pts))
                   (cons 70 (if closed 1 0)))
             (mapcar '(lambda (p) (cons 10 (list (float (car p)) (float (cadr p))))) pts))))

;; polilinea cerrada con arcos: lista de vertices (x y bulge)
(defun fr:bpoly (vs)
  (entmake (append
    (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
          '(100 . "AcDbPolyline") (cons 90 (length vs)) '(70 . 1))
    (apply 'append
      (mapcar '(lambda (v) (list (cons 10 (list (car v) (cadr v))) (cons 42 (caddr v))))
              vs)))))

;; polilinea ABIERTA con arcos: lista de vertices (x y bulge)
(defun fr:bopen (vs)
  (entmake (append
    (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
          '(100 . "AcDbPolyline") (cons 90 (length vs)) '(70 . 0))
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

;; x sobre la pared inclinada a una distancia d medida sobre la pared desde arriba
(defun fr:wallx (d xa xb lw)
  (+ xa (* (/ d lw) (- xb xa))))

;; globo con numero y linea de referencia
(defun fr:balloon (bx by n lx ly)
  (fr:circle bx by 5)
  (fr:text (- bx 1.6) (- by 2.0) n 4)
  (fr:line lx ly (- bx 5) by))

;; ---------- vistas laterales de la lamina ----------
;; altura de un conito (tronco de cono: radio r1 en la base, r2 arriba, alto h)
;; visto de costado, a una distancia d de su eje
(defun fr:cf (d r1 r2 h)
  (cond ((<= d r2) h)
        ((< d r1) (* h (/ (- r1 d) (- r1 r2))))
        (T 0.0)))

;; dibuja un tramo visible del contorno (lista de puntos (t z)) ampliado ke veces
(defun fr:runline (run vx vy ke ts)
  (if (> (length run) 1)
    (fr:poly (mapcar (function (lambda (p)
                       (list (+ vx (* ke (car p))) (+ vy (* ke (+ ts (cadr p)))))))
                     run)
             nil)))

;; vista de costado de la chapa y de los conitos. items = lista de (t prof):
;; t = posicion horizontal del cono en la vista, prof = distancia al observador
;; (menor = mas cerca). Cada cono se dibuja entero salvo lo que tapan los que
;; tiene adelante.
(defun fr:vista (items half vx vy ke ts title / it o tc dp tt z hid run r1 r2 h step)
  (setq r1 1.5 r2 0.9 h 2.0 step 0.05)
  (fr:rect (- vx (* ke half)) vy (+ vx (* ke half)) (+ vy (* ke ts)))
  (foreach it items
    (setq tc (car it) dp (cadr it) run nil tt (- tc r1))
    (while (<= tt (+ tc r1 0.000001))
      (setq z (fr:cf (abs (- tt tc)) r1 r2 h) hid nil)
      (foreach o items
        (if (and (not hid)
                 (< (cadr o) dp)
                 (< (abs (- tt (car o))) r1)
                 (>= (fr:cf (abs (- tt (car o))) r1 r2 h) (- z 0.0001)))
          (setq hid T)))
      (if hid
        (progn (fr:runline run vx vy ke ts) (setq run nil))
        (setq run (cons (list tt z) run)))
      (setq tt (+ tt step)))
    (fr:runline run vx vy ke ts))
  (fr:text (- vx (* ke half)) (- vy 12) title 3.5))

;; ---------- vistas de perfil del cuerpo (1:1) ----------
;; labio arriba (ancho hlp) y debajo la pared que se angosta de wt (arriba) a wb
;; (abajo), con las esquinas inferiores redondeadas. gl = lista de x de los
;; canales verticales (o nil)
(defun fr:vperfil (vx vy hlp wt wb al lh gl title / z0 r dxs len ux uz th bu tl g)
  (setq z0 (- al lh) r 3.0
        dxs (- wt wb)
        len (sqrt (+ (* dxs dxs) (* z0 z0)))
        ux (/ dxs len)
        uz (/ z0 len)
        th (- (/ pi 2.0) (atan dxs z0))
        bu (/ (sin (/ th 4.0)) (cos (/ th 4.0)))
        tl (* r (/ (sin (/ th 2.0)) (cos (/ th 2.0)))))
  (fr:rect (- vx hlp) (+ vy z0) (+ vx hlp) (+ vy al))
  (fr:bpoly (list
    (list (- vx wt) (+ vy z0) 0.0)
    (list (- vx wb (* tl ux)) (+ vy (* tl uz)) bu)
    (list (+ (- vx wb) tl) vy 0.0)
    (list (- (+ vx wb) tl) vy bu)
    (list (+ vx wb (* tl ux)) (+ vy (* tl uz)) 0.0)
    (list (+ vx wt) (+ vy z0) 0.0)))
  (foreach g gl
    (fr:line (+ vx g) (+ vy z0) (+ vx g) vy))
  (fr:text (- vx hlp) (- vy 12) title 3.5))

;; ================= COMANDO PRINCIPAL =================
(defun c:DESPIECE ( / oldecho lg an al lgg ang cxg fx rh ct cb ci cw cl pc gl p1 p2
                      hp i j k off xt xb x0 yt yb p zp zg zl ox cy1 cy2 cy3 cy4 bt
                      rows rw rb nn hf hw fl s x1 x2 zc yw ywc ycn cn hl ke ts vx lh
                      z0 kk yb1 yb2 ycl bx1 bx2 bz1 bz2 xf1 xf2 cwt ex ey sy dr pt
                      yd yp0 bw bh bx zb ypb ytl ybl k2 zc2 ypc hx3 hy3 xs cd hpd a1
                      a2 b1 b2 zt ypt fp1 fp2 ybo yo0 nch mr mdc xw0 xw1 lw mk mt mpts xa)
  (setq oldecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; cargar el tipo de linea de las lineas ocultas si no esta en el dibujo
  (if (not (tblsearch "LTYPE" *fr-hlt*))
    (command "._-LINETYPE" "_L" *fr-hlt* "acadiso.lin" ""))
  (if (not (tblsearch "LTYPE" *fr-hlt*))
    (command "._-LINETYPE" "_L" *fr-hlt* "acad.lin" ""))
  (if (not (tblsearch "LTYPE" *fr-hlt*))
    (princ (strcat "\nNo se pudo cargar el tipo de linea " *fr-hlt*)))

  ;; ===== MEDIDAS (mm) =====
  (setq lg 100.0 an 40.0 al 35.0)        ; largo, ancho y alto del cuerpo
  (setq lgg 56.0 ang 26.0 cxg -10.0)     ; ventana / lamina ralladora (largo, ancho, posicion en x)
  (setq rh 8.0)                          ; radio del agujero para el dedo
  (setq fx (- (/ lg 2.0) (/ an 4.0) 11.0)) ; centro del agujero (queda dentro del extremo de semicirculos)
  (setq zp (+ al 73.0) zg (+ zp 68.0) zl (+ zg 58.0))   ; alturas del despiece

  ;; semicirculo en la pared inclinada del extremo izquierdo (mitad de abajo del circulo)
  ;; mr = radio, mdc = distancia sobre la pared desde donde arranca la inclinacion
  ;; hasta el borde recto (diametro) del semicirculo; 0 = pegado arriba
  (setq mr 6.0 mdc 0.0)

  ;; cuerpo base (medido de las fotos): labio, caida recta, nervaduras, barra y piso
  (setq yt (/ (- an 6.0) 2.0)             ; semiancho del borde interior (segunda linea)
        lh 5.0                            ; alto del labio = largo de la caida recta
        dr 1.0                            ; la caida recta va 1 mm hacia adentro
        pt 3.0                            ; las nervaduras miden 3 mm hacia adentro
        hw 9.5                            ; semiancho del piso
        z0 (- al lh)                      ; altura donde termina la caida y arrancan las nervaduras
        yd (- yt dr)                      ; semiancho al pie de la caida
        yp0 (- yd pt)                     ; cara plana de la nervadura: llega hasta aca
        nn (- hw pt)                      ; la cara de la nervadura llega al piso a |y| = nn
        rb '((-20.5 -7.5) (11.5 24.5))    ; las 2 nervaduras por pared: x inicial y final (13 mm cada una)
        xf1 -25.0 xf2 39.0                ; el piso va de xf1 a xf2 (el extremo izquierdo es el embudo largo)
        bw 2.0 bh 10.0)                   ; barra: 2 mm de ancho y 10 mm de alto
  ;; la barra une las dos nervaduras de mas a la derecha (queda al medio de su largo)
  (setq bx (- (/ (+ (car (cadr rb)) (cadr (cadr rb))) 2.0) (/ bw 2.0)))
  (setq a1 (car (car rb)) a2 (cadr (car rb)) b1 (car (cadr rb)) b2 (cadr (cadr rb))
        hx3 (- (/ (- lg 8.0) 2.0) 2.0)    ; semilargo del pie de la caida (3ra linea)
        hy3 (/ (- an 8.0) 2.0)            ; semiancho del pie de la caida
        ybo (/ (- an 12.0) 2.0)           ; semiancho exterior en el fondo
        yo0 (/ (- an 4.0) 2.0))           ; semiancho exterior justo bajo el labio

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

  (setq cb (fr:rrc 0 0 (- lg 14) (- an 12) 4 6)  ; fondo exterior del cuerpo
        cwt (fr:rrc 0 0 (- lg 4) (- an 4) 5 6)   ; pared exterior justo bajo el labio
        cd (fr:rrc 0 0 (- lg 8) (- an 8) 2 6)    ; pie de la caida recta (1 mm hacia adentro)
        ct (fr:rrc 0 0 lg an 6 6)                ; contorno exterior (cuerpo y tapa)
        ci (fr:rrc 0 0 (- lg 6) (- an 6) 3 6)    ; borde interior del cuerpo
        cw (fr:rrc cxg 0 lgg ang 2 4)            ; ventana / lamina
        cl (fr:rrc 0 0 (- lg 6) (- an 6) 4 6)    ; cara superior de la tapa
        pc (fr:plc 0 0 lg an 6 6 8))             ; contorno de la placa (extremo con 2 semicirculos)

  ;; ============ 1. ISOMETRICA EXPLOTADA ============

  ;; ---- POS 1: CUERPO BASE ----
  ;; exterior: labio (z = al-lh .. al) y pared que baja y se angosta hasta el fondo
  (fr:ipoly (apply 'append ct) al T)                         ; borde exterior del labio
  (fr:ipoly (apply 'append ci) al T)                         ; escalon (segunda linea)
  (fr:ipoly (fr:front ct 6) (- al lh) nil)                   ; canto inferior del labio
  (foreach i '(2 3 0)
    (setq p1 (nth 3 (nth i ct)))
    (fr:iline (car p1) (cadr p1) (- al lh) (car p1) (cadr p1) al))
  ;; fondo exterior: el borde de adelante entra pt mm donde hay huecos (los rectangulos de adentro)
  (setq nch nil)
  (foreach s rb
    (setq nch (append nch (list (list (car s) (- ybo)) (list (car s) (- (- ybo pt)))
                                (list (cadr s) (- (- ybo pt))) (list (cadr s) (- ybo))))))
  (fr:ipoly (append (fr:sub (nth 2 cb) 3 6) (nth 3 cb) nch (fr:sub (nth 0 cb) 0 3)) 0 nil)
  (foreach i '(2 3 0)
    (setq p1 (nth 3 (nth i cb)) p2 (nth 3 (nth i cwt)))
    (fr:iline (car p1) (cadr p1) 0 (car p2) (cadr p2) (- al lh)))
  ;; huecos exteriores: por fuera se ve la forma de cada rectangulo de adentro (hasta el fondo)
  (foreach s rb
    (setq x1 (car s) x2 (cadr s))
    (fr:iline x1 (- ybo) 0 x1 (- yo0) z0)
    (fr:iline x2 (- ybo) 0 x2 (- yo0) z0)
    (fr:ipoly3 (list (list x2 (- yo0) z0) (list x2 (- (- yo0 pt)) z0)
                     (list x2 (- (- ybo pt)) 0) (list x2 (- ybo) 0)) T))
  ;; interior: caida recta de 5 mm, escalon de 1 mm hacia adentro y despues la pared inclinada con sus nervaduras
  (setq hp (fr:back ci 6) hpd (fr:back cd 6))
  (fr:ipoly hp z0 nil)                                       ; pie de la caida recta (tramo visible)
  (fr:iline (car (car hp)) (cadr (car hp)) al (car (car hp)) (cadr (car hp)) z0)
  (fr:iline (car (last hp)) (cadr (last hp)) al (car (last hp)) (cadr (last hp)) z0)
  ;; escalon de 1 mm: su borde interior se corta donde hay nervaduras (ahi se une con ellas)
  (fr:iline (car (car hp)) (cadr (car hp)) z0 (car (car hpd)) (cadr (car hpd)) z0)
  (fr:iline (car (last hp)) (cadr (last hp)) z0 (car (last hpd)) (cadr (last hpd)) z0)
  (fr:ipoly (append (fr:sub (nth 0 cd) 3 6) (nth 1 cd)) z0 nil)
  (fr:iline hx3 yd z0 b2 yd z0)
  (fr:iline b1 yd z0 a2 yd z0)
  (fr:iline a1 yd z0 (- hx3) yd z0)
  (fr:ipoly (fr:sub (nth 2 cd) 0 3) z0 nil)
  ;; lo mas bajo de la pared trasera queda tapado por la pared frontal: se dibuja desde zc2
  (setq k2 (/ (- yd hw) (- z0 3.0))                          ; pendiente de la pared (las nervaduras la siguen)
        zc2 (/ (+ (- (- al yt) yp0) (* k2 z0)) (+ 1.0 k2))
        ypc (- yp0 (* k2 (- z0 zc2)))                        ; cara de la nervadura a esa altura
        ywc (+ hw (* k2 (- zc2 3.0))))                       ; pared sin nervadura a esa altura
  (foreach s rb                                              ; nervaduras de la pared trasera
    (setq x1 (car s) x2 (cadr s))
    (fr:ipoly3 (list (list x1 yt z0) (list x2 yt z0) (list x2 yp0 z0) (list x1 yp0 z0)) T)
    (fr:ipoly3 (list (list x1 yp0 z0) (list x2 yp0 z0) (list x2 ypc zc2) (list x1 ypc zc2)) T)
    (fr:ipoly3 (list (list x1 yd z0) (list x1 yp0 z0) (list x1 ypc zc2) (list x1 ywc zc2)) T))
  ;; barra de 2 x 10 mm: arranca del piso (z = 3) y une las dos nervaduras de la derecha;
  ;; en isometrica casi toda queda tapada por la pared frontal
  (setq zb 3.0
        zt (+ zb bh)
        ypt (+ nn (* k2 (- zt 3.0)))                         ; cara de la nervadura a la altura del tope de la barra
        ytl (- (- al yt) zt))                                ; limite visible sobre la pared frontal
  (if (< ytl ypt)
    (progn
      (fr:ipoly3 (list (list bx ytl zt) (list (+ bx bw) ytl zt)
                       (list (+ bx bw) ypt zt) (list bx ypt zt)) T)
      (if (< zc2 zt)
        (fr:ipoly3 (list (list bx ypt zt) (list bx ytl zt) (list bx ypc zc2)) T))))
  ;; (el semicirculo esta en la pared izquierda, del lado cercano al observador: no se ve en la isometrica)

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

  ;; POS 1 planta (vista superior)
  (fr:rrb ox cy1 lg an 6)                       ; borde exterior del labio
  (fr:rrb ox cy1 (- lg 6) (- an 6) 3)           ; escalon (segunda linea)
  ;; tercera linea: pie de la caida recta (1 mm hacia adentro); se corta donde hay nervaduras
  (setq hx3 (- (/ (- lg 8.0) 2.0) 2.0)
        hy3 (/ (- an 8.0) 2.0)
        xs (list (- hx3) (car (car rb)) (cadr (car rb)) (car (cadr rb)) (cadr (cadr rb)) hx3))
  (foreach sy '(1.0 -1.0)
    (fr:line (+ ox (nth 0 xs)) (+ cy1 (* sy hy3)) (+ ox (nth 1 xs)) (+ cy1 (* sy hy3)))
    (fr:line (+ ox (nth 2 xs)) (+ cy1 (* sy hy3)) (+ ox (nth 3 xs)) (+ cy1 (* sy hy3)))
    (fr:line (+ ox (nth 4 xs)) (+ cy1 (* sy hy3)) (+ ox (nth 5 xs)) (+ cy1 (* sy hy3))))
  (fr:bopen (list (list (+ ox hx3) (- cy1 hy3) 0.414214)
                  (list (+ ox hx3 2.0) (- cy1 (- hy3 2.0)) 0.0)
                  (list (+ ox hx3 2.0) (+ cy1 (- hy3 2.0)) 0.414214)
                  (list (+ ox hx3) (+ cy1 hy3) 0.0)))
  (fr:bopen (list (list (- ox hx3) (+ cy1 hy3) 0.414214)
                  (list (- ox hx3 2.0) (+ cy1 (- hy3 2.0)) 0.0)
                  (list (- ox hx3 2.0) (- cy1 (- hy3 2.0)) 0.414214)
                  (list (- ox hx3) (- cy1 hy3) 0.0)))
  ;; contorno del piso en 2 tramos abiertos: se corta donde la barra se apoya en el piso
  (setq fp1 (list (list bx nn) (list b1 nn) (list b1 hw) (list a2 hw) (list a2 nn) (list a1 nn)
                  (list a1 hw) (list xf1 hw) (list xf1 (- hw)) (list a1 (- hw)) (list a1 (- nn))
                  (list a2 (- nn)) (list a2 (- hw)) (list b1 (- hw)) (list b1 (- nn)) (list bx (- nn)))
        fp2 (list (list (+ bx bw) (- nn)) (list b2 (- nn)) (list b2 (- hw)) (list xf2 (- hw))
                  (list xf2 hw) (list b2 hw) (list b2 nn) (list (+ bx bw) nn)))
  (fr:poly (mapcar (function (lambda (p) (list (+ ox (car p)) (+ cy1 (cadr p))))) fp1) nil)
  (fr:poly (mapcar (function (lambda (p) (list (+ ox (car p)) (+ cy1 (cadr p))))) fp2) nil)
  ;; esquinas inclinadas de los extremos (embudo): de la caida al piso
  (setq ex (+ hx3 1.414)
        ey (+ (- hy3 2.0) 1.414))
  (foreach sy '(1.0 -1.0)
    (fr:line (- ox ex) (+ cy1 (* sy ey)) (+ ox xf1) (+ cy1 (* sy hw)))
    (fr:line (+ ox ex) (+ cy1 (* sy ey)) (+ ox xf2) (+ cy1 (* sy hw))))
  ;; nervaduras: cara plana de 4 mm (desde la caida) y despues siguen la bajada hasta el piso
  (foreach sy '(1.0 -1.0)
    (foreach s rb
      (setq x1 (car s) x2 (cadr s))
      (fr:line (+ ox x1) (+ cy1 (* sy yp0)) (+ ox x2) (+ cy1 (* sy yp0)))
      (if (= x1 b1)
        (progn
          (fr:poly (list (list (+ ox x1) (+ cy1 (* sy yt))) (list (+ ox x1) (+ cy1 (* sy nn)))
                         (list (+ ox bx) (+ cy1 (* sy nn)))) nil)
          (fr:poly (list (list (+ ox bx bw) (+ cy1 (* sy nn))) (list (+ ox x2) (+ cy1 (* sy nn)))
                         (list (+ ox x2) (+ cy1 (* sy yt)))) nil))
        (fr:poly (list (list (+ ox x1) (+ cy1 (* sy yt))) (list (+ ox x1) (+ cy1 (* sy nn)))
                       (list (+ ox x2) (+ cy1 (* sy nn))) (list (+ ox x2) (+ cy1 (* sy yt)))) nil))))
  ;; barra de 2 mm que arranca del piso y une las dos nervaduras enfrentadas de la derecha
  (fr:rect (+ ox bx) (- cy1 ypt) (+ ox bx bw) (+ cy1 ypt))
  ;; semicirculo en la pared inclinada izquierda: borde recto arriba, arco hacia abajo
  (setq xw0 (- (+ hx3 2.0)) xw1 xf1
        lw (sqrt (+ (* (- xw1 xw0) (- xw1 xw0)) (* (- z0 3.0) (- z0 3.0))))
        mpts nil mk 0)
  (while (<= mk 16)
    (setq mt (* pi (/ mk 16.0)))
    (setq mpts (cons (list (+ ox (fr:wallx (+ mdc (* mr (sin mt))) xw0 xw1 lw))
                           (+ cy1 (* mr (cos mt))))
                     mpts))
    (setq mk (1+ mk)))
  (fr:poly (reverse mpts) T)
  ;; lineas ocultas en la planta: pared exterior bajo el labio y fondo (quedan tapados por el labio)
  (fr:hpoly (fr:rrpts ox cy1 (- lg 4.0) (- an 4.0) 5 6) T)
  (fr:hpoly (fr:rrpts ox cy1 (- lg 14.0) (- an 12.0) 4 6) T)
  (fr:text (+ ox (/ lg 2.0) 8) (- cy1 2) "POS 1 - VISTA SUPERIOR" 3.5)

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

  ;; ============ 2b. VISTAS LATERALES DE LA LAMINA (POS 3), AMPLIADAS 4:1 ============
  ;; se miran de costado: arriba la chapa de 1 mm y encima los conitos (base 3, alto 2)
  (setq ke 4.0 ts 1.0 vx 560.0)
  ;; frontal: el observador esta del lado -y (mas cerca = menor y)
  (fr:vista cn (/ lgg 2.0) vx 140.0 ke ts
            "POS 3 - VISTA FRONTAL (SE VE EL LARGO) ESC 4:1")
  ;; lateral derecha: el observador esta del lado +x (mas cerca = mayor x)
  (fr:vista (mapcar (function (lambda (p) (list (cadr p) (- (car p))))) cn)
            (/ ang 2.0) vx 95.0 ke ts
            "POS 3 - VISTA LATERAL (SE VE EL ANCHO) ESC 4:1")
  (fr:text (- vx (* ke (/ lgg 2.0))) 114 "CHAPA ESP. 1 MM - CONITO: BASE 3 MM, ALTURA 2 MM" 3)

  ;; ============ 2c. VISTAS FRONTAL Y LATERAL DEL CUERPO BASE (POS 1), 1:1 ============
  (fr:vperfil 470.0 0.0 (/ lg 2.0) (- (/ lg 2.0) 2.0) (- (/ lg 2.0) 7.0) al lh
              (list (car (car rb)) (cadr (car rb)) (car (cadr rb)) (cadr (cadr rb)))
              "POS 1 - VISTA FRONTAL")
  (fr:vperfil 620.0 0.0 (/ an 2.0) (- (/ an 2.0) 2.0) (- (/ an 2.0) 6.0) al lh nil
              "POS 1 - VISTA LATERAL")

  ;; ============ 2d. LINEAS OCULTAS (ACAD_ISO02W100) EN LAS VISTAS DE LA BASE ============
  ;; ---- vista frontal (centro x = 470) ----
  (setq xa (+ hx3 2.0))
  (foreach sy '(1.0 -1.0)                                    ; caida recta interior del labio
    (fr:hline (+ 470.0 (* sy (/ (- lg 6.0) 2.0))) z0 (+ 470.0 (* sy (/ (- lg 6.0) 2.0))) al))
  (fr:hline (- 470.0 (/ (- lg 6.0) 2.0)) z0 (+ 470.0 (/ (- lg 6.0) 2.0)) z0)   ; escalon
  (fr:hline (- 470.0 xa) z0 (+ 470.0 xf1) 3.0)               ; pared inclinada izquierda (embudo largo)
  (fr:hline (+ 470.0 xa) z0 (+ 470.0 xf2) 3.0)               ; pared inclinada derecha
  (fr:hline (+ 470.0 xf1) 3.0 (+ 470.0 xf2) 3.0)             ; piso
  (fr:hpoly (list (list (+ 470.0 bx) zb) (list (+ 470.0 bx bw) zb)
                  (list (+ 470.0 bx bw) zt) (list (+ 470.0 bx) zt)) T)   ; barra
  (fr:hline (+ 470.0 (fr:wallx mdc xw0 xw1 lw)) (- z0 (* mdc (/ (- z0 3.0) lw)))   ; semicirculo de la pared
            (+ 470.0 (fr:wallx (+ mdc mr) xw0 xw1 lw)) (- z0 (* (+ mdc mr) (/ (- z0 3.0) lw))))
  ;; ---- vista lateral (centro x = 620) ----
  (foreach sy '(1.0 -1.0)
    (fr:hline (+ 620.0 (* sy yt)) z0 (+ 620.0 (* sy yt)) al)             ; caida recta interior del labio
    (fr:hline (+ 620.0 (* sy yt)) z0 (+ 620.0 (* sy yp0)) z0)            ; escalon y cara plana de la nervadura
    (fr:hline (+ 620.0 (* sy yd)) z0 (+ 620.0 (* sy hw)) 3.0)            ; pared inclinada interior
    (fr:hline (+ 620.0 (* sy yp0)) z0 (+ 620.0 (* sy nn)) 3.0))          ; cara inclinada de la nervadura
  (fr:hline (- 620.0 hw) 3.0 (+ 620.0 hw) 3.0)                           ; piso
  (fr:hpoly (list (list (- 620.0 nn) zb) (list (+ 620.0 nn) zb)
                  (list (+ 620.0 ypt) zt) (list (- 620.0 ypt) zt)) T)    ; barra
  (setq mpts nil mk 0)                                                   ; semicirculo de la pared
  (while (<= mk 16)
    (setq mt (* pi (/ mk 16.0)))
    (setq mpts (cons (list (+ 620.0 (* mr (cos mt)))
                           (- z0 (* (+ mdc (* mr (sin mt))) (/ (- z0 3.0) lw))))
                     mpts))
    (setq mk (1+ mk)))
  (fr:hpoly (reverse mpts) T)
  (fr:text 470.0 -20 "LINEAS DISCONTINUAS = ARISTAS OCULTAS" 3)

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
