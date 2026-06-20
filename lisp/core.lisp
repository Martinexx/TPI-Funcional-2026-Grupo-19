#|
FUNCION: transicion
NATURALEZA: Pura
ESTRATEGIA: Construcion de lista + condicional
IMPACTO: no destructiva
|#

(defun transicion (color-actual cambiar-a)
	(list color-actual 
		(cond 
			((AND (equalp color-actual 'en-amarillo) (equalp cambiar-a 'rojo)) "cambiar-a-rojo")
			((AND (equalp color-actual 'en-rojo) (equalp cambiar-a 'verde)) "cambiar-a-verde")
			((AND (equalp color-actual 'en-verde) (equalp cambiar-a 'amarillo)) "cambiar-a-amarillo")
			(t 'accion-por-defecto)
		)
	)
)

#|
CASOS DE PRUEBA
Comportamiento Normal:
	(transicion 'en-amarillo 'rojo) -> (EN-AMARILLO "cambiar-a-rojo")
	(transicion 'en-rojo 'verde) -> (EN-ROJO "cambiar-a-verde")
	(transicion 'en-verde 'amarillo) -> (EN-VERDE "cambiar-a-amarillo")

Comportamiento Alternativo:
	(transicion 'en-rojo 'rojo) -> (EN-ROJO ACCION-POR-DEFECTO)
	(transicion 'texto-sin 'sentido) -> (TEXTO-SIN ACCION-POR-DEFECTO)
|#

#|
NOMBRE: timer
NATURALEZA: pura
ESTRATEGIA: condicional
IMPACTO: no destructiva
|#

(defun timer (tiempo-actual)
	(COND 
		((<= (MOD tiempo-actual (duracion-ciclo)) 89) 'rojo)
		((<= (MOD tiempo-actual (duracion-ciclo)) 209) 'verde)
		(t 'amarillo)
	)
)

#|
CASOS DE PRUEBA
Comportamiento Normal:
	(timer 1781241929) -> ROJO
	(timer 1781242600) -> VERDE
	(timer 1781242700) -> AMARILLO

Caso de Error:
	(timer 'texto) -> MOD: TEXTO is not a real number 
|#

#|	===================================
 	FUNCIÓN: registrar-auditoria
	NATURALEZA: Impura (Escribe en la salida estándar)
 	ESTRATEGIA: Simple
 	IMPACTO: No Destructiva
 	=================================== |#
(defun registrar-auditoria (color-anterior color-nuevo)
  (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"  
(local-time:format-timestring nil (local-time:now):format '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2)))
(CAR (transicion color-anterior color-nuevo))  (CADR (transicion color-anterior color-nuevo ))))

#|
Funcion: duracion-ciclo
Naturaleza: pura
Estrategia: suma los valores del ciclo
Impacto: no destructiva
|#

(defun duracion-ciclo ()
	(+ 90 120 6)
)

#|
CASO DE PRUEBA
	(duracion-ciclo) -> 216
|#

#|
NOMBRE: recomendacion-ciclo
NATURALEZA: pura
ESTRATEGIA: condicional
IMPACTO: no destructiva
|#

(defun recomendacion-ciclo (duracion-ciclo)
	(cond  
		((< duracion-ciclo 35) "llegue al minimo de 35")
		((> duracion-ciclo 150) "baje hasta 150")
		(t "esta en rango optimo")
	)
)

#|
CASOS DE PRUEBA
Comportamiento Normal: 
	(recomendacion-ciclo 20) -> "llegue al minimo de 35"
	(recomendacion-ciclo 100) -> "esta en rango optimo"
	(recomendacion-ciclo 200) -> "baje hasta 150"
	
Caso de error:
	(recomendacion-ciclo #C(0 1)) -> <: #C(0 1) is not a real number
|#

#|
FUNCION: ciclos-por-tiempo
NATURALEZA: Pura
ESTRATEGIA: Division tiempoTotal/ciclo
IMPACTO: No destructiva
|#

(defun ciclos-por-tiempo (minutos) 
	(values (TRUNCATE (/ (* minutos 60) (duracion-ciclo))))
)

;Si se le pasa solo un número a truncate este devuelve la parte entera y la parte decimal del número por separado, 
;devuelve 2 valores, pero al usarse como entrada de otra funcion, esta tomara solo el primer valor devuelto,
;values toma ese unico valor, la parte entera del argumento de truncate y lo devuelve.

#|
CASOS DE PRUEBA
Comportamiento Normal: 
	(ciclos-por-tiempo 60) -> 16
	(ciclos-por-tiempo 123) -> 34

Caso de error: 
	(ciclos-por-tiempo 'no-num) -> *: NO-NUM is not a number
|#

#|
FUNCION: proximo-color
NATURALEZA: pura
ESTRATEGIA: aritmetica, condicional  
IMPACTO: no destructiva
|#

(defun proximo-color (horaUnix)
	(+ (- horaUnix (MOD horaUnix (duracion-ciclo))) (cond 
														((equal (timer horaUnix) 'rojo) 90)
														((equal (timer horaUnix) 'verde) 210)
														((equal (timer horaUnix) 'amarillo) 216)
													) 
	)
				
)


#|
CASOS DE PRUEBA
Comportamiento Normal:
	(proximo-color 1781379859) -> 1781379864
	(proximo-color 1781379864) -> 1781379954
	(proximo-color 1781379955) -> 1781380074
Caso de error:
	(proximo-color 'no-num) -> MOD: NO-NUM is not a real number	
|#

#|
FUNCION: veces_periodo
NATURALEZA: pura
ESTRATEGIA: recursividad multiple 
IMPACTO: no destructiva
|#

(defun veces_periodo (ini fin color)
	(COND 
		((> ini fin) 0)
		((equal (timer ini) color) (+ 1 (veces_periodo (proximo-color ini) fin color)))
		(t (veces_periodo (proximo-color ini) fin color))
	)
)

#|
CASOS DE PRUEBA
Comportamiento Normal:
	(veces_periodo 1781299243 1781302843 'rojo) -> 17 
	(veces_periodo 1781299243 1781302843 'verde) -> 18
	(veces_periodo 1781299243 1781302843 'amarillo) -> 17
	(veces_periodo 0 224639 'rojo) -> 1040
Comportamiento Alternativo:
	(veces_periodo 1781299243 1781302843 'no-color) -> 0
Caso de error:
	(veces_periodo 0 224640 'rojo) -> Program stack overflow. RESET
	(veces_periodo 1781299243 'no-num 'amarillo) -> >: NO-NUM is not a real number 
|#

#|
FUNCION: distribucionPorcentual
NATURALEZA: pura
ESTRATEGIA: orden superior (mapcar) 
IMPACTO: no destructiva
|#

(defun distribucionPorcentual (horaUnix)
	(mapcar (lambda (color) (list color (float (/ (* (veces_periodo horaUnix (+ horaUnix 3600) color) 100) (+ (veces_periodo horaUnix (+ horaUnix 3600) 'rojo) (veces_periodo horaUnix (+ horaUnix 3600) 'verde) (veces_periodo horaUnix (+ horaUnix 3600) 'amarillo)))))) 
			'(rojo verde amarillo)
	)
)

#|
CASOS DE PRUEBA
Comportamiento Normal:
	(distribucionPorcentual 1781381026) -> ((ROJO 34.615383) (VERDE 32.692307) (AMARILLO 32.692307))
	(distribucionPorcentual 1781373826) -> ((ROJO 34.0) (VERDE 34.0) (AMARILLO 32.0))
Caso de Error:
	(distribucionPorcentual 'texto) -> +: TEXTO is not a number
|#
