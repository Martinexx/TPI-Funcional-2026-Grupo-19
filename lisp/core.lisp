
*** - LOAD: A file with name $file does not exist
Break 1 [3]> 
#|
FUNCION: Transicion
NATURALEZA: Pura
ESTRATEGIA: Construccion de lista + alternativas
IMPACTO: No destructiva
|#
(defun transicion (color-actual cambiar-a)
(list color-actual 
	(cond 
		((OR (equalp cambiar-a color-actual) 
			  (AND (equalp color-actual 'en-verde) (equalp cambiar-a 'rojo))
			  (AND (equalp color-actual 'en-amarillo) (equalp cambiar-a 'verde))
			  (AND (equalp color-actual 'en-rojo) (equalp cambiar-a 'amarillo))
		 ) 'accion-por-defecto)

		((equalp cambiar-a 'rojo) "cambiar-a-rojo"  )
		((equalp cambiar-a 'amarillo) "cambiar-a-amarillo" )
		((equalp cambiar-a 'verde) "cambiar-a-verde")
		(t 'accion-por-defecto)))
	)


TRANSICION
Break 1 [3]>

#|
NOMBRE: timer
NATURALEZA: pura
ESTRATEGIA: alternativa cond
IMPACTO: no destructiva
|#

(defun timer(tiempo-actual)
	(COND ((< (MOD tiempo-actual 216) 0) 'error)
			((<= (MOD tiempo-actual 216) 120) 'verde)
			((<= (MOD tiempo-actual 216) 126) 'amarillo)
			(t 'rojo)
	)
)

#|	===================================
 	FUNCIÓN: registrar-auditoria
	NATURALEZA: Impura (Escribe en la salida estándar)
 	ESTRATEGIA: Simple
 	IMPACTO: No Destructiva
 	=================================== |#
(defun registrar-auditoria (color-anterior color-nuevo)
  (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%" (- (get-universal-time) 2208988800) color-anterior color-nuevo))


#|
NOMBRE: recomendacion-ciclo
NATURALEZA: pura
ESTRATEGIA: alternativa cond
IMPACTO: no destructiva
|#
(defun recomendacion-ciclo (duracion-ciclo)
(cond  ((< duracion-ciclo 35) "llegue al minimo de 35")
		((> duracion-ciclo 150) "baje hasta 150")
		(t "esta en rango optimo")))


#|
FUNCION: Piclos-por-tiempo
NATURALEZA: Pura
ESTRATEGIA: Division tiempoTotal/ciclo
IMPACTO: No destructiva
|#

(defun ciclos-por-tiempo (minutos) 

(float (/ (* minutos 60) (duracion-ciclo (* minutos 60)))))

#|
FUNCION: proximo-color
NATURALEZA: pura
ESTRATEGIA: aritmetica, condicional  
IMPACTO: no destructiva
|#

(defun proximo-color (horaUnix)
	(+ horaUnix (- (cond 
						((equal (timer horaUnix) 'rojo) 91)
						((equal (timer horaUnix) 'verde) 211)
						((equal (timer horaUnix) 'amarillo) 216)
					) 
					(MOD horaUnix (duracion-ciclo))
				)
	)
)

#|
CASOS DE PRUEBA
Comportamiento Normal:
	(proximo-color 1781379859) -> 1781379864
	(proximo-color 1781379864) -> 1781379955
	(proximo-color 1781379955) -> 1781380075
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

#| ITERACION 2 
FUNCION: Transicion
NATURALEZA: Pura
ESTRATEGIA: Construccion de lista + alternativas
IMPACTO: No destructiva
|#

(defun transicion (color-actual cambiar-a)
(list color-actual 
	(cond 
		((OR (equalp cambiar-a color-actual) 

			(AND (equalp color-actual 'en-verde) (equalp cambiar-a 'rojo))
			(AND (equalp color-actual 'en-amarillo) (equalp cambiar-a 'verde))
			(AND (equalp color-actual 'en-rojo) (equalp cambiar-a 'amarillo))	
 
			(AND (not (equalp color-actual 'en-rojo)) (equalp cambiar-a 'rojo-intermitente))
			(AND (not (equalp color-actual 'en-amarillo)) (equalp cambiar-a 'amarillo-intermitente))
			(AND (not (equalp color-actual 'en-verde)) (equalp cambiar-a 'verde-intermitente))
		 ) 'accion-por-defecto)

		((equalp cambiar-a 'rojo)  "cambiar-a-rojo")
		((equalp cambiar-a 'amarillo)  "cambiar-a-amarillo")
		((equalp cambiar-a 'verde) "cambiar-a-verde")


		((equalp cambiar-a 'rojo-intermitente) "cambiar-a-rojo-intermitente")
		((equalp cambiar-a 'amarillo-intermitente) "cambiar-a-amarillo-intermitente")
		((equalp cambiar-a 'verde-intermitente) "cambiar-a-verde-intermitente")
		(t 'accion-por-defecto))
)
)	

#|
FUNCION: Transicion
NATURALEZA: Pura
ESTRATEGIA: Construccion de lista + condicional
IMPACTO: No destructiva
|#

(defun transicion (color-actual cambiar-a)
	(list color-actual 
		(cond 
			((AND (equalp color-actual 'verde-intermitente) (equalp cambiar-a 'rojo)) "cambiar-a-rojo")
			((AND (equalp color-actual 'rojo-intermitente) (equalp cambiar-a 'amarillo)) "cambiar-a-amarillo")
			((AND (equalp color-actual 'amarillo-intermitente) (equalp cambiar-a 'verde)) "cambiar-a-verde")

			((AND (equalp color-actual 'rojo) (equalp cambiar-a 'rojo-intermitente) "cambiar-a-rojo-intermitente")
			((AND (equalp color-actual 'amarillo) (equalp cambiar-a 'amarillo-intermitente) "cambiar-a-amarillo-intermitente")
			((AND (equalp color-actual 'verde) (equalp cambiar-a 'verde-intermitente) "cambiar-a-verde-intermitente")
			(t 'accion-por-defecto)
		)
	)
)

