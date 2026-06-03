
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
FUNCION: veces_periodo
NATURALEZA: pura
ESTRATEGIA: recursividad 
IMPACTO: no destructiva
|#

(defun veces_periodo(ini fin color)
	(COND ((> ini fin) 0)
			((AND (equal (timer ini) color) (not (equal (timer ini) (timer (- ini 1))))) 
			 (+ 1 (veces_periodo (+ ini 1) fin color))
			)

			(t (veces_periodo (+ ini 1) fin color))
	)
)

#|
FUNCION: distribucionPorcentual
NATURALEZA: pura
ESTRATEGIA: orden superior (mapcar) 
IMPACTO: no destructiva
|#

(defun distribucionPorcentual (horaUnix)
	(mapcar (lambda (color) (list color (/ (* (veces_periodo horaUnix (+ horaUnix 3600) color) 100) (+ (veces_periodo horaUnix (+ horaUnix 3600) 'verde) (veces_periodo horaUnix (+ horaUnix 3600) 'amarillo) (veces_periodo horaUnix (+ horaUnix 3600) 'rojo))))) 
			'(verde amarillo rojo)
	)
)

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



