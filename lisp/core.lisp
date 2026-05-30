
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
	((equalp cambiar-a color-actual) 'accion-por-defecto)
	((equalp cambiar-a 'rojo) "cambiar-a-rojo"  )
	((equalp cambiar-a 'amarillo) "cambiar-a-amarillo" )
	((equalp cambiar-a 'verde) "cambiar-a-verde")
	(t 'accion-por-defecto))))

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
			((<= (MOD tiempo-actual 216) 210) 'amarillo)
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


