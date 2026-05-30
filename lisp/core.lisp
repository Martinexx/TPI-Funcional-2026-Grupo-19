
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
