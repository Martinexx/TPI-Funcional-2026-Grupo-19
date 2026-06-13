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
	(transicion 'texto-sin 'sentido) -> (TEXTO-SIN ACCION-POR-DEFECTO)  //este no se si cuenta como error
|#