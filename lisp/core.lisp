
*** - LOAD: A file with name $file does not exist
Break 1 [3]> 
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
  (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"  
(local-time:format-timestring nil (local-time:now):format '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2)))
(CAR (transicion color-anterior color-nuevo))  (CADR (transicion color-anterior color-nuevo ))))



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

(truncate (/ (* minutos 60) (duracion-ciclo))))

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

; ITERACION 2 
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

#|
NOMBRE: timer
NATURALEZA: pura
ESTRATEGIA: condicional
IMPACTO: no destructiva
|#

(defun timer(tiempo-actual)
	(COND 
		((<= (MOD tiempo-actual (duracion-ciclo)) 86) 'rojo)
		((<= (MOD tiempo-actual (duracion-ciclo)) 89) 'rojo-intermitente)
		((<= (MOD tiempo-actual (duracion-ciclo)) 206) 'verde)
		((<= (MOD tiempo-actual (duracion-ciclo)) 209) 'verde-intermitente)
		((<= (MOD tiempo-actual (duracion-ciclo)) 212) 'amarillo)
		(t 'amarillo-intermitente)
	)
)

#|
CASOS DE PRUEBA
Comportamiento Normal:
	(timer 1781245161) -> ROJO
	(timer 1781245168) -> ROJO-INTERMITENTE 
	(timer 1781245268) -> VERDE 
	(timer 1781245504) -> VERDE-INTERMITENTE
	(timer 1781245507) -> AMARILLO
	(timer 1781245510) -> AMARILLO-INTERMITENTE

Caso de Error:
	(timer '(lista)) ->	MOD: (LISTA) is not a real number	
|#			

#|
FUNCION: proximo-color
NATURALEZA: pura
ESTRATEGIA: aritmetica, condicional  
IMPACTO: no destructiva
|#

(defun proximo-color (horaUnix)
	(+ horaUnix (- (cond 
						((equal (timer horaUnix) 'rojo) 91)
						((equal (timer horaUnix) 'rojo-intermitente) 91)
						((equal (timer horaUnix) 'verde) 211)
						((equal (timer horaUnix) 'verde-intermitente) 211)
						((equal (timer horaUnix) 'amarillo) 216)
						((equal (timer horaUnix) 'amarillo-intermitente) 216)
					) 
					(MOD horaUnix (duracion-ciclo))
				)
	)
)

#|
CASOS DE PRUEBA
Comportamiento Normal:
	(proximo-color 1781379864) -> 1781379955 
	(proximo-color 1781379952) -> 1781379955
	(proximo-color 1781379955) -> 1781380075
	(proximo-color 1781380072) -> 1781380075
	(proximo-color 1781380075) -> 1781380080
	(proximo-color 1781380078) -> 1781380080
Caso de error:
	(proximo-color 'no-num) -> MOD: NO-NUM is not a real number	
|#

#|
FUNCION: complemento
NATURALEZA: pura
ESTRATEGIA: condicional
IMPACTO: no destructiva
|#

(defun complemento (color)
	(COND
		((equal color 'rojo) 'rojo-intermitente)
		((equal color 'verde) 'verde-intermitente)
		((equal color 'amarillo) 'amarillo-intermitente)

		((equal color 'rojo-intermitente) 'rojo)
		((equal color 'verde-intermitente) 'verde)
		((equal color 'amarillo-intermitente) 'amarillo)
	)
)

#|
CASOS DE PRUEBA
Comportamiento Normal:
	(complemento 'rojo) -> ROJO-INTERMITENTE
	(complemento 'verde) -> VERDE-INTERMITENTE
	(complemento 'amarillo) -> AMARILLO-INTERMITENTE
	(complemento 'rojo-intermitente) -> ROJO
	(complemento 'amarillo-intermitente) -> AMARILLO
	(complemento 'verde-intermitente) -> VERDE
Comportamiento Alternativo:
	(complemento 'texto) -> NIL
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
		(
			(OR (equal (timer ini) color) (equal (timer ini) (complemento color)))
			(+ 1 (veces_periodo (proximo-color ini) fin color))
		)
		(t (veces_periodo (proximo-color ini) fin color))
	)
)

#|
CASOS DE PRUEBA
Comportamiento Normal:
	(veces_periodo 1781299243 1781302843 'rojo) -> 17
	(veces_periodo 1781299243 1781302843 'verde) -> 18
	(veces_periodo 1781299243 1781302843 'amarillo) -> 17
Comportamiento Alternativo:
	(veces_periodo 1781299243 1781302843 'no-color) -> 0
Caso de error:
	(veces_periodo 0 224640 'rojo) -> Program stack overflow. RESET
	(veces_periodo 1781299243 'no-num 'amarillo) -> >: NO-NUM is not a real number
|#


			
#|
FUNCION: informe
NATURALEZA: impura
ESTRATEGIA: reutiliza otras funciones para cumplir con lo pedido
IMPACTO: No destructivo
|#
(defun informe (color-anterior color-nuevo)
(with-open-file (stream
                 "informe-ejecucion-semaforo.txt"
                 :direction :output
                 :if-exists :append)
  
(format stream "Informe de Ejecución del Sistema Semafórico~%")
(format stream "=========================================~%")

(format stream(registrar-auditoria color-anterior color-nuevo))


(format stream "~% --- Fin del Informe ---")))

#|
casos de pruebas:
caso normal: 'en-rojo 'verde - Tiempo [formato pedido]: la luz ha cambiado de 'en-rojo a "cambiar-a-verde"
caso alternativo: 5 'amarillo - Tiempo [formato pedido]: la luz ha cambiado de 5  a 'accion-por-defecto

|#


NOMBRE: timer
NATURALEZA: pura
ESTRATEGIA: condicional
IMPACTO: no destructiva
|#

(defun timer(tiempo-actual)
	(COND 
		((<= (MOD tiempo-actual (duracion-ciclo)) 90) 'rojo)
		((<= (MOD tiempo-actual (duracion-ciclo)) 210) 'verde)
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

#|
FUNCION: duracion-ciclo
NATURALEZA: pura
ESTRATEGIA: suma
IMPACTO: no destructiva
|#

(defun duracion-ciclo()
	(+ 90 120 6)
)			

#|
CASO DE PRUEBA
(duracion-ciclo) -> 216
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
