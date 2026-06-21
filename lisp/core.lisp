(Load "C:/Users/Valen/quicklisp/setup.lisp") ;debe utilizar su propia ruta
(quicklisp-quickstart:install)
(ql:quickload :Local-time)

;;===================================================================================
;;FUNCION: Transicion
;;NATURALEZA: Pura (Dados dos simbolos en un orden, siempre devuelve la misma lista)
;;ESTRATEGIA: Construccion de lista + condicional
;;IMPACTO: No destructiva
;;===================================================================================

;;; transicion recibe dos colores y retorna una lista de la forma (color-actual "cambiar-a-cambiar-a"), "cambiar-a-" seguido del valor del 
;;; parametro cambiar-a 

(defun transicion (color-actual cambiar-a)
 (list color-actual (cond 
					 ((AND (eq color-actual 'en-rojo) (eq cambiar-a 'rojo-intermitente)) "cambiar-a-rojo-intermitente")
					 ((AND (eq color-actual 'en-verde) (eq cambiar-a 'verde-intermitente)) "cambiar-a-verde-intermitente")
					 ((AND (eq color-actual 'en-amarillo) (eq cambiar-a 'amarillo-intermitente)) "cambiar-a-amarillo-intermitente")

					 ((AND (eq color-actual 'en-rojo-intermitente) (eq cambiar-a 'verde)) "cambiar-a-verde")
					 ((AND (eq color-actual 'en-verde-intermitente) (eq cambiar-a 'amarillo)) "cambiar-a-amarillo")
					 ((AND (eq color-actual 'en-amarillo-intermitente) (eq cambiar-a 'rojo)) "cambiar-a-rojo")
					 (t 'accion-por-defecto))))

#|
CASOS DE PRUEBA
 Comportamiento Normal:
  (transicion 'en-rojo 'rojo-intermitente) -> (EN-ROJO "cambiar-a-rojo-intermitente")
  (transicion 'en-rojo-intermitente 'verde) -> (EN-ROJO-INTERMITENTE "cambiar-a-verde")
  (transicion 'en-verde 'verde-intermitente) -> (EN-VERDE "cambiar-a-verde-intermitente")
  (transicion 'en-verde-intermitente 'amarillo) -> (EN-VERDE-INTERMITENTE "cambiar-a-amarillo")
  (transicion 'en-amarillo 'amarillo-intermitente) -> (EN-AMARILLO "cambiar-a-amarillo-intermitente")
  (transicion 'en-amarillo-intermitente 'rojo) ->	(EN-AMARILLO-INTERMITENTE "cambiar-a-rojo")	

 Comportamiento Alternativo:	
  (transicion 'en-rojo-intermitente 'amarillo) -> (EN-ROJO-INTERMITENTE ACCION-POR-DEFECTO)
  (transicion 'en-verde-intermitente 'verde-intermitente) -> (EN-VERDE-INTERMITENTE ACCION-POR-DEFECTO)
  (transicion 'cualquier-cosa 8) -> (CUALQUIER-COSA ACCION-POR-DEFECTO)	
|#		

;;========================================================================
;;FUNCION: timer
;;NATURALEZA: pura (Dado un tiempo unix, siempre devuelve el mismo color)
;;ESTRATEGIA: condicional + aritmetica
;;IMPACTO: no destructiva
;;========================================================================

;;;timer recibe tiempo unix y determina de que color está el semáforo en ese momento

(defun timer (tiempo-actual)
(COND   ((<= (MOD tiempo-actual (duracion-ciclo)) 89) 'rojo)
		((<= (MOD tiempo-actual (duracion-ciclo)) 92) 'rojo-intermitente)
		((<= (MOD tiempo-actual (duracion-ciclo)) 212) 'verde)
		((<= (MOD tiempo-actual (duracion-ciclo)) 215) 'verde-intermitente)
		((<= (MOD tiempo-actual (duracion-ciclo)) 221) 'amarillo)
		(t 'amarillo-intermitente)))

#|
CASOS DE PRUEBA
 Comportamiento Normal:
  (timer 1781245161) -> ROJO
  (timer 1781245215) -> ROJO-INTERMITENTE 
  (timer 1781245268) -> VERDE 
  (timer 1781245338) -> VERDE-INTERMITENTE
  (timer 17812455520) -> AMARILLO
  (timer 17812455522) -> AMARILLO-INTERMITENTE

 Caso de Error:
  (timer '(lista)) ->	MOD: (LISTA) is not a real number	
|#

;;==================================================
;;FUNCIÓN: registrar-auditoria
;;NATURALEZA: Impura (Escribe en la salida estándar)
;;ESTRATEGIA: Simple
;;IMPACTO: No Destructiva
;;==================================================

;;;registrar-auditoria imprime el cuando y el resultado de evaluar la transicion de dos colores

(defun registrar-auditoria (color-anterior color-nuevo)
 (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"  
  (local-time:format-timestring nil (local-time:now):format '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2)))
  (FIRST (transicion color-anterior color-nuevo))  (SECOND (transicion color-anterior color-nuevo ))))

#|
CASOS DE PRUEBA
 Caso Normal: 
  (registrar-auditoria 'en-rojo 'verde) -> "Tiempo [instante en el que se ejecuta la funcion con el formato requerido]: la luz ha cambiado de 'en-rojo a cambiar-a-verde"
 Caso Alternativo: 
  (registrar-auditoria 'en-rojo 'rojo) -> "Tiempo [instante en el que se ejecuta la funcion con el formato requerido]: la luz ha cambiado de 'en-rojo a 'accion-por-defecto"
|#

;;================================================================
;;FUNCION: informe
;;NATURALEZA: impura
;;ESTRATEGIA: reutiliza otras funciones para cumplir con lo pedido
;;IMPACTO: No destructivo
;;================================================================

;;;informe guarda la impresion de registrar-auditoria en un archivo de texto, si no existe crea dicho .txt y lo guarda

(defun informe (color-anterior color-nuevo)
 (with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output :if-exists :append)
  (format stream "Informe de Ejecución del Sistema Semafórico~%")
  (format stream "=========================================~%")
  (format stream (registrar-auditoria color-anterior color-nuevo))
  (format stream "~% --- Fin del Informe ---")))

#|
CASOS DE PRUEBA
 Caso Normal:
  (informe 'en-rojo 'verde - Tiempo [formato pedido]: la luz ha cambiado de 'en-rojo a "cambiar-a-verde"
 Caso Alternativo:
  5 'amarillo - Tiempo [formato pedido]: la luz ha cambiado de 5  a 'accion-por-defecto
|#

;;==================================================
;;FUNCION: duracion-ciclo
;;NATURALEZA: pura (siempre devuelve el mismo valor)
;;ESTRATEGIA: suma la duracion de cada color
;;IMPACTO: no destructiva
;;==================================================

;;;duracion-ciclo calcula los segundos del ciclo con intermitencia

(defun duracion-ciclo ()
 (+ 90 3 120 3 6 3))

#|
CASO DE PRUEBA
 (duracion-ciclo) -> 225
|#

;;============================================================================
;;FUNCION: recomendacion-ciclo
;;NATURALEZA: pura (Dada una duracion siempre devuelve la misma recomendacion)
;;ESTRATEGIA: condicional
;;IMPACTO: no destructiva
;;============================================================================

;;;recomendacion-ciclo determina como es la duracion de un ciclo, corta, ideal o larga y aconseja una optimizacion

(defun recomendacion-ciclo (duracion)
 (cond ((< duracion 35) "llegue al minimo de 35")
	   ((> duracion 150) "baje hasta 150")
	   (t "esta en rango optimo")))

#|
CASOS DE PRUEBA
 Comportamiento Normal: 
  (recomendacion-ciclo 20) -> "llegue al minimo de 35"
  (recomendacion-ciclo 100) -> "esta en rango optimo"
  (recomendacion-ciclo 200) -> "baje hasta 150"
 
 Caso de Error:
  (recomendacion-ciclo #C(0 1)) -> <: #C(0 1) is not a real number
  (recomendacion-ciclo 'no-num) -> <: NO-NUM is not a real number
|#

;;==============================================================================================
;;FUNCION: ciclos-por-tiempo
;;NATURALEZA: Pura (Dada una cantidad de minutos, siempre devuelve la misma cantidad de ciclos)
;;ESTRATEGIA: Division tiempoTotal/ciclo
;;IMPACTO: No destructiva
;;==============================================================================================

;;;ciclos-por-tiempo calcula la cantidad de ciclos completos que entran en cierta cantidad de minutos

(defun ciclos-por-tiempo (minutos) 
 (values (TRUNCATE (/ (* minutos 60) (duracion-ciclo)))))

;;Cuando una funcion esta como argumento de otra siempre la funcion externa tomara solo el primer valor que devuelva la funcion interna 
;;truncate devuelve la parte entera y la decimal de la division, values devuelve solo la parte entera

#|
CASOS DE PRUEBA
 Comportamiento Normal: 
  (ciclos-por-tiempo 60) -> 16
  (ciclos-por-tiempo 123) -> 32

 Caso de error: 
  (ciclos-por-tiempo 'no-num) -> *: NO-NUM is not a number
|#

;;========================================================================
;;FUNCION: proximo-color
;;NATURALEZA: pura (Dado un tiempo unix, devuelve siempre el mismo color)
;;ESTRATEGIA: aritmetica, condicional  
;;IMPACTO: no destructiva
;;========================================================================

;;;proximo-color recibe un tiempo unix y lo devuelve adelantado hasta el primer segundo del siguente color del ciclo,
;;;sin tener en cuenta las intermitencias como colores distintos

(defun proximo-color (horaUnix)
 (+ (- horaUnix (MOD horaUnix (duracion-ciclo))) (cond 
										  		  ((eq (timer horaUnix) 'rojo) 93)
												  ((eq (timer horaUnix) 'rojo-intermitente) 93)
												  ((eq (timer horaUnix) 'verde) 216)
												  ((eq (timer horaUnix) 'verde-intermitente) 216)
												  ((eq (timer horaUnix) 'amarillo) 225)
												  ((eq (timer horaUnix) 'amarillo-intermitente) 225))))

#|
CASOS DE PRUEBA
 Comportamiento Normal:
  (proximo-color 1781245161) -> 1781245218 
  (proximo-color 1781245215) -> 1781245218
  (proximo-color 1781245268) -> 1781245341
  (proximo-color 1781245338) -> 1781245341
  (proximo-color 17812455520) -> 17812455525
  (proximo-color 17812455522) -> 17812455525
 Caso de Error:
  (proximo-color 'no-num) -> MOD: NO-NUM is not a real number	
|#

;;======================================================================
;;FUNCION: color-complementario
;;NATURALEZA: pura (Dado un simbolo, siempre devuelve un mismo simbolo)
;;ESTRATEGIA: condicional
;;IMPACTO: no destructiva
;;======================================================================

;;;color-complementario recibe un color y devuelve el otro simbolo que representa el mismo color

(defun color-complementario (color)
 (COND
  ((eq color 'rojo) 'rojo-intermitente)
  ((eq color 'verde) 'verde-intermitente)
  ((eq color 'amarillo) 'amarillo-intermitente)
  ((eq color 'rojo-intermitente) 'rojo)
  ((eq color 'verde-intermitente) 'verde)
  ((eq color 'amarillo-intermitente) 'amarillo)))

#|
CASOS DE PRUEBA
 Comportamiento Normal:
  (color-complementario 'rojo) -> ROJO-INTERMITENTE
  (color-complementario 'verde) -> VERDE-INTERMITENTE
  (color-complementario 'amarillo) -> AMARILLO-INTERMITENTE
  (color-complementario 'rojo-intermitente) -> ROJO
  (color-complementario 'amarillo-intermitente) -> AMARILLO
  (color-complementario 'verde-intermitente) -> VERDE
 Comportamiento Alternativo:
  (color-complementario 'texto) -> NIL
|#

#|
FUNCION: veces_periodo
NATURALEZA: pura
ESTRATEGIA: recursividad multiple 
IMPACTO: no destructiva
|#

(defun veces_periodo (ini fin color)
	(COND ((> ini fin) 0)
		((OR (equal (timer ini) color) (equal (timer ini) (color-complementario color))) 
		(+ 1 (veces_periodo (proximo-color ini) fin color)))
		(t (veces_periodo (proximo-color ini) fin color))))

#|
CASOS DE PRUEBA
Comportamiento Normal:
	(veces_periodo 1781299243 1781302843 'rojo) -> 16
	(veces_periodo 1781299243 1781302843 'verde) -> 17
	(veces_periodo 1781299243 1781302843 'amarillo) -> 16
Comportamiento Alternativo:
	(veces_periodo 1781299243 1781302843 'no-color) -> 0
Caso de error:
	(veces_periodo 0 2300000 'rojo) -> Program stack overflow. RESET
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
	(distribucionPorcentual 1781381026) -> ((ROJO 34.69388) (VERDE 32.65306) (AMARILLO 32.65306))
	(distribucionPorcentual 1781373826) -> ((ROJO 34.69388) (VERDE 32.65306) (AMARILLO 32.65306))
Caso de Error:
	(distribucionPorcentual 'texto) -> +: TEXTO is not a number
|#
