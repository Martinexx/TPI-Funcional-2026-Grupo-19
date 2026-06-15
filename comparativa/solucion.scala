/*
FUNCION: transicion
NATURALEZA: pura
ESTRATEGIA: condicional, construccion de lista  
IMPACTO: no destructiva
*/

def transicion(coloractual: String, cambiara: String): List[String] = {
if (
	((coloractual == "en-rojo") && (cambiara == "verde")) ||
	((coloractual == "en-verde") && (cambiara == "amarillo")) ||
	((coloractual == "en-amarillo") && (cambiara == "rojo"))
	) 
{

	List(coloractual, "cambiar-a-".+(cambiara))

} else {

	List (coloractual, "accion-por-defecto")

	}
}

/*
CASOS DE PRUEBA
Comportamiento Normal:
	transicion("en-rojo", "verde") -> List[String] = List(en-rojo, cambiar-a-verde)
	transicion("en-verde", "amarillo") -> List[String] = List(en-verde, cambiar-a-amarillo)
	transicion("en-amarillo", "rojo") ->  List[String] = List(en-amarillo, cambiar-a-rojo)
Caso de Error:
	transicion(8, "rojo") -> error: type mismatch;
        					found   : Int(8)
        					required: String 
*/

/*
FUNCION: transicion_inter
NATURALEZA: pura
ESTRATEGIA: condicional, construccion de lista  
IMPACTO: no destructiva
*/

def transicion_inter(coloractual: String, cambiara: String): List[String] = {
if (
	((coloractual == "en-rojo") && (cambiara == "rojo-intermitente")) ||
	((coloractual == "en-verde") && (cambiara == "verde-intermitente")) ||
	((coloractual == "en-amarillo") && (cambiara == "amarillo-intermitente")) ||
	((coloractual == "en-rojo-intermitente") && (cambiara == "verde")) ||
	((coloractual == "en-verde-intermitente") && (cambiara == "amarillo")) ||
	((coloractual == "en-amarillo-intermitente") && (cambiara == "rojo")) 
	) 
{

	List(coloractual, "cambiar-a-".+(cambiara))

} else {

	List (coloractual, "accion-por-defecto")

	}
}


/*
CASOS DE PRUEBA
Comportamiento Normal:
	transicion_inter("en-rojo", "rojo-intermitente") -> List(en-rojo, cambiar-a-rojo-intermitente)
	transicion_inter("en-verde", "verde-intermitente") -> List(en-verde, cambiar-a-verde-intermitente)
	transicion_inter("en-amarillo", "amarillo-intermitente") ->  List(en-amarillo, cambiar-a-amarillo-intermitente)
	transicion_inter("en-rojo-intermitente", "verde") -> List(en-rojo-intermitente, cambiar-a-verde)
	transicion_inter("en-verde-intermitente", "amarillo") -> List(en-verde-intermitente, cambiar-a-amarillo)
	transicion_inter("en-amarillo-intermitente", "rojo") -> List(en-amarillo-intermitente, cambiar-a-rojo)
Caso de Error:
	transicion_inter(8, "rojo") -> error: type mismatch;
        					found   : Int(8)
        					required: String
*/

/*
Funcion: timer
Naturaleza: pura
Estrategia: utilizacion de condicionales para determinar el rango
Impacto: no destrutiva
*/
def timer (segundos:Int): String = {
segundos match {
	case segundos if ((segundos % 216) <= 89) => "rojo"
	case segundos if ((segundos % 216) <= 209) => "verde"
	case _ => "amarillo"
}
}
/*
casos de prueba:
caso normal: 220 -  "rojo"
caso error: cualquier parametro que no sea numero entero
*/


/*
Funcion: timer_inter
Naturaleza: pura
Estrategia: utilizacion de condicionales para determinar el rango
Impacto: no destrutiva
*/
def timer_inter (segundos:Int): String = {
segundos match {
	case segundos if ((segundos % 216) <= 86) => "rojo"
	case segundos if ((segundos % 216) <= 89) => "rojo-intermitente"
	case segundos if ((segundos % 216) <= 206) => "verde"
	case segundos if ((segundos % 216) <= 209) => "verde-intermitente"
	case segundos if ((segundos % 216) <= 212) => "amarillo"
	case _ => "amarillo-intermitente"
}
}
/*
casos de prueba:
caso normal: 304 - "rojo-intermitente" 
caso error: cualquier parametro que no sea entero

*/

/*
Objeto singlenton Semaforo, contiene transicion y timer como metodos propios
*/
object Semaforo {

		def transicionIntermitencia (coloractual: String, cambiara: String): List[String] = {
		if (/*no se puede pasar de un color al siguiente si el actual no es intermitente*/
		((coloractual == "en-rojo-intermitente") && (cambiara == "verde")) ||
		((coloractual == "en-amarillo-intermitente") && (cambiara == "rojo")) ||
		((coloractual == "en-verde-intermitente") && (cambiara == "amarillo")) ||
		((coloractual == "en-rojo") && (cambiara == "rojo-intermitente")) ||
		((coloractual == "en-amarillo") && (cambiara == "amarillo-intermitente")) ||
		((coloractual == "en-verde") && (cambiara == "verde-intermitente"))) 

		{List(coloractual, cambiara)} 

		else 

		{List (coloractual, "accion-por-defecto")}	
		}

		def timer (segundos:Int): String = {
segundos match {
	case segundos if ((segundos % 216) <= 87) => "rojo"
	case segundos if ((segundos % 216) <= 90) => "rojo-intermitente"
	case segundos if ((segundos % 216) <= 207) => "verde"
	case segundos if ((segundos % 216) <= 210) => "verde-intermitente"
	case segundos if ((segundos % 216) <= 213) => "amarillo"
	case _ => "amarillo-intermitente"}}
}
