/*
FUNCION: transicion
NATURALEZA: pura
ESTRATEGIA: condicional, construccion de lista  
IMPACTO: no destructiva
*/

def transicion(coloractual: String, cambiara: String): List[String] = {
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

def transicion(coloractual: String, cambiara: String): List[String] = {if (((coloractual == "en-rojo") && (cambiara == "rojo-intermitente")) || ((coloractual == "en-verde") && (cambiara == "verde-intermitente")) || ((coloractual == "en-amarillo") && (cambiara == "amarillo-intermitente")) || ((coloractual == "en-rojo-intermitente") && (cambiara == "verde")) || ((coloractual == "en-verde-intermitente") && (cambiara == "amarillo")) || ((coloractual == "en-amarillo-intermitente") && (cambiara == "rojo"))) {List(coloractual, "cambiar-a-".+(cambiara))} else {List (coloractual, "accion-por-defecto")}}


/*
CASOS DE PRUEBA
Comportamiento Normal:
	print(transicion("en-rojo", "rojo-intermitente")) -> List(en-rojo, cambiar-a-rojo-intermitente)
	print(transicion("en-verde", "verde-intermitente")) -> List(en-verde, cambiar-a-verde-intermitente)
	print(transicion("en-amarillo", "amarillo-intermitente")) ->  List(en-amarillo, cambiar-a-amarillo-intermitente)
	print(transicion("en-rojo-intermitente", "verde")) -> List(en-rojo-intermitente, cambiar-a-verde)
	print(transicion("en-verde-intermitente", "amarillo")) -> List(en-verde-intermitente, cambiar-a-amarillo)
	print(transicion("en-amarillo-intermitente", "rojo")) -> List(en-amarillo-intermitente, cambiar-a-rojo)
Caso de Error:
	transicion(8, "rojo") -> error: type mismatch;
        					found   : Int(8)
        					required: String
*/

/*
Funcion: timer
Naturaleza: pura
Estrategia: condicional
Impacto: no destrutiva
*/
def timer (segundos:Int): String = {
	segundos match {
		case segundos if ((segundos % 225) <= 89) => "rojo"
		case segundos if ((segundos % 225) <= 92) => "rojo-intermitente"
		case segundos if ((segundos % 225) <= 212) => "verde"
		case segundos if ((segundos % 225) <= 215) => "verde-intermitente"
		case segundos if ((segundos % 225) <= 221) => "amarillo"
		case _ => "amarillo-intermitente"
	}
}

def timer (segundos:Int): String = {segundos match {case segundos if ((segundos % 225) <= 89) => "rojo"; case segundos if ((segundos % 225) <= 92) => "rojo-intermitente"; case segundos if ((segundos % 225) <= 212) => "verde"; case segundos if ((segundos % 225) <= 215) => "verde-intermitente"; case segundos if ((segundos % 225) <= 221) => "amarillo"; case _ => "amarillo-intermitente"}}

/*
CASOS DE PRUEBA
Comportamiento Normal:
	print(timer(1781245161)) -> rojo
	print(timer(1781245215)) -> rojo-intermitente 
	print(timer(1781245268)) -> verde 
	print(timer(1781245338)) -> verde-intermitente
	print(timer(1781245567)) -> amarillo
	print(timer(1781245573)) -> amarillo-intermitente

Caso de Error:
	print(timer("a")) ->	error: type mismatch;
        					found   : String("a")
        					required: Int
*/

/*
Objeto singlenton Semaforo, contiene transicion y timer como metodos propios
*/
object Semaforo {

		def transicion (coloractual: String, cambiara: String): List[String] = {
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
				case _ => "amarillo-intermitente"
			}
		}
}
