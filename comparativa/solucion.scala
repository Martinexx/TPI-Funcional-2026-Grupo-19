/*
Funcion: timer
Naturaleza: pura
Estrategia: utilizacion de condicionales para determinar el rango
Impacto: no destrutiva
*/
def timer (segundos:Int): String = {
segundos match {
	case segundos if ((segundos % 216) <= 90) => "rojo"
	case segundos if ((segundos % 216) <= 210) => "verde"
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
	case segundos if ((segundos % 216) <= 87) => "rojo"
	case segundos if ((segundos % 216) <= 90) => "rojo-intermitente"
	case segundos if ((segundos % 216) <= 207) => "verde"
	case segundos if ((segundos % 216) <= 210) => "verde-intermitente"
	case segundos if ((segundos % 216) <= 213) => "amarillo"
	case _ => "amarillo-intermitente"
}
}
/*
casos de prueba:
caso normal: 304 - "rojo-intermitente" 
caso error: cualquier parametro que no sea entero

*/
