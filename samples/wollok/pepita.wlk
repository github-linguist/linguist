
object pepita {
	var energia = 100
	var ubicacion = 0
	
	method energia() = energia
	
	method volar(km) {
		console.println("")
		energia -= (2 * km) + 10
	}
	
	method comer(comida) {
		energia += comida.energia()
	}
	
	method ubicacion() = ubicacion
	
	method volarA(lugar) {
		energia -= ubicacion - lugar.ubicacion()
		ubicacion = lugar.ubicacion()
	} 
	
	method puedeIrA(lugar) {

	}
	
}

object alpiste {
	val energia = 10
	
	method energia() = energia
}

object pepona {
	var energia = 50
	
	method comer(comida) {
		energia += comida.energia()
	}
	
	method energia() = energia
}

object otraPepita {
	var energia = 0
	
	method comer(comida) {
		energia += comida.energia()
	}
	
	method energia() = energia
}

object golondrina1 {
	var energia = 0
	
	method comer(comida) {
		energia += comida.energia()
	}
	
	method energia() = energia
}


