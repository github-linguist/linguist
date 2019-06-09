class Pirata {
	const items = []
	var dinero
	var nivelEbriedad

	constructor(itemsPirata) {
		items = itemsPirata
	}

	method items() {
		return items
	}

	method dinero() {
		return dinero
	}

	method nivelEbriedad() {
		return nivelEbriedad
	}
}

class PirataAbstemio inherits Pirata {
  override method nivelEbriedad() = 0
}

object buscarTesoro {
	method puedeSerCumplidaPor(pirata) {
		return (pirata.items().contains("brujula") || pirata.items().contains("mapa") || pirata.items().contains("botellaGrogXD")) &&
			(pirata.dinero() <= 5) && (pirata.nivelEbriedad() < 3)
	}
}

object serLeyenda {
	const itemObligatorio = "dienteDeOro"

	method puedeSerCumplidaPor(pirata) {
		return pirata.items().size() == 7 && pirata.items().contains(itemObligatorio)
	}

}

object saquear {
	//var victima = barcoPirata
	//var saqueador = barcoPirata2
	var cantidadDeDineroMaxima = 5

	method puedeSerCumplidaPor(pirata) {
		//return pirata.dinero() < cantidadDeDineroMaxima && victima.esVulnerablePara(pirata)
	}
}

object main {

	var p = new Pirata(["brujula", "cuchillo", "cuchillo"])
	//var barco = new Barco()

	method p() {
		return p
	}


}
