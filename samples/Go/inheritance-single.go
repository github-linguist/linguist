package main

type animal struct {
    alive bool
}

type dog struct {
    animal
    obedienceTrained bool
}

type cat struct {
    animal
    litterBoxTrained bool
}

type lab struct {
    dog
    color string
}

type collie struct {
    dog
    catchesFrisbee bool
}

func main() {
    var pet lab
    pet.alive = true
    pet.obedienceTrained = false
    pet.color = "yellow"
}
