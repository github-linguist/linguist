model prey_predator

global {
    int nb_preys_init <- 200;
    int nb_predators_init <- 20;
    float prey_max_energy <- 1.0;
    float prey_max_transfert <- 0.1 ;
    float prey_energy_consum <- 0.05;
    float predator_max_energy <- 1.0;
    float predator_energy_transfert <- 0.5;
    float predator_energy_consum <- 0.02;
    float prey_proba_reproduce <- 0.01;
    int prey_nb_max_offsprings <- 5; 
    float prey_energy_reproduce <- 0.5; 
    float predator_proba_reproduce <- 0.01;
    int predator_nb_max_offsprings <- 3;
    float predator_energy_reproduce <- 0.5;
    file map_init <- image_file("../images/predator_prey_raster_map.png");
    
    int nb_preys -> {length (prey)};
    int nb_predators -> {length (predator)};
    
    init {
        create prey number: nb_preys_init ; 
        create predator number: nb_predators_init ;
        ask vegetation_cell {
            color <- rgb (map_init at {grid_x,grid_y}) ;
            food <- 1 - (((color as list)[0]) / 255) ;
            foodProd <- food / 100 ; 
        }
    }
    
    reflex save_result when: (nb_preys > 0) and (nb_predators > 0){
        save ("cycle: "+ cycle + "; nbPreys: " + nb_preys
            + "; minEnergyPreys: " + ((prey as list) min_of each.energy)
            + "; maxSizePreys: " + ((prey as list) max_of each.energy) 
            + "; nbPredators: " + nb_predators           
            + "; minEnergyPredators: " + ((predator as list) min_of each.energy)          
            + "; maxSizePredators: " + ((predator as list) max_of each.energy)) 
            to: "results.txt" type: "text" ;
    }
    
    reflex stop_simulation when: (nb_preys = 0) or (nb_predators = 0) {
        do halt ;
    } 
}

species generic_species {
    float size <- 1.0;
    rgb color  ;
    float max_energy;
    float max_transfert;
    float energy_consum;
    float proba_reproduce ;
    float nb_max_offsprings;
    float energy_reproduce;
    image_file my_icon;
    vegetation_cell myCell <- one_of (vegetation_cell) ;
    float energy <- (rnd(1000) / 1000) * max_energy  update: energy - energy_consum max: max_energy ;
    
    init {
        location <- myCell.location;
    }
        
    reflex basic_move {
        myCell <- choose_cell();
        location <- myCell.location; 
    } 
    
    vegetation_cell choose_cell {
        return nil;
    }
        
    reflex die when: energy <= 0 {
        do die ;
    }
    
    reflex reproduce when: (energy >= energy_reproduce) and (flip(proba_reproduce)) {
        int nb_offsprings <- 1 + rnd(nb_max_offsprings -1);
        create species(self) number: nb_offsprings {
            myCell <- myself.myCell ;
            location <- myCell.location ;
            energy <- myself.energy / nb_offsprings ;
        }
        energy <- energy / nb_offsprings ;
    }
    
    aspect base {
        draw circle(size) color: color ;
    }
    aspect icon {
        draw my_icon size: 2 * size ;
    }
    aspect info {
        draw square(size) color: color ;
        draw string(energy with_precision 2) size: 3 color: #black ;
    }
}

species prey parent: generic_species {
    rgb color <- #blue;
    float max_energy <- prey_max_energy ;
    float max_transfert <- prey_max_transfert ;
    float energy_consum <- prey_energy_consum ;
    float proba_reproduce <- prey_proba_reproduce ;
    int nb_max_offsprings <- prey_nb_max_offsprings ;
    float energy_reproduce <- prey_energy_reproduce ;
    file my_icon <- file("../images/predator_prey_sheep.png") ;
        
    reflex eat when: myCell.food > 0 {
        float energy_transfert <- min([max_transfert, myCell.food]) ;
        myCell.food <- myCell.food - energy_transfert ;
        energy <- energy + energy_transfert ;
    }
    
    vegetation_cell choose_cell {
        return (myCell.neighbors) with_max_of (each.food);
    }
}
    
species predator parent: generic_species {
    rgb color <- #red ;
    float max_energy <- predator_max_energy ;
    float energy_transfert <- predator_energy_transfert ;
    float energy_consum <- predator_energy_consum ;
    list<prey> reachable_preys update: prey inside (myCell);
    float proba_reproduce <- predator_proba_reproduce ;
    int nb_max_offsprings <- predator_nb_max_offsprings ;
    float energy_reproduce <- predator_energy_reproduce ;
    file my_icon <- file("../images/predator_prey_wolf.png") ;
    
    reflex eat when: ! empty(reachable_preys) {
        ask one_of (reachable_preys) {
            do die ;
        }
        energy <- energy + energy_transfert ;
    }
    
    vegetation_cell choose_cell {
        vegetation_cell myCell_tmp <- shuffle(myCell.neighbors) first_with (!(empty (prey inside (each))));
        if myCell_tmp != nil {
            return myCell_tmp;
        } else {
            return one_of (myCell.neighbors);
        } 
    }
}
    
grid vegetation_cell width: 50 height: 50 neighbors: 4 {
    float maxFood <- 1.0 ;
    float foodProd <- (rnd(1000) / 1000) * 0.01 ;
    float food <- (rnd(1000) / 1000) max: maxFood update: food + foodProd ;
    rgb color <- rgb(int(255 * (1 - food)), 255, int(255 * (1 - food))) update: rgb(int(255 * (1 - food)), 255, int(255 *(1 - food))) ;
    list<vegetation_cell> neighbors  <- (self neighbors_at 2); 
}

experiment prey_predator type: gui {
    parameter "Initial number of preys: " var: nb_preys_init  min: 0 max: 1000 category: "Prey" ;
    parameter "Prey max energy: " var: prey_max_energy category: "Prey" ;
    parameter "Prey max transfert: " var: prey_max_transfert  category: "Prey" ;
    parameter "Prey energy consumption: " var: prey_energy_consum  category: "Prey" ;
    parameter "Initial number of predators: " var: nb_predators_init  min: 0 max: 200 category: "Predator" ;
    parameter "Predator max energy: " var: predator_max_energy category: "Predator" ;
    parameter "Predator energy transfert: " var: predator_energy_transfert  category: "Predator" ;
    parameter "Predator energy consumption: " var: predator_energy_consum  category: "Predator" ;
    parameter 'Prey probability reproduce: ' var: prey_proba_reproduce category: 'Prey' ;
    parameter 'Prey nb max offsprings: ' var: prey_nb_max_offsprings category: 'Prey' ;
    parameter 'Prey energy reproduce: ' var: prey_energy_reproduce category: 'Prey' ;
    parameter 'Predator probability reproduce: ' var: predator_proba_reproduce category: 'Predator' ;
    parameter 'Predator nb max offsprings: ' var: predator_nb_max_offsprings category: 'Predator' ;
    parameter 'Predator energy reproduce: ' var: predator_energy_reproduce category: 'Predator' ;
    
    output {
        display main_display {
            grid vegetation_cell lines: #black ;
            species prey aspect: icon ;
            species predator aspect: icon ;
        }
        display info_display {
            grid vegetation_cell lines: #black ;
            species prey aspect: info ;
            species predator aspect: info ;
        }
        display Population_information refresh:every(5#cycles) {
            chart "Species evolution" type: series size: {1,0.5} position: {0, 0} {
                data "number_of_preys" value: nb_preys color: #blue ;
                data "number_of_predator" value: nb_predators color: #red ;
            }
            chart "Prey Energy Distribution" type: histogram background: rgb("lightGray") size: {0.5,0.5} position: {0, 0.5} {
                data "]0;0.25]" value: prey count (each.energy <= 0.25) color:#blue;
                data "]0.25;0.5]" value: prey count ((each.energy > 0.25) and (each.energy <= 0.5)) color:#blue;
                data "]0.5;0.75]" value: prey count ((each.energy > 0.5) and (each.energy <= 0.75)) color:#blue;
                data "]0.75;1]" value: prey count (each.energy > 0.75) color:#blue;
            }
            chart "Predator Energy Distribution" type: histogram background: rgb("lightGray") size: {0.5,0.5} position: {0.5, 0.5} {
                data "]0;0.25]" value: predator count (each.energy <= 0.25) color: #red ;
                data "]0.25;0.5]" value: predator count ((each.energy > 0.25) and (each.energy <= 0.5)) color: #red ;
                data "]0.5;0.75]" value: predator count ((each.energy > 0.5) and (each.energy <= 0.75)) color: #red ;
                data "]0.75;1]" value: predator count (each.energy > 0.75) color: #red;
            }
        }
        monitor "Number of preys" value: nb_preys;
        monitor "Number of predators" value: nb_predators;
    }
}
