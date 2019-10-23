model model4

global {
    int nb_people <- 2147;
    int nb_infected_init <- 5;
    float step <- 5 #mn;
    file roads_shapefile <- file("../includes/roads.shp");
    file buildings_shapefile <- file("../includes/buildings.shp");
    geometry shape <- envelope(roads_shapefile);    
    graph road_network;
    
    
    int nb_people_infected <- nb_infected_init update: people count (each.is_infected);
    int nb_people_not_infected <- nb_people - nb_infected_init update: nb_people - nb_people_infected;
    float infected_rate update: nb_people_infected/nb_people;
    
    
    init{
        create road from: roads_shapefile;
        road_network <- as_edge_graph(road);        
        create building from: buildings_shapefile;
        create people number:nb_people {
            location <- any_location_in(one_of(building));              
        }
        ask nb_infected_init among people {
            is_infected <- true;
        }
    }
}

species people skills:[moving]{     
    float speed <- (2 + rnd(3)) #km/#h;
    bool is_infected <- false;
    point target;
    
    reflex stay when: target = nil {
        if flip(0.05) {
            target <- any_location_in (one_of(building));
        }
    }
        
    reflex move when: target != nil{
        do goto target:target on: road_network;
        if (location = target) {
            target <- nil;
        } 
    }

    reflex infect when: is_infected{
        ask people at_distance 10 #m {
            if flip(0.05) {
                is_infected <- true;
            }
        }
    }
    
    aspect circle {
        draw circle(10) color:is_infected ? #red : #green;
    }
    
    aspect geom3D {
        if target != nil {
            draw obj_file("../includes/people.obj", 90::{-1,0,0}) size: 5
            at: location + {0,0,7} rotate: heading - 90 color: is_infected ? #red : #green;
        }
    }
    
}

species road {
    aspect geom {
        draw shape color: #black;
    }
    aspect geom3D {
        draw line(shape.points, 2.0) color: #black;
    }
}

species building {
    aspect geom {
        draw shape color: #gray;
    }
    aspect geom3D {
        draw shape depth: 20 #m border: #black texture:["../includes/roof_top.png","../includes/texture.jpg"];
    }
}

experiment main type: gui {
    parameter "Nb people infected at init" var: nb_infected_init min: 1 max: 2147;

    output {
        monitor "Infected people rate" value: infected_rate;
        
        display map {
            species road aspect:geom;
            species building aspect:geom;
            species people aspect:circle;           
        }
        
        
        
        display chart_display refresh: every(10 #cycle) {
            chart "Disease spreading" type: series {
                data "susceptible" value: nb_people_not_infected color: #green;
                data "infected" value: nb_people_infected color: #red;
            }
        }
        display view3D type: opengl ambient_light: 80 {
            image "../includes/luneray.png" refresh:false; 
            species building aspect:geom3D refresh: false;
            species road aspect: geom3D refresh: false;
            species people aspect: geom3D ; 
        }
    }
}

