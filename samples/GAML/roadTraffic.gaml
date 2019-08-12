model tutorial_gis_city_traffic

global {
    file shape_file_buildings <- file("../includes/building.shp");
    file shape_file_roads <- file("../includes/road.shp");
    file shape_file_bounds <- file("../includes/bounds.shp");
    geometry shape <- envelope(shape_file_bounds);
    float step <- 10 #mn;
    int nb_people <- 100;
    int current_hour update: (time / #hour) mod 24;
    int min_work_start <- 6;
    int max_work_start <- 8;
    int min_work_end <- 16; 
    int max_work_end <- 20; 
    float min_speed <- 1.0 #km / #h;
    float max_speed <- 5.0 #km / #h; 
    float destroy <- 0.02;
    int repair_time <- 2 ;
    graph the_graph;
    
    init {
        create building from: shape_file_buildings with: [type::string(read ("NATURE"))] {
            if type="Industrial" {
                color <- #blue ;
            }
        }
        create road from: shape_file_roads ;
        map<road,float> weights_map <- road as_map (each:: (each.destruction_coeff * each.shape.perimeter));
        the_graph <- as_edge_graph(road) with_weights weights_map;
        
        
        list<building> residential_buildings <- building where (each.type="Residential");
        list<building>  industrial_buildings <- building  where (each.type="Industrial") ;
        create people number: nb_people {
            speed <- min_speed + rnd (max_speed - min_speed) ;
            start_work <- min_work_start + rnd (max_work_start - min_work_start) ;
            end_work <- min_work_end + rnd (max_work_end - min_work_end) ;
            living_place <- one_of(residential_buildings) ;
            working_place <- one_of(industrial_buildings) ;
            objective <- "resting";
            location <- any_location_in (living_place); 
        }
    }
    
    reflex update_graph{
        map<road,float> weights_map <- road as_map (each:: (each.destruction_coeff * each.shape.perimeter));
        the_graph <- the_graph with_weights weights_map;
    }
    reflex repair_road when: every(repair_time #hour / step) {
        road the_road_to_repair <- road with_max_of (each.destruction_coeff) ;
        ask the_road_to_repair {
            destruction_coeff <- 1.0 ;
        }
    }
}

species building {
    string type; 
    rgb color <- #gray  ;
    
    aspect base {
        draw shape color: color ;
    }
}

species road  {
    float destruction_coeff <- 1 + ((rnd(100))/ 100.0) max: 2.0;
    int colorValue <- int(255*(destruction_coeff - 1)) update: int(255*(destruction_coeff - 1));
    rgb color <- rgb(min([255, colorValue]),max ([0, 255 - colorValue]),0)  update: rgb(min([255, colorValue]),max ([0, 255 - colorValue]),0) ;
    
    aspect base {
        draw shape color: color ;
    }
}

species people skills:[moving] {
    rgb color <- #yellow ;
    building living_place <- nil ;
    building working_place <- nil ;
    int start_work ;
    int end_work  ;
    string objective ; 
    point the_target <- nil ;
        
    reflex time_to_work when: current_hour = start_work and objective = "resting"{
        objective <- "working" ;
        the_target <- any_location_in (working_place);
    }
        
    reflex time_to_go_home when: current_hour = end_work and objective = "working"{
        objective <- "resting" ;
        the_target <- any_location_in (living_place); 
    } 
     
    reflex move when: the_target != nil {
        path path_followed <- self goto [target::the_target, on::the_graph, return_path:: true];
        list<geometry> segments <- path_followed.segments;
        loop line over: segments {
            float dist <- line.perimeter;
            ask road(path_followed agent_from_geometry line) { 
                destruction_coeff <- destruction_coeff + (destroy * dist / shape.perimeter);
            }
        }
        if the_target = location {
            the_target <- nil ;
        }
    }
    
    aspect base {
        draw circle(10) color: color;
    }
}

experiment road_traffic type: gui {
    parameter "Shapefile for the buildings:" var: shape_file_buildings category: "GIS" ;
    parameter "Shapefile for the roads:" var: shape_file_roads category: "GIS" ;
    parameter "Shapefile for the bounds:" var: shape_file_bounds category: "GIS" ;
    parameter "Number of people agents" var: nb_people category: "People" ;
    parameter "Earliest hour to start work" var: min_work_start category: "People" min: 2 max: 8;
    parameter "Latest hour to start work" var: max_work_start category: "People" min: 8 max: 12;
    parameter "Earliest hour to end work" var: min_work_end category: "People" min: 12 max: 16;
    parameter "Latest hour to end work" var: max_work_end category: "People" min: 16 max: 23;
    parameter "minimal speed" var: min_speed category: "People" min: 0.1 #km/#h ;
    parameter "maximal speed" var: max_speed category: "People" max: 10 #km/#h;
    parameter "Value of destruction when a people agent takes a road" var: destroy category: "Road" ;
    parameter "Number of hours between two road repairs" var: repair_time category: "Road" ;
    
    output {
        display city_display type:opengl {
            species building aspect: base ;
            species road aspect: base ;
            species people aspect: base ;
        }
        display chart_display refresh:every(10#cycles) { 
            chart "Road Status" type: series size: {1, 0.5} position: {0, 0} {
                data "Mean road destruction" value: mean (road collect each.destruction_coeff) style: line color: #green ;
                data "Max road destruction" value: road max_of each.destruction_coeff style: line color: #red ;
            }
            chart "People Objectif" type: pie style: exploded size: {1, 0.5} position: {0, 0.5}{
                data "Working" value: people count (each.objective="working") color: #magenta ;
                data "Resting" value: people count (each.objective="resting") color: #blue ;
            }
        }
    }
}

