model GoldBdi

global {
    int nb_mines <- 10; 
    int nbminer<-5;
    int nb_police <- 1;
    int fine <-0;
    market the_market;
    
    string mine_at_location <- "mine_at_location";
    string empty_mine_location <- "empty_mine_location";
    
    float step <- 10#mn;
    
    //possible predicates concerning miners
    predicate mine_location <- new_predicate(mine_at_location) ;
    predicate choose_goldmine <- new_predicate("choose a gold mine");
    predicate has_gold <- new_predicate("extract gold");
    predicate find_gold <- new_predicate("find gold") ;
    predicate sell_gold <- new_predicate("sell gold") ;
    predicate share_information <- new_predicate("share information") ;
    
    
    emotion joy <- new_emotion("joy");
    
    float inequality <- 0.0 update:standard_deviation(miner collect each.gold_sold);
    
    geometry shape <- square(20 #km);
    init
    {
        create market {
            the_market <- self; 
        }
        create goldmine number:nb_mines;
        create miner number:nbminer;
        create policeman number:nb_police;
    }
    
    reflex end_simulation when: sum(goldmine collect each.quantity) = 0 and empty(miner where each.has_belief(has_gold)){
        do pause;
        ask miner{
            write name + " : " +gold_sold;
        }
        write "**********************";
        write "fine : " + fine;
    }
}

species goldmine {
    int quantity <- rnd(1,20);
    aspect default
    {
        if (quantity = 0) {
            draw triangle(200) color: #gray border: #black; 
        } else {
            draw triangle(200 + quantity*50) color: #yellow border: #black; 
        }
     
    }
}

species market {
    int golds;
    aspect default
    {
      draw square(1000) color: #black ;
    }
}

species policeman skills: [moving] control:simple_bdi {
    predicate patroling <- new_predicate("patrolling");
    float viewdist <- 1000.0;
    miner agent_perceived <- nil;
    
    init {
        do add_desire(patroling);
    }
    
    perceive target:miner in: viewdist{
        enforcement law:"working" sanction:"sanctionToLaw";
        enforcement obligation:has_gold /*when:has_belief(has_gold)*/ sanction: "sanctionToObligation" reward:"rewardToObligation";
    }
    
    sanction sanctionToLaw{
        ask agent_perceived{
            thresholdLaw <- 0.0;
            gold_sold <- gold_sold-5;
        }
        fine <- fine +5;
    }
    
    sanction sanctionToObligation {
        ask agent_perceived{
            gold_sold <- gold_sold-3;
            do remove_intention(sell_gold,true);
            thresholdObligation <- self.thresholdObligation - 0.1;
        }
        fine <- fine + 3;
    }
    
    sanction rewardToObligation{
        ask agent_perceived{
            gold_sold <- gold_sold+2;
        }
        fine <- fine -2;
    }
    
    plan patrol intention: patroling{
        do wander;
    }
    
    aspect base{
        draw circle(viewdist) color: #blue depth:0.0;
    }
}

species miner skills: [moving] control:simple_bdi {
    
    float viewdist<-1000.0;
    float speed <- 2#km/#h;
    rgb mycolor<-rnd_color(255);
    point target;
    int gold_sold;
    int gold_transported<-0;
    agent agent_perceived<-nil;
    
    bool use_social_architecture <- true;
    bool use_emotions_architecture <- true;
    bool use_personality <- true;
    
    float openness <- gauss(0.5,0.12);
    float conscientiousness <- gauss(0.5,0.12);
    float extraversion <- gauss(0.5,0.12);
    float agreeableness <- gauss(0.5,0.12);
    float neurotism <- gauss(0.5,0.12);
    
    float plan_persistence <- 1.0;
    float intention_persistence <- 1.0;
    
    float thresholdLaw <- 1.0;
    float thresholdObligation <- 1.0;
    float thresholdNorm <- 0.5;
    
    init
    {
        do add_desire(find_gold);
    }
    
    perceive target:self{
        if(gold_transported>0){
            do add_belief(has_gold);
        } else {
            do remove_belief(has_gold);
        }
    }
    
    perceive target:miner in:viewdist {
        myself.agent_perceived<-self;
        socialize liking: point(mycolor.red, mycolor.green, mycolor.blue) distance_to point(myself.mycolor.red, myself.mycolor.green, myself.mycolor.blue) / ( 255) - 1;
        enforcement norm:"share_information" sanction:"sanctionToNorm" reward:"rewardToNorm";
    }
        
    sanction sanctionToNorm{
        do change_liking(agent_perceived,-0.1);
    }   
    
    sanction rewardToNorm{
        do change_liking(agent_perceived,0.1);
    }
        
    perceive target:goldmine where (each.quantity > 0) in:viewdist {
        focus id:mine_at_location var:location;
        ask myself {
            if (has_emotion(joy)) {do add_desire(predicate:share_information, strength: 5.0);}
            do remove_intention(find_gold, false);
        }
    }
    
    rule belief: has_gold new_desire: sell_gold strength: 3.0;
    
    law working belief: mine_location new_obligation: has_gold when:not has_obligation(has_gold) and not has_belief(has_gold) strength: 2.0 threshold:thresholdLaw;
    
    plan letsWander intention:find_gold 
    {
        do wander;
    }
    
    norm doingJob obligation:has_gold finished_when: has_belief(has_gold) threshold:thresholdObligation{
        if (target = nil) {
            do add_subintention(has_gold,choose_goldmine, true);
            do current_intention_on_hold();
        } else {
            do goto target: target ;
            if (target = location)  {
                goldmine current_mine<- goldmine first_with (target = each.location);
                if current_mine.quantity > 0 {
                    gold_transported <- gold_transported+1;
                    do add_belief(has_gold);
                    ask current_mine {quantity <- quantity - 1;}    
                } else {
                    do add_belief(new_predicate(empty_mine_location, ["location_value"::target]));
                    do remove_belief(new_predicate(mine_at_location, ["location_value"::target]));
                }
                target <- nil;
            }
        }   
    }
    
    plan getMoreGold intention:has_gold
    {
        if (target = nil) {
            do add_subintention(has_gold,choose_goldmine, true);
            do current_intention_on_hold();
        } else {
            do goto target: target ;
            if (target = location)  {
                goldmine current_mine<- goldmine first_with (target = each.location);
                if current_mine.quantity > 0 {
                    gold_transported <- 3;
                    do add_belief(has_gold);
                    ask current_mine {if(quantity>=3) {
                        quantity <- quantity - 3;
                    }else {
                        quantity <- 0;
                    } 
                    }   
                } else {
                    do add_belief(new_predicate(empty_mine_location, ["location_value"::target]));
                    do remove_belief(new_predicate(mine_at_location, ["location_value"::target]));
                }
                target <- nil;
            }
        }   
    }
    
    plan choose_closest_goldmine intention: choose_goldmine instantaneous: true{
        list<point> possible_mines <- get_beliefs_with_name(mine_at_location) collect (point(get_predicate(mental_state (each)).values["location_value"]));
        list<point> empty_mines <- get_beliefs_with_name(empty_mine_location) collect (point(get_predicate(mental_state (each)).values["location_value"]));
        possible_mines <- possible_mines - empty_mines;
        if (empty(possible_mines)) {
            do remove_intention(has_gold, true); 
        } else {
            target <- (possible_mines with_min_of (each distance_to self)).location;
        }
        do remove_intention(choose_goldmine, true); 
    }
    
    plan return_to_base intention: sell_gold when: has_belief(has_gold){
        do goto target: the_market ;
        if (the_market.location = location)  {
            do remove_belief(has_gold);
            do remove_intention(sell_gold, true);
            gold_sold <- gold_sold + gold_transported;
            gold_transported <- 0;
        }
    }
    
    norm share_information intention:share_information threshold:thresholdNorm instantaneous: true{
        list<miner> my_friends <- list<miner>((social_link_base where (each.liking > 0)) collect each.agent);
        loop known_goldmine over: get_beliefs_with_name(mine_at_location) {
            ask my_friends {
                do add_belief(known_goldmine);
            }
        }
        loop known_empty_goldmine over: get_beliefs_with_name(empty_mine_location) {
            ask my_friends {
                do add_belief(known_empty_goldmine);
            }
        }
        
        do remove_intention(share_information, true); 
    }
    
    plan share_information_to_friends intention: share_information instantaneous: true{
        list<miner> my_friends <- list<miner>((social_link_base where (each.liking > 0)) collect each.agent);
        loop known_goldmine over: get_beliefs_with_name(empty_mine_location) {
            ask my_friends {
                do add_belief(known_goldmine);
            }
        }       
        do remove_intention(share_information, true); 
    }

    aspect default {
      draw circle(200) color: mycolor border: #black depth: gold_sold;
    }
}


experiment GoldBdi type: gui {
    output {
        display map type: opengl
        {
            species market ;
            species goldmine ;
            species miner;
            species policeman aspect:base;
        }
        
    }
}

