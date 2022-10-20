(define (problem search-and-rescure-scenario_task)
(:domain search-and-rescure-scenario)
(:objects
    drone1 drone2 drone3 drone4 - drone
    RZ1 RZ2 RZ3 RZ4 - location
    water1 water2 food1 food2 medicine1  - package
)
(:init
    (at-drone drone1 Area1)
    (at-drone drone2 Area2)
    (at-drone drone3 Area3)
    (at-drone drone4 Area4)

    (at-package water1 Area1)
    (at-package water2 Area1)
    (at-package food1 Area2)
    (at-package food2 Area3)
    (at-package medicine1 Area4)

    (drone-empty drone1)
    (drone-empty drone2)
    (drone-empty drone3)
    (drone-empty drone4)

    (drone-ground drone1)
    (drone-ground drone2)
    (drone-ground drone3)
    (drone-ground drone4)

    (location-available-drone drone1 Area1)
    (location-available-drone drone1 Area2)
    (location-available-drone drone1 Area3)
    (location-available-drone drone1 Area4)
    
    (location-available-drone drone2 Area1)
    (location-available-drone drone2 Area2)
    (location-available-drone drone2 Area3)
    (location-available-drone drone2 Area4)
    
    (location-available-drone drone3 Area1)
    (location-available-drone drone3 Area2)
    (location-available-drone drone3 Area3)
    (location-available-drone drone3 Area4)
    
    (location-available-drone drone4 Area1)
    (location-available-drone drone4 Area2)
    (location-available-drone drone4 Area3)
    (location-available-drone drone4 Area4)

    (location-available Area1)
    (location-available Area2)
    (location-available Area3)
    (location-available Area4)
    (location-available RZ1)
    (location-available RZ2)
    (location-available RZ3)
    (location-available RZ4)

    (= (total-cost) 0)
)
(:goal (and
    (at-package water1 RZ1)
    (at-package water2 RZ3)
    (at-package medicine1 RZ2)
    (at-package food2 RZ2)
    (at-package food1 RZ4)
    (at-drone drone1 Area1)
    (at-drone drone2 Area2)
    (drone-ground drone1)
    (drone-ground drone2)
    (at-drone drone3 Area3)
    (at-drone drone4 Area4)
    (drone-ground drone3)
    (drone-ground drone4)
))

(:metric minimize (total-cost))

)
