(define (domain search-and-rescure-scenario)
	
	(:requirements :strips :typing :fluents :disjunctive-preconditions :durative-actions)

	(:types drone package location dronezones)

	(:constants Area1 Area2 Area3 Area4 - location)

	(:functions 
		(total-cost)
	)

	(:predicates
		(drone-empty ?d - drone)
		(holding ?d - drone ?p - package)
		(at-drone ?d - drone ?l - location)
		(at-package ?p - package ?l - location)
		(drone-ground ?d - drone) 
		(drone-fly ?d - drone) 
		(location-available ?l - location)
		(location-available-drone ?d - drone ?l -location)
		
	)

	(:durative-action TAKEOFF
		:parameters (?d - drone ?l - location)
		:duration ( = ?duration 5)
		:condition (and (at start (drone-ground ?d))
					(at start(location-available-drone ?d ?l))
					(over all(at-drone ?d ?l))
					(at start(at-drone ?d ?l))
					) 
		:effect (and (at start(not (drone-ground ?d)))
					 (at end(drone-fly ?d))
					 (at start(not (location-available ? l)))
					 (at start(not(location-available-drone ?d ?l)))
					 (at start(increase (total-cost) 10))
					 )
	)
	
	(:durative-action LAND
		:parameters (?d - drone ?l - location)
		:duration ( = ?duration 5)
		:condition (and (at start(drone-fly ?d))
						(at start(location-available-drone ?d ?l))
						(over all(at-drone ?d ?l))
						(at start(at-drone ?d ?l))
					) 
		:effect  (and (at end(drone-ground ?d))
					(at start(not (drone-fly ?d)))
					(at start(increase (total-cost) 10))
					)
					 
	)

	
	(:durative-action LOAD
		:parameters (?d - drone ?p - package ?l - location)
		:duration (= ?duration 5)
		:condition (and (over all(at-drone ?d ?l))
						(at start(drone-empty ?d))
						(at start(at-package ?p ?l))
						(over all(drone-ground ?d)))
		:effect (and (at end(holding ?d ?p))
					 (at start(not (drone-empty ?d)))
					 (at start(not (location-available ? l)))
					 (at start(not (at-package ?p ?l)))
					 (at start(increase (total-cost) 10))
					 )
	)

	(:durative-action UNLOAD
		:parameters (?d - drone ?p - package ?l - location)
		:duration (= ?duration 5)
		:condition (and (over all(at-drone ?d ?l))
						(at start(holding ?d ?p))
						(over all(drone-ground ?d)))
		:effect (and (at start(not (holding ?d ?p)))
					 (at end(drone-empty ?d))
					 (at end(at-package ?p ?l))
					 (at start(increase (total-cost) 10))
					 )
	)

	(:durative-action MOVE
		:parameters (?d - drone ?from ?to - location)
		:duration (= ?duration 5)
		:condition (and (at start(at-drone ?d ?from))
							(over all(drone-fly ?d))
							(at start(location-available ?to))
							)
		:effect (and (at start(not(at-drone ?d ?from)))
					 (at end(at-drone ?d ?to))
					 (at start(not (location-available ?to)))
					 (at start(location-available ?from))
					 (at end(location-available-drone ?d ?to))
					 (at start(increase (total-cost) 10))
					 )
	)
)
