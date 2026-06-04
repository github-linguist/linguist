;;Domain for cleaning floor tiles
;; A domain file for CMP2020M assignment 2018/2019
;;Myles Leslie 15614035

;; Define the name for this domain (needs to match the domain definition
;;   in the problem files)

(define (domain floor-tile)

	;; We only require some typing to make this plan faster. We can do without!
	(:requirements :typing)

	;; We have two types: robots and the tiles, both are objects
	(:types robot tile - object)

	;; define all the predicates as they are used in the probem files
	(:predicates  

    ;; described what tile a robot is at
    (robot-at ?r - robot ?x - tile)

    ;; indicates that tile ?x is above tile ?y
    (up ?x - tile ?y - tile)

    ;; indicates that tile ?x is below tile ?y
    (down ?x - tile ?y - tile)

    ;; indicates that tile ?x is right of tile ?y
    (right ?x - tile ?y - tile)

    ;; indicates that tile ?x is left of tile ?y
    (left ?x - tile ?y - tile)
    
    ;; indicates that a tile is clear (robot can move there)
    (clear ?x - tile)

    ;; indicates that a tile is cleaned
    (cleaned ?x - tile)
 	)

;; ACTIONS that need to be defined:
;; defining actions - if tile is cleaned below the robot
(:action clean-down

;; find what tile the robot is at and the where the tile it's going to is
 :parameters (?r - robot ?x - tile ?y - tile)
 
;; find if the tile is below the robot and if it's cleaned
 :precondition (and( down ?y ?x)(robot-at ?r ?x)  (not(cleaned ?y)))
 
 ;; if not cleaned then clean 
 :effect (and(cleaned ?y))
 )
 
(:action clean-up

;; find what tile the robot is at and the where the tile it's going to is
 :parameters (?r - robot ?x - tile ?y - tile)
 
 ;; find if the tile is above the robot and if it's cleaned
 :precondition (and ( up ?y ?x) (robot-at ?r ?x) (not(cleaned ?y)))
 
 ;; if not cleaned then clean
 :effect (and(cleaned ?y))
 )
 
;; all further actions are the same except the direction the robot is going
;;when there is a tile below
(:action down 

;; find what tile the robot is at and the where the tile it's going to is
	:parameters (?r - robot ?x - tile ?y - tile)

;; find if the tile is below the robot, checks if it's clear and if it's cleaned or not
  	:precondition (and  (down ?y ?x )(robot-at ?r ?x ) (clear ?y )(not(cleaned?y)))  
	
;; robot moves to tile if it's clear and doesn't if it isn't clear.
  	:effect (and (robot-at ?r ?y) (not(robot-at ?r ?x)) (not (clear ?y)) (clear ?x))
 )
 
 ;;when there is a tile above the robot
 (:action up   

;; find what tile the robot is at and the where the tile it's going to is
 	:parameters (?r - robot ?x - tile ?y - tile)  

;;find if the tile is above the robot, checks if it's clear and if it's cleaned or not
  	:precondition (and  (up ?y ?x )(robot-at ?r ?x ) (clear ?y )(not(cleaned?y)))

;;robot moves to tile if it's clear and doesn't if it isn't clear.	
  	:effect (and (robot-at ?r ?y) (not(robot-at ?r ?x)) (not (clear ?y)) (clear ?x))
 )
 
;;when there is a tile to the left
 (:action left
 
 ;; find what tile the robot is at and the where the tile it's going to is
	:parameters (?r - robot ?x - tile ?y - tile)  

;;find if the tile is to the left of the robot, checks if it's clear and if it's cleaned or not	
  	:precondition (and  (left ?y ?x )(robot-at ?r ?x ) (clear ?y )(not(cleaned?y)))  

;;robot moves to tile if it's clear and doesn't if it isn't clear.	
  	:effect (and (robot-at ?r ?y) (not(robot-at ?r ?x)) (not (clear ?y)) (clear ?x))
 )
 
;;when there is a tile to the right
 (:action right 
 
 ;; find what tile the robot is at and the where the tile it's going to is
	:parameters (?r - robot ?x - tile ?y - tile)
	
;;find if the tile is to the right of the robot, checks if it's clear and if it's cleaned or not
  	:precondition (and  (right ?y ?x )(robot-at ?r ?x ) (clear ?y )(not(cleaned?y))) 
	
;;robot moves to tile if it's clear and doesn't if it isn't clear.
  	:effect (and (robot-at ?r ?y) (not(robot-at ?r ?x)) (not (clear ?y)) (clear ?x))
 )
)
