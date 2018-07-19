#!/usr/bin/env python
################################################################################################
# import necessary modules
# ------------------------
from numpy import *
import turtle
################################################################################################
#	Functions defining the drawing actions (used by the function DrawSierpinskiTriangle).
#	-------------------------------------------------------------------------------------
def Left(turn, point, fwd, angle, turt):
	turt.left(angle)
	return [turn, point, fwd, angle, turt]
def Right(turn, point, fwd, angle, turt):
	turt.right(angle)
	return [turn, point, fwd, angle, turt]
def Forward(turn, point, fwd, angle, turt):
	turt.forward(fwd)
	return [turn, point, fwd, angle, turt]
################################################################################################
#		The drawing function
#		--------------------
#
# level		level of Sierpinski triangle (minimum value = 1)
# ss		screensize (Draws on a screen of size ss x ss. Default value = 400.)
#-----------------------------------------------------------------------------------------------
def DrawSierpinskiTriangle(level, ss=400):
	# typical values
	turn = 0		# initial turn (0 to start horizontally)
	angle=60.0 		# in degrees

	# Initialize the turtle
	turtle.hideturtle()
	turtle.screensize(ss,ss)
	turtle.penup()
	turtle.degrees()

	# The starting point on the canvas
	fwd0         = float(ss)
	point=array([-fwd0/2.0, -fwd0/2.0])

	# Setting up the Lindenmayer system
	# Assuming that the triangle will be drawn in the following way:
	#	1.) Start at a point
	#	2.) Draw a straight line - the horizontal line (H)
	#	3.) Bend twice by 60 degrees to the left (--)
	#	4.) Draw a straight line - the slanted line (X)
	#	5.) Bend twice by 60 degrees to the left (--)
	#	6.) Draw a straight line - another slanted line (X)
	# 		This produces the triangle in the first level. (so the axiom to begin with is H--X--X)
	#	7.) For the next level replace each horizontal line using
	#		X->XX
	#		H -> H--X++H++X--H
	#			The lengths will be halved.


	decode    = {'-':Left, '+':Right, 'X':Forward, 'H':Forward}
	axiom     = 'H--X--X'

	# Start the drawing
	turtle.goto(point[0], point[1])
	turtle.pendown()
	turtle.hideturtle()
	turt=turtle.getpen()
	startposition=turt.clone()

	# Get the triangle in the Lindenmayer system
	fwd       = fwd0/(2.0**level)
	path      = axiom
	for i in range(0,level):
		path=path.replace('X','XX')
		path=path.replace('H','H--X++H++X--H')

	# Draw it.
	for i in path:
		[turn, point, fwd, angle, turt]=decode[i](turn, point, fwd, angle, turt)
################################################################################################

DrawSierpinskiTriangle(5)
