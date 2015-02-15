// triangle displays a smooth shaded triangle with OpenGL.
package main

import (
	"log"
	"runtime"
	"time"

	gl "github.com/chsc/gogl/gl21"
	"github.com/mewmew/glfw/win"
)

// Window dimensions.
const (
	Width  = 640
	Height = 480
)

func main() {
	// OpenGL requires a dedicated OS thread.
	runtime.LockOSThread()
	defer runtime.UnlockOSThread()

	// Open window with the specified dimensions.
	err := win.Open(Width, Height)
	if err != nil {
		log.Fatalln(err)
	}
	defer win.Close()

	// Initiate viewport.
	resize(Width, Height)

	// Register that we are interested in receiving close and resize events.
	win.EnableCloseChan()
	win.EnableResizeChan()

	// 60 frames per second.
	c := time.Tick(time.Second / 60)

	// Event loop.
	for {
		select {
		case <-win.CloseChan:
			return
		case e := <-win.ResizeChan:
			resize(e.Width, e.Height)
		case <-c:
			draw()
		}
	}
}

// resize resizes the window to the specified dimensions.
func resize(width, height int) {
	gl.Viewport(0, 0, gl.Sizei(width), gl.Sizei(height))
	gl.MatrixMode(gl.PROJECTION)
	gl.LoadIdentity()
	gl.Ortho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0)
	gl.MatrixMode(gl.MODELVIEW)
}

// draw draws the triangle.
func draw() {
	gl.ClearColor(0.3, 0.3, 0.3, 0.0)
	gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

	gl.ShadeModel(gl.SMOOTH)

	gl.LoadIdentity()
	gl.Translatef(-15.0, -15.0, 0.0)

	gl.Begin(gl.TRIANGLES)

	gl.Color3f(1.0, 0.0, 0.0)
	gl.Vertex2f(0.0, 0.0)

	gl.Color3f(0.0, 1.0, 0.0)
	gl.Vertex2f(30.0, 0.0)

	gl.Color3f(0.0, 0.0, 1.0)
	gl.Vertex2f(0.0, 30.0)

	gl.End()

	win.SwapBuffers()
}
