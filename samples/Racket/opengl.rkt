#lang racket/gui
(require sgl/gl)

(define (resize w h)
  (glViewport 0 0 w h))

(define (draw-opengl)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)

  (glShadeModel GL_SMOOTH)

  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho 0.0 1.0 0.0 1.0 -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  (glBegin GL_TRIANGLES)
  (glColor3f 1 0 0)
  (glVertex3d 0.25 0.25 0.0)
  (glColor3f 0 1 0)
  (glVertex3d 0.75 0.25 0.0)
  (glColor3f 0 0 1)
  (glVertex3d 0.75 0.75 0.0)
  (glEnd))


(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define/override (on-paint)
      (with-gl-context (λ() (draw-opengl) (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context (λ() (resize width height))))
    (super-instantiate () (style '(gl)))))

(define win (new frame% [label "Racket Rosetta Code OpenGL example"]
                        [min-width 200] [min-height 200]))
(define gl  (new my-canvas% [parent win]))

(send win show #t)
