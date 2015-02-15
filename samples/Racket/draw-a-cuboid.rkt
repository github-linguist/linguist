#lang racket/gui
(require sgl/gl)

; Macro to delimit and automatically end glBegin - glEnd contexts.
(define-syntax-rule (gl-begin-end Vertex-Mode statement ...)
  (let () (glBegin Vertex-Mode) statement ... (glEnd)))

(define (resize w h)
  (glViewport 0 0 w h))

(define (draw-opengl x y z)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glEnable GL_DEPTH_TEST)
  (glClear GL_COLOR_BUFFER_BIT)
  (glClear GL_DEPTH_BUFFER_BIT)

  (define max-axis (add1 (max x y z)))

  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho (/ (- max-axis) 2) max-axis (/ (- max-axis) 2) max-axis (/ (- max-axis) 2) max-axis)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glRotatef -45 1.0 0.0 0.0)
  (glRotatef 45 0.0 1.0 0.0)

  (gl-begin-end GL_QUADS
                (glColor3f 0 0 1)
                (glVertex3d x 0.0 z)
                (glVertex3d x y z)
                (glVertex3d x y 0.0)
                (glVertex3d x 0.0 0.0))
  (gl-begin-end GL_QUADS
                (glColor3f 1 0 0)
                (glVertex3d x 0.0 0.0)
                (glVertex3d x y 0.0)
                (glVertex3d 0.0 y 0.0)
                (glVertex3d 0.0 0.0 0.0))
  (gl-begin-end GL_QUADS
                (glColor3f 0 1 0)
                (glVertex3d x y 0.0)
                (glVertex3d x y z)
                (glVertex3d 0.0 y z)
                (glVertex3d 0.0 y 0.0)))

(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (init-field (x 2) (y 3) (z 4))

    (define/override (on-paint)
      (with-gl-context
        (lambda ()
          (draw-opengl x y z)
          (swap-gl-buffers))))

    (define/override (on-size width height)
      (with-gl-context
        (lambda ()
          (resize width height))))

    (super-instantiate () (style '(gl)))))

(define win (new frame% (label "Racket Draw a cuboid") (min-width 300) (min-height 300)))
(define gl  (new my-canvas% (parent win) (x 2) (y 3) (z 4)))

(send win show #t)
