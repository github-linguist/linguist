(use 'penumbra.opengl)
(require '[penumbra.app :as app])

(defn init [state]
  (app/title! "Triangle")
  (clear-color 0.3 0.3 0.3 0)
  (shade-model :smooth)
  state)

(defn reshape [[x y w h] state]
  (ortho-view -30 30 -30 30 -30 30)
  (load-identity)
  (translate -15 -15)
  state)

(defn display [[dt time] state]
  (draw-triangles
    (color 1 0 0) (vertex 0 0)
    (color 0 1 0) (vertex 30 0)
    (color 0 0 1) (vertex 0 30)))

(app/start {:display display, :reshape reshape, :init init} {})
