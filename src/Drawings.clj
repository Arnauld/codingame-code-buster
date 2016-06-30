(ns Drawings
  (:require [quil.core :as q]
            [Player :as p]))
;;------------------------------------------------------------------------------------------

(def pos (atom {:x (/ p/WIDTH 2)
                :y (/ p/HEIGHT 2)}))
(def v (atom {:vx 150 :vy 100}))
(def g (atom (p/generate-grid)))

;;------------------------------------------------------------------------------------------

(defn- s [x]
  "basic scaling..."
  (int (/ x 20)))

(defn setup []
  (q/frame-rate 10)                                         ;; Set framerate to 1 FPS
  (q/background 255))                                       ;; Set the background colour to

;; a nice shade of grey.
(defn draw-p []
  (q/stroke (q/random 255))                                 ;; Set the stroke colour to a random grey
  (q/stroke-weight (q/random 10))                           ;; Set the stroke thickness randomly
  (q/fill (q/random 255))                                   ;; Set the fill colour to a random grey

  (let [diam (q/random 100)                                 ;; Set the diameter to a value between 0 and 100
        x (q/random (q/width))                              ;; Set the x coord randomly within the sketch
        y (q/random (q/height))]                            ;; Set the y coord randomly within the sketch
    (q/ellipse x y diam diam)))                             ;; Draw a circle at x y with the correct diameter

(defn draw []
  (q/clear)
  (let [ex (+ (:x @pos) (:vx @v))
        ey (+ (:y @pos) (:vy @v))
        nvx (if (or (< ex 0) (< p/WIDTH ex)) (- (:vx @v)) (:vx @v))
        nvy (if (or (< ey 0) (< p/HEIGHT ey)) (- (:vy @v)) (:vy @v))

        e1 {:x ex :y ey}
        es [e1]
        grid (-> @g
                 (p/tick-cells)
                 (p/mark-visited-cells e1)
                 (p/compute-zones-of-interest))
        diam (p/grid-cell-diameter grid)]

    (reset! pos {:x ex :y ey})
    (reset! v {:vx nvx :vy nvy})
    (reset! g grid)
    (doseq [z (p/grid-cells grid)]
      (q/stroke 34)
      (q/stroke-weight 2)
      ;(q/no-fill)
      (q/fill (+ 160 (* 4 (max 0 (:visited z))))
              (- 160 (* 8 (max 0 (:visited z))))
              (- 160 (* 8 (max 0 (:visited z))))
              255)
      (q/ellipse (s (:x z))
                 (s (:y z))
                 (s diam)
                 (s diam)))
    (q/fill 200 100)
    (doseq [e es]
      (q/ellipse (s (:x e))
                 (s (:y e))
                 (s p/FOG_DIAMETER)
                 (s p/FOG_DIAMETER)))))

(q/defsketch example                                        ;; Define a new sketch named example
             :title "Oh so many grey circles"               ;; Set the title of the sketch
             :settings #(q/smooth 2)                        ;; Turn on anti-aliasing
             :setup setup                                   ;; Specify the setup fn
             :draw draw                                     ;; Specify the draw fn
             :size [(s p/WIDTH) (s p/HEIGHT)])              ;; You struggle to beat the golden ratio

