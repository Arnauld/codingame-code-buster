(ns Drawings
  (:require [quil.core :as q]
            [Player :as p]))

(defn s [x] (int (/ x 20)))

(defn setup []
  (q/frame-rate 1)                                          ;; Set framerate to 1 FPS
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
  (doseq [z (p/generate-zones)]
    (q/stroke 34)
    (q/stroke-weight 4)
    ;(q/no-fill)
    (q/fill 160 100)
    (q/ellipse (s (:x z))
               (s (:y z))
               (s p/FOG_DIAMETER)
               (s p/FOG_DIAMETER))))

(q/defsketch example                                        ;; Define a new sketch named example
             :title "Oh so many grey circles"               ;; Set the title of the sketch
             :settings #(q/smooth 2)                        ;; Turn on anti-aliasing
             :setup setup                                   ;; Specify the setup fn
             :draw draw                                     ;; Specify the draw fn
             :size [(s p/WIDTH) (s p/HEIGHT)])              ;; You struggle to beat the golden ratio

