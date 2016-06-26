(ns Player
  (:gen-class)
  (:import (clojure.lang LineNumberingPushbackReader)
           (java.io StringReader)))

;---------------------------------------------------------------
;                _   _ _ _ _   _
;          _   _| |_(_) (_) |_(_) ___  ___
;    _____| | | | __| | | | __| |/ _ \/ __|
;   |_____| |_| | |_| | | | |_| |  __/\__ \
;          \__,_|\__|_|_|_|\__|_|\___||___/
;---------------------------------------------------------------

(defn as-stream [s]
  (-> (StringReader. s) LineNumberingPushbackReader.))

(defn debug [& args]
  (binding [*out* *err*]
    (println args)))

(defn sqr [x]
  (* x x))

(defn- d-sqr [c1 c2]
  (sqr (- c1 c2)))

(defn d2 [e1 e2]
  (+ (d-sqr (:x e1) (:x e2))
     (d-sqr (:y e1) (:y e2))))

(defn abs [x]
  (if (neg? x) (- x)
               x))

;---------------------------------------------------------------
;       _      _                      _   _     _       _
;      | | ___| |_   _ __ ___   ___  | |_| |__ (_)_ __ | | __
; _____| |/ _ \ __| | '_ ` _ \ / _ \ | __| '_ \| | '_ \| |/ /
;|_____| |  __/ |_  | | | | | |  __/ | |_| | | | | | | |   <
;      |_|\___|\__| |_| |_| |_|\___|  \__|_| |_|_|_| |_|_|\_\
;
;---------------------------------------------------------------
(def MIN_DIST_BUST 900)
(def MAX_DIST_BUST 1760)
(def MIN_D2_BUST (sqr MIN_DIST_BUST))
(def MAX_D2_BUST (sqr MAX_DIST_BUST))
(def MAX_DIST_RELEASE 1600)
(def MAX_D2_RELEASE (sqr MAX_DIST_RELEASE))
(def MAX_DIST_MOVE 800)
(def HOME1 {:x 0 :y 0})
(def HOME2 {:x 16000 :y 9000})

(defn my-home [game]
  (if (= 0 (:myTeamId game))
    HOME1
    HOME2))

(defn my-busters [game entities]
  (let [myTeamId (:myTeamId game)]
    (filter #(= (:entityType %) myTeamId) entities)))

(defn ghosts [entities]
  (filter #(= (:entityType %) -1) entities))

(defn can-bust? [e1 e2]
  (let [ds (d2 e1 e2)]
    (and (<= ds MAX_D2_BUST)
         (<= MIN_D2_BUST ds))))

(defn must-release? [buster]
  (= 1 (:state buster)))

(defn can-release? [game buster]
  (or (<= (d2 HOME1 buster) MAX_D2_RELEASE)
      (<= (d2 HOME2 buster) MAX_D2_RELEASE)))

(defn action-release []
  {:type :release
   :msg  "go away ectoplasm!"})

(defn action-move
  ([e]
   (action-move (:x e) (:y e)))
  ([x y]
   {:type :move
    :x    x
    :y    y
    :msg  (str "go to " x "," y)}))

(defn action-move-random [e]
  (let [random (rand-int 360)
        theta (/ (* Math/PI random) 180)
        x (+ (:x e) (* MAX_DIST_MOVE (Math/cos theta)))
        y (+ (:y e) (* MAX_DIST_MOVE (Math/sin theta)))]
    {:type :move
     :x    x
     :y    y
     :msg  (str "random move to" x "," y)}))

(defn action-move-away [buster dst]
  (let [deltaX (- (:x dst) (:x buster))
        deltaY (- (:y dst) (:y buster))
        dist (Math/sqrt (+ (sqr deltaX) (sqr deltaY)))
        ux (/ deltaX dist)
        uy (/ deltaY dist)
        deltaDist (abs (- MIN_DIST_BUST dist))
        newX (+ (:x buster) (* deltaDist ux))
        newY (+ (:y buster) (* deltaDist uy))]
    {:type :move
     :x    newX
     :y    newY
     :msg  (str "one step aside from " (:entityId dst))}))

(defn action-move-closest-home [buster]
  (let [dh1 (d2 HOME1 buster)
        dh2 (d2 HOME2 buster)]
    (if (< dh1 dh2)
      (action-move HOME1)
      (action-move HOME2))))

(defn action-bust [e]
  {:type    :bust
   :ghostId (:entityId e)
   :msg     "be detroyed!"})

(defn closest [buster ghosts]
  (first (sort-by #(d2 buster %) ghosts)))

(defn closest-to-bust [buster ghosts]
  (first (sort-by #(abs (- MIN_D2_BUST (d2 buster %))) ghosts)))

(defn do-buster-think [game buster ghosts]
  (cond (must-release? buster)
        (if (can-release? game buster)
          (action-release)
          (action-move-closest-home buster))
        (empty? ghosts)
        (action-move-random buster)
        :else
        (let [ghosts-bustable (filter #(can-bust? buster %) ghosts)]
          (if (seq ghosts-bustable)
            (action-bust (first ghosts-bustable))
            (action-move-away buster (closest-to-bust buster ghosts))
            ))))

(defn do-think [game my-busters ghosts]
  (map #(do-buster-think game % ghosts) my-busters))

;---------------------------------------------------------------
;             _
;            (_) ___
;       _____| |/ _ \
;      |_____| | (_) |
;            |_|\___/
;---------------------------------------------------------------

(defn read-initialization-input [in]
  {:bustersPerPlayer (read in)
   :ghostCount       (read in)
   :myTeamId         (read in)})

; Send your busters out into the fog to trap ghosts and bring them home!
(defn read-game-turn [game in]
  ; nb-entities: the number of busters and ghosts visible to you
  (let [nb-entities (read in)]
    (loop [i nb-entities
           entities []]
      (if (> i 0)
        (let [entity {:entityId   (read in)                 ; entityId: buster id or ghost id
                      :x          (read in)                 ; x, y: position of this buster / ghost
                      :y          (read in)
                      :entityType (read in)                 ; entityType: the team id if it is a buster, -1 if it is a ghost.
                      :state      (read in)                 ; state: For busters: 0=idle, 1=carrying a ghost.
                      :value      (read in)}]               ; value: For busters: Ghost id being carried. For ghosts: number of busters attempting to trap this ghost.
          (recur (dec i) (conj entities entity)))
        entities))))

(defn display-action [action]
  (cond
    (= :release (:type action)) (println "RELEASE" (:msg action))
    (= :move (:type action)) (println "MOVE" (int (:x action)) (int (:y action)) (:msg action))
    (= :bust (:type action)) (println "BUST" (:ghostId action) (:msg action))))

(defn display-actions [actions]
  (doseq [action actions]
    (display-action action)))

(defn -main [& args]
  (let [game (read-initialization-input *in*)]
    (debug "game/input" game)
    ; bustersPerPlayer: the amount of busters you control
    ; ghostCount: the amount of ghosts on the map
    ; myTeamId: if this is 0, your base is on the top left of the map, if it is one, on the bottom right
    (loop [round 1]
      (debug "Round #" round)
      (let [entities (read-game-turn game *in*)
            myBusters (my-busters game entities)
            ghosts (ghosts entities)
            actions (do-think game myBusters ghosts)]
        ;(debug "game/entities" entities)
        (doseq [e entities]
          (debug "entity>" e))
        ;(debug "actions" actions)
        (doseq [a actions]
          (debug "action>" a))
        (display-actions actions)
        (recur (inc round))))))