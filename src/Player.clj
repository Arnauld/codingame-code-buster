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
(def WIDTH 16001)
(def HEIGHT 9001)
(def MIN_DIST_BUST 900)
(def MAX_DIST_BUST 1760)
(def MIN_D2_BUST (sqr MIN_DIST_BUST))
(def MAX_D2_BUST (sqr MAX_DIST_BUST))
(def MAX_DIST_RELEASE 1600)
(def MAX_D2_RELEASE (sqr MAX_DIST_RELEASE))
(def MAX_DIST_MOVE 800)
(def MAX_DIST_STUN 1760)
(def MAX_D2_STUN (sqr MAX_DIST_STUN))
(def FOG_RADIUS 2200)
(def FOG_RADIUS2 (sqr FOG_RADIUS))
(def HOME1 {:x 0 :y 0})
(def HOME2 {:x 16000 :y 9000})

(defn closest [buster positionables]
  (first (sort-by #(d2 buster %) positionables)))

(defn closest-to-bust [buster ghosts]
  (first (sort-by #(abs (- MIN_D2_BUST (d2 buster %))) ghosts)))

(defn generate-zones []
  (let [DIAMETER (* 2 FOG_RADIUS)]
    (for [x (range 0 (/ WIDTH FOG_RADIUS))
          y (range 0 (/ HEIGHT FOG_RADIUS))]
      {:x (+ FOG_RADIUS (* x FOG_RADIUS))
       :y (+ FOG_RADIUS (* y FOG_RADIUS))})))

(defn init-ia-data []
  {:unvisited-zones (generate-zones)
   :ghosts          []})

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
  ([e ^String msg]
   (action-move (:x e) (:y e) msg))
  ([x y ^String msg]
   {:type :move
    :x    (int x)
    :y    (int y)
    :msg  msg}))

(defn action-move-random [ia-data e]
  (let [ghosts (:ghosts ia-data)
        unvisited (:unvisited-zones ia-data)
        closest-ghost (closest e ghosts)]
    (if closest-ghost
      [(-> ia-data
           (assoc :ghosts (-> (remove #(= % closest-ghost) ghosts)
                              (conj (assoc closest-ghost :buster (:entityId e))))))
       (action-move closest-ghost "seen ghost this way!")]
      (let [closest-zone (closest e unvisited)]
        (debug "closest unvisited zone" closest-zone)
        (if closest-zone
          [(-> ia-data
               (assoc :unvisited-zones (-> (remove #(= % closest-zone) unvisited))))
           (action-move closest-zone "do not this!")]
          (let [random (rand-int 360)
                theta (/ (* Math/PI random) 180)
                x (+ (:x e) (* MAX_DIST_MOVE (Math/cos theta)))
                y (+ (:y e) (* MAX_DIST_MOVE (Math/sin theta)))]
            [ia-data (action-move x y "random move")]))))))

(defn action-move-away [buster dst]
  (let [deltaX (- (:x dst) (:x buster))
        deltaY (- (:y dst) (:y buster))
        dist (Math/sqrt (+ (sqr deltaX) (sqr deltaY)))
        ux (/ deltaX dist)
        uy (/ deltaY dist)
        deltaDist (abs (- MIN_DIST_BUST dist))
        newX (+ (:x buster) (* deltaDist ux))
        newY (+ (:y buster) (* deltaDist uy))]
    (action-move newX newY "one step aside")))

(defn action-move-to-release [buster]
  (let [dh1 (d2 HOME1 buster)
        dh2 (d2 HOME2 buster)]
    (if (< dh1 dh2)
      (action-move HOME1 "let's trash this")
      (action-move HOME2 "let's trash this"))))

(defn action-bust [e]
  {:type    :bust
   :ghostId (:entityId e)
   :msg     "Zzzzapppppp!"})


(defn do-buster-think [game ia-data buster ghosts]
  (cond (must-release? buster)
        (if (can-release? game buster)
          [ia-data (action-release)]
          [ia-data (action-move-to-release buster)])
        (empty? ghosts)
        (action-move-random ia-data buster)
        :else
        (let [ghosts-bustable (filter #(can-bust? buster %) ghosts)]
          (if (seq ghosts-bustable)
            [ia-data (action-bust (first ghosts-bustable))]
            [ia-data (action-move-away buster (closest-to-bust buster ghosts))]
            ))))

(defn update-ia-data [ia-data my-busters ghosts]
  (let [unvisited (:unvisited-zones ia-data)
        visited-zones (reduce (fn [zs buster]
                                (into zs (filter #(< (d2 buster %) 50) unvisited)))
                              #{} my-busters)
        seen-ghosts (:ghosts ia-data)
        ; --- remove ghosts that should currently be seen, in case there were zapped
        ghosts-in-fog (reduce (fn [gs buster]
                                (into gs (filter #(< (d2 buster %) FOG_RADIUS2) seen-ghosts)))
                              #{} my-busters)]
    (-> ia-data
        (assoc :ghosts (into ghosts (remove ghosts-in-fog seen-ghosts)))
        (assoc :unvisited-zones (remove visited-zones unvisited)))))

(defn do-think [game previous-ia-data my-busters ghosts]
  (let [updated-ia-data (update-ia-data previous-ia-data my-busters ghosts)
        [_ actions] (reduce (fn [[ia-data actions] buster]
                              (let [[new-data action]
                                    (do-buster-think game ia-data buster ghosts)]
                                [new-data (conj actions action)]))
                            [updated-ia-data []] my-busters)]
    [updated-ia-data actions]))

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
    (loop [round 1
           ia-data (init-ia-data)]
      (debug "Round #" round)
      (let [entities (read-game-turn game *in*)
            _ (debug "game/entities" entities)
            _ (doseq [e entities]
                (debug "entity>" e))
            myBusters (my-busters game entities)
            ghosts (ghosts entities)
            [new-ia-data actions] (do-think game ia-data myBusters ghosts)]
        ;(debug "actions" actions)
        (doseq [a actions]
          (debug "action>" a))
        (display-actions actions)
        (recur (inc round) new-ia-data)))))