(ns Player
  (:gen-class)
  (:import (clojure.lang LineNumberingPushbackReader)
           (java.io StringReader)))

;
; TODO rethink exploration... follow enemies?
; TODO keep track of seen enemies to hunt them
; TODO try roles: Explorer, Hunter, ...
;
;


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

(defn distance [e1 e2]
  (int (Math/sqrt (d2 e1 e2))))

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
(def MIN_DIST_STUN 0)
(def MIN_D2_STUN (sqr MIN_DIST_STUN))

(def FOG_RADIUS 2200)
(def FOG_RADIUS2 (sqr FOG_RADIUS))
(def FOG_DIAMETER (* 2 FOG_RADIUS))
(def HOME1 {:x 0 :y 0})
(def HOME2 {:x 16000 :y 9000})
(def STUN_RELOAD 20)
(def GHOST_MOVE_AWAY 400)

(def VISITED_ALIVE_DURATION 50)

(def STATE_IDLE 0)
(def STATE_CARRYING 1)
(def STATE_STUNNED 2)
(def STATE_TRAPPING 3)

(defn closest [buster positionables]
  (first (sort-by #(d2 buster %) positionables)))

(defn farest [buster positionables]
  (last (sort-by #(d2 buster %) positionables)))

(defn closest-to-bust [buster ghosts]
  (first (sort-by #(abs (- MIN_D2_BUST (d2 buster %))) ghosts)))


;---------------------------------------------------------------
; GRID
;---------------------------------------------------------------

(defn grid-cell-diameter [grid]
  (get grid :diameter))

(defn grid-cells [grid]
  (vals (get grid :cells)))

(defn generate-grid []
  (let [sz (* 0.1 FOG_DIAMETER)
        decal (/ sz 2)
        cells (for [x (range 0 (dec (/ WIDTH sz)))
                    y (range 0 (dec (/ HEIGHT sz)))]
                [[x y] {:x       (+ decal (* x sz))
                        :y       (+ decal (* y sz))
                        :visited 0}])
        cells (into {} cells)]
    {:diameter sz
     :cells    cells}))

(defn tick-cells [grid]
  (let [cells (:cells grid)
        newCells (reduce (fn [acc [k cell]]
                           (assoc acc k (update-in cell [:visited] dec))) {} cells)]
    (assoc grid :cells newCells)))

(defn mark-visited-cells [grid buster]
  (let [cells (:cells grid)
        diam (:diameter grid)
        limit (sqr (- FOG_RADIUS (/ diam 6)))
        newCells (reduce (fn [acc [k cell]]
                           (if (< (d2 buster cell) limit)
                             (assoc acc k (assoc cell :visited VISITED_ALIVE_DURATION))
                             (assoc acc k cell))) {} cells)]
    (assoc grid :cells newCells)))

(defn compute-zones-of-interest [grid]
  grid)

;---------------------------------------------------------------

(defn generate-zones-0 []
  (let [sz (* 0.75 FOG_DIAMETER)]
    (for [x (range 0 (/ WIDTH sz))
          y (range 0 (/ HEIGHT sz))]
      {:x (+ (/ FOG_RADIUS 2) (* x sz))
       :y (+ (/ FOG_RADIUS 2) (* y sz))})))


(defn generate-zones [myTeam]
  (let [zones [{:order 1, :x 1100.0, :y 1100.0, :weight (if (= 0 myTeam) 10 80)}
               {:order 12, :x 1100.0, :y 4400.0, :weight 100}
               {:order 11, :x 1100.0, :y 7700.0, :weight 10}
               {:order 2, :x 4400.0, :y 1100.0, :weight 10}
               {:order 13, :x 4400.0, :y 4400.0, :weight 10}
               {:order 10, :x 4400.0, :y 7700.0, :weight 10}
               {:order 3, :x 7700.0, :y 1100.0, :weight (if (= 0 myTeam) 50 20)}
               {:order 14, :x 7700.0, :y 4400.0, :weight 10}
               {:order 9, :x 7700.0, :y 7700.0, :weight (if (= 0 myTeam) 20 50)}
               {:order 4, :x 11000.0, :y 1100.0, :weight 10}
               {:order 15, :x 11000.0, :y 4400.0, :weight 10}
               {:order 8, :x 11000.0, :y 7700.0, :weight 10}
               {:order 5, :x 14300.0, :y 1100.0, :weight 100}
               {:order 6, :x 14300.0, :y 4400.0, :weight 10}
               {:order 7, :x 14300.0, :y 7700.0, :weight (if (= 0 myTeam) 80 10)}]]
    (if (= 0 myTeam)
      zones
      (map (fn [z] (let [w (:order z)]
                     (assoc z :order (- 16 w)))) zones))))

(defn generate-unexplored-zones [myTeam]
  [{:order (if (= 0 myTeam) 1 2), :x 1100.0, :y 1100.0}
   {:order (if (= 0 myTeam) 4 3), :x 1100.0, :y 7700.0}
   {:order (if (= 0 myTeam) 3 4), :x 14300.0, :y 1100.0}
   {:order (if (= 0 myTeam) 2 1), :x 14300.0, :y 7700.0}])

;---------------------------------------------------------------

(defn my-home [game]
  (if (= 0 (:myTeamId game))
    HOME1
    HOME2))

(defn init-ia-data [game]
  {:unvisited-zones  (generate-zones (:myTeamId game))
   :unexplored-zones (generate-unexplored-zones (:myTeamId game))
   :ghosts           []
   :busters          (reduce (fn [acc id] (assoc acc id {}))
                             {}
                             (range (:bustersPerPlayer game)))})


(defn my-busters [game entities]
  (let [myTeamId (:myTeamId game)]
    (filter #(= (:entityType %) myTeamId) entities)))

(defn other-busters [game entities]
  (let [myTeamId (:myTeamId game)]
    (filter #(not (or (= (:entityType %) myTeamId)
                      (= (:entityType %) -1))) entities)))

(defn ghosts [entities]
  (filter #(= (:entityType %) -1) entities))

(defn can-bust? [e1 e2]
  (let [ds (d2 e1 e2)]
    (and (<= ds MAX_D2_BUST)
         (<= MIN_D2_BUST ds))))

(defn should-bust? [ghost]
  (let [nb-total-busters (:value ghost)
        nb-busters (:nb-busters ghost)
        nb-busters (if (nil? nb-busters) 0 nb-busters)
        stamina (:state ghost)
        r (or
            (< nb-busters 1)
            (and (<= nb-busters nb-total-busters) (= 0 stamina))
            (< (* 2 nb-busters) stamina))]
    (debug "should-bust?" ghost)
    (debug "            " r "(" nb-busters " vs " stamina ")")
    r))

(defn must-release? [buster]
  (= STATE_CARRYING (:state buster)))

(defn stunned? [buster]
  (= STATE_STUNNED (:state buster)))

(defn trapping? [buster]
  (= STATE_TRAPPING (:state buster)))

(defn carrying? [buster]
  (= STATE_CARRYING (:state buster)))

(defn can-release? [game buster]
  (<= (d2 (my-home game) buster) MAX_D2_RELEASE))

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

(defn action-stun [target]
  {:type   :stun
   :target (:entityId target)
   :msg    "take that!"})


(defn can-stun? [ia-data e]
  (let [round (:round ia-data)
        stun-round (get-in ia-data [:busters (:entityId e) :stun-round])]
    (or (nil? stun-round)
        (< STUN_RELOAD (- round stun-round)))))

(defn can-stun-target? [e1 e2]
  (if (and e1 e2)
    (let [d2 (d2 e1 e2)]
      (and (<= d2 MAX_D2_STUN)
           (<= MIN_D2_STUN d2)))
    false))

(defn action-stun-other [ia-data buster targets]
  (let [others (:round-other-busters ia-data)
        target (first targets)]                             ; TODO: keep the one my other busters could not stun
    [(-> ia-data
         (update-in [:busters (:entityId buster)] assoc :stun-round (:round ia-data))
         (assoc :round-other-busters (remove #{target} others)))
     (action-stun target)]))

(defn action-move-random [ia-data e]
  (let [seen-ghosts (:ghosts ia-data)
        target-ghost (closest e seen-ghosts)]
    (if (seq target-ghost)
      [ia-data
       (action-move target-ghost "previously seen!")]

      (let [zones (:unvisited-zones ia-data)
            grouped (group-by :weight zones)
            unvisited (if (empty? grouped)
                        []
                        (get grouped (apply max (keys grouped))))
            target-zone (get-in ia-data [:busters (:entityId e) :target-zone])
            preferred-zone (if (or (nil? target-zone)
                                   (not-any? #{target-zone} unvisited))
                             (farest e unvisited)
                             target-zone)]
        (debug "#:preferred unvisited zone '" preferred-zone "' target-zone: " target-zone)
        (if preferred-zone
          [(-> ia-data
               (assoc :unvisited-zones (remove #(= % preferred-zone) zones))
               (update-in [:busters (:entityId e)] assoc :target-zone preferred-zone))
           (action-move preferred-zone "let's explore!")]
          (let [random (rand-int 360)
                theta (/ (* Math/PI random) 180)
                x (+ (:x e) (* MAX_DIST_MOVE (Math/cos theta)))
                y (+ (:y e) (* MAX_DIST_MOVE (Math/sin theta)))]
            [ia-data (action-move x y "random move")]))))))

(defn action-move-away [buster dst]
  (let [deltaX (- (:x dst) (:x buster))
        deltaY (- (:y dst) (:y buster))
        dist (Math/sqrt (+ (sqr deltaX) (sqr deltaY)))
        deltaDist (- (+ dist GHOST_MOVE_AWAY) (/ (+ MAX_DIST_BUST MIN_DIST_BUST) 2))
        ux (/ deltaX dist)
        uy (/ deltaY dist)
        newX (+ (:x buster) (* deltaDist ux))
        newY (+ (:y buster) (* deltaDist uy))]
    (action-move newX newY (str "one step aside" (distance {:x newX :y newY} buster)))))

(defn action-move-to-release [ia-data buster game]
  (action-move (my-home game) "recycling!")
  ;(let [dh1 (d2 HOME1 buster)
  ;      dh2 (d2 HOME2 buster)]
  ;  (if (< dh1 dh2)
  ;    (action-move HOME1 "let's trash this")
  ;    (action-move HOME2 "let's trash this")))
  )

(defn action-bust [e]
  {:type    :bust
   :ghostId (:entityId e)
   :msg     "Zzzzap!"})

(defn action-stunned []
  (action-move 1 1 "stunned!"))

(defn one-or-inc [x]
  (if (nil? x) 1 (inc x)))

(defn do-buster-think-0 [game ia-data buster]
  (let [ghosts (:round-ghosts ia-data)
        ghosts-bustable (filter #(and (can-bust? buster %)
                                      (should-bust? %)) ghosts)
        closest-ghost (closest buster ghosts)

        other-busters (:round-other-busters ia-data)
        stunnable-busters (filter #(not (stunned? %)) other-busters)
        high-priority-targets (filter #(or (trapping? %)
                                           (carrying? %)) stunnable-busters)

        danger-targets (filter #(can-stun-target? buster %) stunnable-busters)

        canStun (can-stun? ia-data buster)
        canStunTarget (can-stun-target? buster (closest buster stunnable-busters))
        canStunVipTarget (can-stun-target? buster (closest buster high-priority-targets))


        _ (debug "---me:" buster)
        _ (debug "   canStun..........:" canStun)
        _ (debug "   VIP..............:" high-priority-targets)
        _ (debug "   canStun VIP......:" canStunVipTarget)
        _ (debug "   stunnable targets:" danger-targets)
        _ (debug "   ghost bustable...:" ghosts-bustable)

        ]
    (cond (stunned? buster)
          [ia-data (action-stunned)]

          (must-release? buster)
          (if (can-release? game buster)
            [ia-data (action-release)]
            (if (seq danger-targets)
              (action-stun-other ia-data buster danger-targets)
              [ia-data (action-move-to-release ia-data buster game)])) ; TODO move away from visible enemies

          (and canStun
               (seq high-priority-targets)
               canStunVipTarget)
          (action-stun-other ia-data buster high-priority-targets)

          (and (seq ghosts))
          (if (seq ghosts-bustable)
            (let [ghost (first ghosts-bustable)
                  next-ia (-> ia-data
                              (assoc :round-ghosts
                                     (map (fn [b] (if (= b ghost)
                                                    (update-in b [:nb-busters] one-or-inc)
                                                    b)) ghosts)))]
              [next-ia (action-bust (first ghosts-bustable))])
            [ia-data (action-move-away buster (closest-to-bust buster ghosts))])

          (and canStun
               canStunTarget)
          (action-stun-other ia-data buster stunnable-busters)


          (and closest-ghost)
          [(-> ia-data
               (assoc :ghosts (-> (remove #(= % closest-ghost) ghosts)
                                  (conj (assoc closest-ghost :buster (:entityId buster)))))
               (update-in [:busters (:entityId buster)] dissoc :target-zone))
           (action-move closest-ghost "a ghost!")]

          :else
          (action-move-random ia-data buster))))

(defn do-explore [game ia-data e]
  (let [zones (:unexplored-zones ia-data)
        ordered (sort-by :order zones)
        target-zone (get-in ia-data [:busters (:entityId e) :target-zone])
        target-zone (if (or (nil? target-zone)
                            (< (d2 e target-zone) 50))
                      (first ordered)
                      target-zone)
        _ (debug "---me:" e)
        _ (debug "   unexplored" zones)
        _ (debug "   target" target-zone ", d=" (if target-zone (distance target-zone e)))
        ]
    (if target-zone
      [(-> ia-data
           (assoc :unexplored-zones (remove #(= % target-zone) zones))
           (update-in [:busters (:entityId e)] assoc :target-zone target-zone))
       (action-move target-zone "exploring!")]
      nil)))

(defn do-buster-think [game ia-data buster]
  (let [role (get-in ia-data [:roles (:entityId buster)])
        computed (cond
                   (= :explorer role)
                   (do-explore game ia-data buster)
                   :else nil)]
    (if computed
      computed
      (do-buster-think-0 game ia-data buster))))



(defn update-ia-data [ia-data game entities]
  (let [my-busters (my-busters game entities)
        other-busters (other-busters game entities)
        ghosts (ghosts entities)
        ;
        unvisited (:unvisited-zones ia-data)
        visited-zones (reduce (fn [zs buster]
                                (into zs (filter #(< (d2 buster %) 50) unvisited)))
                              #{} my-busters)
        ;
        seen-ghosts (:ghosts ia-data)
        ; --- remove ghosts that should currently be seen, in case there were zapped
        ghosts-in-fog (reduce (fn [gs buster]
                                (into gs (filter #(< (d2 buster %) FOG_RADIUS2) seen-ghosts)))
                              #{} my-busters)]
    (-> ia-data
        (assoc :ghosts (into ghosts (remove ghosts-in-fog seen-ghosts)))
        (assoc :unvisited-zones (remove visited-zones unvisited))
        (assoc :round-ghosts ghosts)
        (assoc :round-busters my-busters)
        (assoc :round-other-busters other-busters))))

(defn do-think [game round-ia-data]
  (let [my-busters (:round-busters round-ia-data)
        [ia-data actions] (reduce (fn [[ia-data actions] buster]
                                    (let [x (do-buster-think game ia-data buster)
                                          [new-data action] x]
                                      [new-data (conj actions action)]))
                                  [round-ia-data []] my-busters)
        next-ia-data (-> round-ia-data
                         (assoc :busters (:busters ia-data))
                         (assoc :unexplored-zones (:unexplored-zones ia-data)))]
    [next-ia-data actions]))

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
    (= :bust (:type action)) (println "BUST" (:ghostId action) (:msg action))
    (= :stun (:type action)) (println "STUN" (:target action) (:msg action))))

(defn display-actions [actions]
  (doseq [action actions]
    (display-action action)))

(def EXPLORER true)

(defn affect-roles [game ia-data]
  (assoc ia-data
    :roles
    (cond-> {}
            EXPLORER (assoc 0 :explorer))))

(defn -main [& args]
  (let [game (read-initialization-input *in*)]
    (debug "game/input" game)
    ; bustersPerPlayer: the amount of busters you control
    ; ghostCount: the amount of ghosts on the map
    ; myTeamId: if this is 0, your base is on the top left of the map, if it is one, on the bottom right
    (loop [round 1
           ia-data (affect-roles game (init-ia-data game))]
      (debug "Round #" round)
      (let [entities (read-game-turn game *in*)
            _ (debug "game/entities" entities)
            _ (debug "game/ia-data" ia-data)
            round-ia-data (-> ia-data
                              (assoc :round round)
                              (update-ia-data game entities))
            [new-ia-data actions] (do-think game round-ia-data)]
        (debug "actions" actions)
        (display-actions actions)
        (recur (inc round)
               (dissoc new-ia-data
                       :round-ghosts
                       :round-busters
                       :round-other-busters))))))
