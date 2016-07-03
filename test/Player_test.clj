(ns Player-test
  (:require [clojure.test :refer :all]
            [Player :refer :all]))

;--------
; MATH
;--------


;
;
;

;(deftest a-test
;  (testing "basic game setup"
;    (let [game {:bustersPerPlayer 3, :ghostCount 21, :myTeamId 0}
;          ia-data (init-ia-data game)
;          entities [{:entityId 0, :x 1600, :y 1600, :entityType 0, :state 0, :value -1}
;                    {:entityId 1, :x 2449, :y 751, :entityType 0, :state 0, :value -1}
;                    {:entityId 2, :x 751, :y 2449, :entityType 0, :state 0, :value -1}]
;          myBusters (my-busters game entities)
;          otherBusters (other-busters game entities)
;          ghosts (ghosts entities)
;          [new-ia-data actions] (do-think game ia-data myBusters otherBusters ghosts)]
;
;      (is (= {0 {} 1 {} 2 {}}) (get ia-data :busters))
;      (is (= {:x 0 :y 0} (my-home game)))
;      (is (= myBusters [{:entityId 0, :x 1600, :y 1600, :entityType 0, :state 0, :value -1}
;                        {:entityId 1, :x 2449, :y 751, :entityType 0, :state 0, :value -1}
;                        {:entityId 2, :x 751, :y 2449, :entityType 0, :state 0, :value -1}]))
;      (is (= ghosts []))
;      (is (= actions []))
;      )))

(deftest do-think-case-1
  (testing "no need to have too much buster once ghost is out of stamina"
    (let [game {:bustersPerPlayer 4, :ghostCount 21, :myTeamId 0}
          entities [{:entityId 0, :x 2596, :y 3443, :entityType 0, :state 3, :value 8}
                    {:entityId 1, :x 2562, :y 3719, :entityType 0, :state 3, :value 8}
                    {:entityId 2, :x 2549, :y 3511, :entityType 0, :state 3, :value 8}
                    {:entityId 3, :x 1158, :y 276, :entityType 0, :state 0, :value -1}
                    {:entityId 8, :x 2435, :y 4949, :entityType -1, :state 3, :value 3}]
          ia-data {:other-busters   [],
                   :round           11,
                   :unvisited-zones [{:x 1100.0, :y 1100.0} {:x 1100.0, :y 4400.0}
                                     {:x 1100.0, :y 7700.0} {:x 4400.0, :y 1100.0}
                                     {:x 4400.0, :y 4400.0} {:x 4400.0, :y 7700.0}
                                     {:x 7700.0, :y 1100.0} {:x 7700.0, :y 4400.0}
                                     {:x 7700.0, :y 7700.0} {:x 11000.0, :y 1100.0}
                                     {:x 11000.0, :y 4400.0} {:x 11000.0, :y 7700.0}
                                     {:x 14300.0, :y 1100.0} {:x 14300.0, :y 4400.0}
                                     {:x 14300.0, :y 7700.0}],
                   :ghosts          [{:entityId 8, :x 2435, :y 4949, :entityType -1, :state 8, :value 2}],
                   :busters         {3 {}, 2 {}, 1 {}, 0 {}}}
          ;; ---
          round-ia-data (-> ia-data
                            (assoc :round 11)
                            (update-ia-data game entities))
          [new-ia-data actions] (do-think game round-ia-data)]
      (doseq [a actions]
        (println a))
      (is (<= (count (filter #(and
                               (= (:type %) :bust)
                               (= (:ghostId %) 8))
                             actions)) 2)))))

(deftest do-think-case-2
  (testing "focus the opponent carrying a ghost"
    (let [game {:bustersPerPlayer 4, :ghostCount 21, :myTeamId 0}
          entities [{:entityId 0, :x 506, :y 1740, :entityType 0, :state 2, :value 7}
                    {:entityId 1, :x 6576, :y 5081, :entityType 0, :state 0, :value -1}
                    {:entityId 2, :x 5261, :y 4172, :entityType 0, :state 0, :value -1}
                    {:entityId 3, :x 2989, :y 3839, :entityType 0, :state 0, :value -1}
                    {:entityId 6, :x 1642, :y 1669, :entityType 1, :state 2, :value 9}
                    {:entityId 7, :x 3033, :y 1725, :entityType 1, :state 1, :value 3}]
          ia-data {:busters         {3 {}, 2 {}, 1 {}, 0 {}}
                   :unvisited-zones [{:x 1100.0, :y 1100.0} {:x 1100.0, :y 4400.0}
                                     {:x 1100.0, :y 7700.0} {:x 4400.0, :y 1100.0}
                                     {:x 4400.0, :y 4400.0} {:x 4400.0, :y 7700.0}
                                     {:x 7700.0, :y 1100.0} {:x 7700.0, :y 4400.0}
                                     {:x 7700.0, :y 7700.0} {:x 11000.0, :y 1100.0}
                                     {:x 11000.0, :y 4400.0} {:x 11000.0, :y 7700.0}
                                     {:x 14300.0, :y 1100.0} {:x 14300.0, :y 4400.0}
                                     {:x 14300.0, :y 7700.0}]}
          round-ia-data (-> ia-data
                            (assoc :round 11)
                            (update-ia-data game entities))
          [new-ia-data actions] (do-think game round-ia-data)]
      (doseq [a actions]
        (println a))
      )))

(deftest do-think-case-3
  (testing "prevent NPE on start :p"
    (let [game {:bustersPerPlayer 2, :ghostCount 9, :myTeamId 0}
          entities [{:entityId 0, :x 1176, :y 2024, :entityType 0, :state 0, :value -1}
                    {:entityId 1, :x 2024, :y 1176, :entityType 0, :state 0, :value -1}]
          ia-data {:unvisited-zones [{:x 1100.0, :y 1100.0} {:x 1100.0, :y 4400.0}
                                     {:x 1100.0, :y 7700.0} {:x 4400.0, :y 1100.0}
                                     {:x 4400.0, :y 4400.0} {:x 4400.0, :y 7700.0}
                                     {:x 7700.0, :y 1100.0} {:x 7700.0, :y 4400.0}
                                     {:x 7700.0, :y 7700.0} {:x 11000.0, :y 1100.0}
                                     {:x 11000.0, :y 4400.0} {:x 11000.0, :y 7700.0}
                                     {:x 14300.0, :y 1100.0} {:x 14300.0, :y 4400.0}
                                     {:x 14300.0, :y 7700.0}],
                   :ghosts          [],
                   :busters         {1 {}, 0 {}}}
          round-ia-data (-> ia-data
                            (assoc :round 11)
                            (update-ia-data game entities))
          [new-ia-data actions] (do-think game round-ia-data)]
      (doseq [a actions]
        (println a)))))

(deftest do-think-case-4
  (testing "weird step aside case"
    (let [game {:bustersPerPlayer 2, :ghostCount 13, :myTeamId 0}
          entities [{:entityId 0, :x 7168, :y 4425, :entityType 0, :state 0, :value -1}
                    {:entityId 1, :x 7109, :y 6124, :entityType 0, :state 3, :value 0}
                    {:entityId 0, :x 8484, :y 5116, :entityType -1, :state 0, :value 2}]


          ia-data {:round-busters   [{:entityId 0, :x 6460, :y 4053, :entityType 0, :state 0, :value -1}
                                     {:entityId 1, :x 7109, :y 6124, :entityType 0, :state 3, :value 0}],
                   :round           78,
                   :unvisited-zones [{:x 1100.0, :y 1100.0} {:x 1100.0, :y 4400.0} {:x 1100.0, :y 7700.0} {:x 4400.0, :y 1100.0} {:x 4400.0, :y 4400.0} {:x 4400.0, :y 7700.0} {:x 7700.0, :y 1100.0} {:x 7700.0, :y 4400.0} {:x 7700.0, :y 7700.0} {:x 11000.0, :y 1100.0} {:x 11000.0, :y 4400.0} {:x 11000.0, :y 7700.0} {:x 14300.0, :y 1100.0} {:x 14300.0, :y 4400.0} {:x 14300.0, :y 7700.0}],
                   :ghosts          [{:entityId 0, :x 8484, :y 5116, :entityType -1, :state 0, :value 2}],
                   :busters         {1 {:target-zone {:x 14300.0, :y 4400.0}},
                                     0 {:target-zone {:x 14300.0, :y 7700.0}}}}

          round-ia-data (-> ia-data
                            (assoc :round 11)
                            (update-ia-data game entities))
          [new-ia-data actions] (do-think game round-ia-data)]
      (doseq [a actions]
        (println a)))))


(deftest do-think-case-5
  (testing "prevent stun of an already stunned"
    (let [game {:bustersPerPlayer 3, :ghostCount 17, :myTeamId 0}

          entities [{:entityId 0, :x 7786, :y 3966, :entityType 0, :state 0, :value -1}
                    {:entityId 1, :x 9632, :y 3097, :entityType 0, :state 0, :value -1}
                    {:entityId 2, :x 6643, :y 6138, :entityType 0, :state 0, :value -1}
                    {:entityId 5, :x 8704, :y 4752, :entityType 1, :state 2, :value 10}]
          ia-data {:unvisited-zones [{:x 1100.0, :y 1100.0} {:x 1100.0, :y 4400.0}
                                     {:x 1100.0, :y 7700.0} {:x 4400.0, :y 1100.0}
                                     {:x 4400.0, :y 4400.0} {:x 4400.0, :y 7700.0}
                                     {:x 7700.0, :y 1100.0} {:x 7700.0, :y 4400.0}
                                     {:x 7700.0, :y 7700.0} {:x 11000.0, :y 1100.0}
                                     {:x 11000.0, :y 4400.0} {:x 11000.0, :y 7700.0}
                                     {:x 14300.0, :y 1100.0} {:x 14300.0, :y 4400.0}
                                     {:x 14300.0, :y 7700.0}],
                   :ghosts          [],
                   :busters         {0 {:target-zone {:x 14300.0, :y 7700.0}, :stun-round 63},
                                     1 {:target-zone {:x 14300.0, :y 4400.0}, :stun-round 107},
                                     2 {:target-zone {:x 1100.0, :y 7700.0}, :stun-round 52}},
                   :round           107}

          round-ia-data (-> ia-data
                            (assoc :round 11)
                            (update-ia-data game entities))
          [new-ia-data actions] (do-think game round-ia-data)]
      (doseq [a actions]
        (println a)))))

(deftest do-think-case-6
  (testing "prevent error on search unexplored"
    (let [game {:bustersPerPlayer 3, :ghostCount 17, :myTeamId 0}
          entities [{:entityId 0, :x 5028, :y 2731, :entityType 0, :state 0, :value -1}
                    {:entityId 1, :x 6293, :y 3268, :entityType 0, :state 0, :value -1}
                    {:entityId 2, :x 5850, :y 3823, :entityType 0, :state 0, :value -1}]
          ia-data {:round           54,
                   :unvisited-zones [{:x 1100.0, :y 1100.0, :weight 10} {:y 7700.0, :weight 10, :x 1100.0}
                                     {:y 4400.0, :weight 100, :x 1100.0} {:y 1100.0, :weight 10, :x 4400.0}
                                     {:y 4400.0, :weight 10, :x 4400.0} {:y 7700.0, :weight 10, :x 4400.0}
                                     {:x 7700.0, :y 1100.0, :weight 50} {:y 4400.0, :weight 10, :x 7700.0}
                                     {:x 7700.0, :y 7700.0, :weight 20} {:y 1100.0, :weight 10, :x 11000.0}
                                     {:y 4400.0, :weight 10, :x 11000.0} {:y 7700.0, :weight 10, :x 11000.0}
                                     {:y 1100.0, :weight 100, :x 14300.0} {:y 4400.0, :weight 10, :x 14300.0}
                                     {:x 14300.0, :y 7700.0, :weight 80}],
                   :ghosts          [{:entityId 0, :x 7569, :y 3828, :entityType -1, :state 0, :value 2}],
                   :busters         {2 {:target-zone {:y 4400.0, :weight 100, :x 1100.0}},
                                     1 {:target-zone {:y 1100.0, :weight 100, :x 14300.0}},
                                     0 {:target-zone {:y 1100.0, :weight 100, :x 14300.0}}}}
          round-ia-data (-> ia-data
                            (assoc :round 11)
                            (update-ia-data game entities))
          [new-ia-data actions] (do-think game round-ia-data)]
      (doseq [a actions]
        (println a)))))

(deftest do-think-case-7
  (testing "prevent weird random move..."
    (let [game {:bustersPerPlayer 2, :ghostCount 9, :myTeamId 0}
          entities [{:entityId 0, :x 3490, :y 918, :entityType 0, :state 0, :value -1}
                    {:entityId 1, :x 4618, :y 3048, :entityType 0, :state 0, :value -1}]
          ia-data {:round           40,
                   :unvisited-zones [{:x 1100.0, :y 1100.0, :weight 10}
                                     {:y 7700.0, :weight 10, :x 1100.0}
                                     {:y 1100.0, :weight 10, :x 4400.0}
                                     {:y 4400.0, :weight 10, :x 4400.0}
                                     {:y 7700.0, :weight 10, :x 4400.0}
                                     {:x 7700.0, :y 1100.0, :weight 50}
                                     {:y 4400.0, :weight 10, :x 7700.0}
                                     {:x 7700.0, :y 7700.0, :weight 20}
                                     {:y 1100.0, :weight 10, :x 11000.0}
                                     {:y 4400.0, :weight 10, :x 11000.0}
                                     {:y 7700.0, :weight 10, :x 11000.0}
                                     {:y 1100.0, :weight 100, :x 14300.0}
                                     {:y 4400.0, :weight 10, :x 14300.0}
                                     {:x 14300.0, :y 7700.0, :weight 80}],
                   :ghosts          [],
                   :busters         {1 {:target-zone {:y 1100.0, :weight 100, :x 14300.0}},
                                     0 {:target-zone {:y 1100.0, :weight 100, :x 14300.0}}}}
          round-ia-data (-> ia-data
                            (assoc :round 11)
                            (update-ia-data game entities))
          [new-ia-data actions] (do-think game round-ia-data)]
      (is (empty? (filter #(= "random move" (:msg %))
                          actions)))
      (doseq [a actions]
        (println a)))))


(deftest do-think-case-8
  (testing "prevent locked case..."
    (let [game {:bustersPerPlayer 2, :ghostCount 9, :myTeamId 0}
          entities [{:entityId 0, :x 7512, :y 1104, :entityType 0, :state 3, :value 2}
                    {:entityId 1, :x 7439, :y 521, :entityType 0, :state 0, :value -1}
                    {:entityId 2, :x 9385, :y 2098, :entityType 1, :state 3, :value 2}
                    {:entityId 3, :x 9273, :y 1430, :entityType 1, :state 3, :value 2}
                    {:entityId 2, :x 8899, :y 495, :entityType -1, :state 11, :value 3}]
          ia-data {:round           88,
                   :unvisited-zones [{:x 1100.0, :y 1100.0, :weight 10}
                                     {:y 7700.0, :weight 10, :x 1100.0}
                                     {:y 1100.0, :weight 10, :x 4400.0}
                                     {:y 4400.0, :weight 10, :x 4400.0}
                                     {:y 7700.0, :weight 10, :x 4400.0}
                                     {:x 7700.0, :y 1100.0, :weight 50}
                                     {:y 4400.0, :weight 10, :x 7700.0}
                                     {:x 7700.0, :y 7700.0, :weight 20}
                                     {:y 1100.0, :weight 10, :x 11000.0}
                                     {:y 4400.0, :weight 10, :x 11000.0}
                                     {:y 7700.0, :weight 10, :x 11000.0}
                                     {:y 1100.0, :weight 100, :x 14300.0}
                                     {:y 4400.0, :weight 10, :x 14300.0}
                                     {:x 14300.0, :y 7700.0, :weight 80}],
                   :ghosts          [{:entityId 2, :x 8899, :y 495, :entityType -1, :state 14, :value 3}],
                   :busters         {1 {:target-zone {:y 1100.0, :weight 100, :x 14300.0}},
                                     0 {:target-zone {:y 1100.0, :weight 100, :x 14300.0}}}}
          round-ia-data (-> ia-data
                            (assoc :round 11)
                            (update-ia-data game entities))
          [new-ia-data actions] (do-think game round-ia-data)]
      (is (empty? (filter #(= "random move" (:msg %))
                          actions)))
      (doseq [a actions]
        (println a)))))

