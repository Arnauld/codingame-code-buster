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

(deftest another-test
  (testing "another test"
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
          myBusters (my-busters game entities)
          otherBusters (other-busters game entities)
          ghosts (ghosts entities)
          [new-ia-data actions] (do-think game ia-data myBusters otherBusters ghosts)]
      (doseq [a actions]
        (println a))
      (is (<= (count (filter #(and
                               (= (:type %) :bust)
                               (= (:ghostId %) 8))
                             actions)) 2)))))
