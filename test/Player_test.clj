(ns Player-test
  (:require [clojure.test :refer :all]
            [Player :refer :all]))

;--------
; MATH
;--------


;
;
;

(deftest a-test
  (testing "basic game setup"
    (let [game {:bustersPerPlayer 3, :ghostCount 21, :myTeamId 0}
          ia-data (init-ia-data game)
          entities [{:entityId 0, :x 1600, :y 1600, :entityType 0, :state 0, :value -1}
                    {:entityId 1, :x 2449, :y 751, :entityType 0, :state 0, :value -1}
                    {:entityId 2, :x 751, :y 2449, :entityType 0, :state 0, :value -1}]
          myBusters (my-busters game entities)
          otherBusters (other-busters game entities)
          ghosts (ghosts entities)
          [new-ia-data actions] (do-think game ia-data myBusters otherBusters ghosts)]

      (is (= {0 {} 1 {} 2 {}}) (get ia-data :busters))
      (is (= {:x 0 :y 0} (my-home game)))
      (is (= myBusters [{:entityId 0, :x 1600, :y 1600, :entityType 0, :state 0, :value -1}
                        {:entityId 1, :x 2449, :y 751, :entityType 0, :state 0, :value -1}
                        {:entityId 2, :x 751, :y 2449, :entityType 0, :state 0, :value -1}]))
      (is (= ghosts []))
      (is (= actions []))
      )))
