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
    (let [game {:bustersPerPlayer 2, :ghostCount 9, :myTeamId 0}
          entities [{:entityId 0, :x 1176, :y 2024, :entityType 0, :state 0, :value -1}
                    {:entityId 1, :x 2024, :y 1176, :entityType 0, :state 0, :value -1}
                    {:entityId 2, :x 1973, :y 3723, :entityType -1, :state 0, :value 0}]
          myBusters (my-busters game entities)
          ghosts (ghosts entities)
          actions (do-think game myBusters ghosts)]

      (is (= {:x 0 :y 0} (my-home game)))
      (is (= myBusters [{:entityId 0, :x 1176, :y 2024, :entityType 0, :state 0, :value -1}
                        {:entityId 1, :x 2024, :y 1176, :entityType 0, :state 0, :value -1}]))
      (is (= ghosts [{:entityId 2, :x 1973, :y 3723, :entityType -1, :state 0, :value 0}]))
      (is (= actions []))
      )))
