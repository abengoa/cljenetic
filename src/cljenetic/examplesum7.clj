(ns cljenetic.examplesum7
	(use cljenetic.core clojure.math.numeric-tower))
	
(defn mygenerator [] (take 10 (map #(* 3 %) (repeatedly rand))))
(defn mymutator [gene] (* 3 (rand)))
(defn myfitness [i] (/ 1.0 (+ 1 (abs (- (reduce + i) 7)))))
(def myga (genetic-algorithm mygenerator 50 (partial individual-gene-mutator mymutator) 0.05 roulette-wheel-selector 10 single-point-crossover myfitness false))
(def solution (evolve-solution myga 1000 #(> % 0.99999)))
(best-fit solution)