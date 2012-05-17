(ns cljenetic.examplephrase
	(use cljenetic.core clojure.math.numeric-tower))
	

(def s "this is an example phrase i am trying to evolve")
(def letters [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o\ \p \q \r \s \t \u \v \w \x \y \z \space])
(defn get-letter [] (first (shuffle letters)))
(defn phrase-generator [] (take (count s) (repeatedly get-letter)))
(defn distance [s1 s2] (reduce + (map #(abs (- (int %1) (int %2))) s1 s2)))
(defn letter-mutator [gene] (get-letter))
(defn phrase-fitness [i] (/ 1.0 (+ 1 (/ (distance s i) 10.0))))
(def phrase-ga (genetic-algorithm phrase-generator 500 (partial individual-gene-mutator letter-mutator) 0.5 roulette-wheel-selector 0 full-random-crossover phrase-fitness false))
(def sol (evolve-solution phrase-ga 500 #(> % 0.99)))