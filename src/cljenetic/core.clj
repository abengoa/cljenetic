(ns cljenetic.core
	"A small library for genetic algorithms in Clojure."
	(use clojure.math.numeric-tower))

(defn setup-population 
	"Generates a population for the GA using the individual generator."
	[generator size]
	(repeatedly size generator))
	
(defn calculate-fitness 
	"Applies the fitness function to all the individuals of a population and returns a map of individuals as keys and fitness as values."
	[fitness-fn population]
	(reduce into {} (pmap (fn [i] {i (fitness-fn i)}) population)))
	
(defn rank-fitness 
	"Ranks the individuals of a population by their fitness value. The result is a map of individuals as keys and rank as values."
	[fitness-map]
	(reduce into {} (map-indexed (fn [i v] {v i}) (map first (sort-by second fitness-map)))))
	
(defn genetic-algorithm
	"Creates a structure that holds the generation information of a GA. Parameters:
		individual-generator: a function with no parameters that generates one individual each time it is called.
		population-size: number of individuals in the GA population.
		individual-mutator: function that takes one individual as parameter and returns a mutated version.
		mutation-rate: probability of individual mutation on each generation.
		selection-method: function that selects pairs of individuals from the population for generating offspring for the next generation.
		keep-n: integer, the number of best individuals that from generation k that are passed to generation k+1 without modification.
		crossover-fn: function that takes two individuals and generates two new individuals from crossing the original ones.
		fitness-fn: function that the GA tries to maximize. It is applied to each individual.
		ranked: boolean value that determines if the selection of individuals should be fitness value-based or rank-based."
	[individual-generator population-size individual-mutator mutation-rate selection-method keep-n crossover-fn fitness-fn ranked]
	 (let [population (setup-population individual-generator population-size)
			fitness (calculate-fitness fitness-fn population)]
	 {:individual-mutator individual-mutator
	  :mutation-rate mutation-rate
	  :selection-method selection-method
	  :keep-n keep-n
	  :crossover-fn crossover-fn
	  :fitness-fn fitness-fn
	  :ranked ranked
	  :population population
	  :fitness fitness
	  :generation 0
	  :best []}))

(defn population 
	"Obtains the population of a GA, as a list of individuals."
	[g-a] (:population g-a))

(defn sorted-population 
	"Obtains the population of a GA, as a list of individuals sorted by their fitness value."
	[g-a] (reverse (sort-by second (:fitness g-a))))
	
(defn best-fit 
	"Obtains the best individual of a population."
	[g-a] (first (sorted-population g-a)) )

;; -------------------- Selection algorithms

(defn random-selector 
	"Selects random pairs of individuals from a population for crossover."
	[g-a] (partition 2 (shuffle (population g-a))))
	
(defn best-fit-selector 
	"Selects pairs of the original population, by sorting the individuals by fitness."
	[g-a] (partition 2 (map first (reverse (sort-by second (:fitness g-a))))))

(defn select-rnd-element 
	"Selects a random element from a probability map, where the keys are the possible elements and the values are the probabilities of selecting each element"
	[prob-map max-val] 
	(let [rn (* max-val (rand))] (first (reduce (fn [[a p] [n prob]] (if (and (<= p rn) (>= (+ p prob) rn)) [n (+ p prob)] [a (+ p prob)])) [nil 0] (shuffle (seq prob-map))))))

(defn roulette-wheel-selector 
	"Standard roulette wheel selection."
	[g-a]
	(let [target (count (population g-a))
		  total-fitness-value (reduce + (map second (:fitness g-a)))
;		  probs (reduce into {} (map (fn [[i f]] {i (/ f total-fitness-value)}) (:fitness g-a)))
		  selector #(select-rnd-element (:fitness g-a) total-fitness-value)]
		(partition 2 (repeatedly target selector))))

(defn crossover-population 
	"Crossover all the population of a GA, by using the selection and crossover functions until enough individuals are generated."
	[g-a]
	(reduce concat (map (fn [[i1 i2]] ((:crossover-fn g-a) i1 i2)) ((:selection-method g-a) g-a))))

;; -------------------- Crossover functions 

(defn single-point-crossover 
	"Crosses two individuals at a single point, selected randomly."
	[i1 i2]
	(let [cross-point (rand-int (count i1))]
		[(concat (take cross-point i1) (drop cross-point i2))
		(concat (take cross-point i2) (drop cross-point i1))]))

(defn multiple-point-crossover 
	"Crosses two individuals by n randomly-selected cut points."
	[n i1 i2]
	(if (pos? n)
		(let [i (rand-int (- (count i1) (dec n)))
			  [r1 r2] (multiple-point-crossover (dec n) (drop i i2) (drop i i1))]
			[(concat (take i i1) r1) (concat (take i i2) r2)])
		[i1 i2]))

(defn full-random-crossover 
	"Crosses two individuals, by selecting randomly for each gene."
	[i1 i2]
	(let [pairs (map #(if (> 0.5 (rand)) [%1 %2] [%2 %1]) i1 i2)]
		[(map first pairs) (map second pairs)]))

;(defn partially-mapped-crossover [i1 i2])
;(defn greedy-subtour-crossover [i1 i2])

;; ------------------- Genetic mutators
		
(defn individual-gene-mutator 
	"A mutator that changes one gene using a gene mutation function."
	[gene-mutation-fn individual]
	(let [i (rand-int (count individual))
		  r (drop i individual)]
		(concat (take i individual) [(gene-mutation-fn (first r))] (rest r))))

(defn mutator 
	"A function that uses a mutation function to change an individual with a given probability"
	[mutation-rate mutation-fn individual]
	(if (< (rand) mutation-rate) (mutation-fn individual) individual))

;; ------------------ Generic mechanisms for genetic algorithms

(defn g-a-iteration 
	"Calculates one generation of a GA."
	[g-a]
		(let [new-population 
			(take (count (population g-a)) (concat (take (:keep-n g-a) (map first (sorted-population g-a)))
				(map (partial mutator (:mutation-rate g-a) (:individual-mutator g-a)) (crossover-population g-a))))
		  new-fitness (calculate-fitness (:fitness-fn g-a) new-population)
		  new-fitness (if (:ranked g-a) (rank-fitness new-fitness) new-fitness)
		  ]
		  (assoc g-a :population new-population :fitness new-fitness :generation (inc (:generation g-a)) 
		  ;:best (concat (:best g-a) [(best-fit g-a)])
		  )))
 
(defn evolve-solution 
	"Takes a GA and evolves the population until either the fitness-predicate becomes true or a maximum of iteration-limit is reached."
	[g-a iteration-limit fitness-predicate]
	(let [evolution (take iteration-limit (iterate g-a-iteration g-a))
		solution (first (filter #(fitness-predicate (second (best-fit %))) evolution))]
		(if solution solution (last evolution))))

;; ------------------ Genetic algorithm execution with state persistence on disk
		
(defn evolve-solution-with-state 
	"Evolves the solution of a GA, saving in filename the state of the GA each savepoint-frequency generations."
	[g-a iteration-limit fitness-predicate filename savepoint-frequency]
	(let [newg-a (evolve-solution g-a savepoint-frequency fitness-predicate)]
		(spit filename (pr-str (dissoc newg-a :individual-generator :individual-mutator :selection-method :mutation-rate :keep-n :crossover-fn :fitness-fn)))
		(if (or (fitness-predicate (second (best-fit newg-a))) (< iteration-limit 0))
			newg-a
			(recur newg-a (- iteration-limit savepoint-frequency) fitness-predicate filename savepoint-frequency))))

(defn retrieve-algorithm-state 
	"Retrieves the state of a GA that was saved to disk by the evolve-solution-with-state function."
	[g-a filename]
	(merge g-a (read-string (slurp filename))))
	




