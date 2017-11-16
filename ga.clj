;; gorilla-repl.fileformat = 1

;; **
;;; # A Simple Genetic Algorithm to Solve a Silly, Made-Up Problem
;;; 
;;; Here we'll demonstrate the core concepts of genetic algorithms, in Clojure, by writing code to evolve a sequence of 5 integers between 1 and 100 (inclusive) that, when divided in sequence, produce 5. 
;;; 
;;; That is, we want numbers a, b, c, d, e such that `a / b / c / d / e = 5`
;;; 
;;; It's a silly, made-up problem, but it will nonetheless allow us to see how genetic algorithms work, and how to implement them in Clojure.
;;; 
;;; First, we define the `ga` namespace, including the Gorilla REPL plotting library:
;; **

;; @@
(ns ga
  (:require [gorilla-plot.core :as plot]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; What should the "genomes" in our evolving population be? 
;;; 
;;; Since we're looking for a sequence of 5 integers, we'll just make each genome be a vector of 5 integers. 
;;; 
;;; Since the genetic algorithms will begin with a population of random genomes, we'll define a `random-genome` function that gives us one of these. We'll do this by using `repeatedly` to call a function five times, and we'll write that function to pick a number that can range from 0 to 99, and then add one to that result so that the "genes" in the genome can range from 1 to 100:
;; **

;; @@
(defn random-genome
  "Returns a random genome."
  []
  (vec (repeatedly 5 #(inc (rand-int 100)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ga/random-genome</span>","value":"#'ga/random-genome"}
;; <=

;; **
;;; Let's try it out:
;; **

;; @@
(random-genome)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>87</span>","value":"87"},{"type":"html","content":"<span class='clj-long'>93</span>","value":"93"},{"type":"html","content":"<span class='clj-long'>30</span>","value":"30"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>23</span>","value":"23"}],"value":"[87 93 30 8 23]"}
;; <=

;; **
;;; The next essential ingredient in a genetic algorithm is a "fitness function," which will take a genome and tell us how close or far it is from being a solution. 
;;; 
;;; This is sometimes also called an "error" function, because it's often formulated to give a value of zero for a perfect solution, with higher numbers for genomes that are worse. So the function tells you how bad a genome is, and the job of the genetic algorithm is to find a genome with zero badness.
;;; 
;;; Here we'll actually use the name `badness` for this function, just to make it clear that we're looking for something with a low value, and we'll define badness to be the absolute value of the difference between 5 and what we get from dividing the first number in the genome by the second, and then dividing that result by the third, and so on:
;; **

;; @@
(defn badness
  "Returns a number indicating how bad the genome is at solving the problem."
  [genome]
  (Math/abs (float (- (apply / genome) 5))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ga/badness</span>","value":"#'ga/badness"}
;; <=

;; **
;;; A couple of notes on that code above:
;;; 
;;; - `(apply / genome)`, if for example genome is `[3 4 10 2 1]`, is the same as `(/ 3 4 10 2 1)`. Calling `apply` with a function and a collection can be thought of as sticking the function inside a list of the collection's contents, and then evaluating the result.
;;; 
;;; - The call to `float` is necessary because the result of the divisions might be a "ratio," like `1/2`, rather than a floating-point number, like `0.5`. Clojure functions that process numbers can generally handle ratios as well as floating-point numbers, but here we're using the Java absolute value library function, `Math/abs`, which can be called from Clojure but isn't actually a Clojure function and will complain if it doesn't get an actual floating-point number.
;;; 
;;; Okay, let's look at the badness of a random genome:
;; **

;; @@
(let [my-genome (random-genome)]
  (str "The badness of " my-genome " is " (badness my-genome)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;The badness of [20 50 43 35 59] is 4.9999957&quot;</span>","value":"\"The badness of [20 50 43 35 59] is 4.9999957\""}
;; <=

;; **
;;; Now we need functions that will introduce random variation into genomes. As in most genetic algorithms, we'll define one, called `mutate`, which produces a child genome from a single parent genome, and another, called `crossover`, that produces a child from two parents.
;;; 
;;; For the `mutate` function, we'll randmly add or subtract 1 from a randomly chosen number in the genome, making sure that we don't let the new value go below 1 or above 100. We'll do this by picking a mutation point randomly and then concatenating the initial portion of the parent (obtained with `take`), a vector of just the altered number at the mutation point, and the tail portion of the parent (obtained with `drop`):
;; **

;; @@
(defn mutate
  "Returns a mutated version of genome."
  [genome]
  (let [mutation-point (rand-int 5)]
    (vec (concat (take mutation-point genome)
                 [(->> ((rand-nth [inc dec]) (nth genome mutation-point))
                       (max 1)
                       (min 100))]
                 (drop (inc mutation-point) genome)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ga/mutate</span>","value":"#'ga/mutate"}
;; <=

;; **
;;; Let's try that out a couple of times:
;; **

;; @@
(mutate [5 5 5 5 5])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[5 4 5 5 5]"}
;; <=

;; @@
(mutate [1 1 1 1 1])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[1 1 2 1 1]"}
;; <=

;; **
;;; Now let's define a crossover function that picks a crossover point and concatenates the initial portion of one parent with the tail portion of the other:
;; **

;; @@
(defn crossover
  "Returns the result of crossing over genome1 and genome2."
  [genome1 genome2]
  (let [crossover-point (rand-int 6)]
    (vec (concat (take crossover-point genome1)
                 (drop crossover-point genome2)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ga/crossover</span>","value":"#'ga/crossover"}
;; <=

;; @@
(crossover [1 1 1 1 1] [2 2 2 2 2])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[1 1 1 2 2]"}
;; <=

;; **
;;; It will be useful, for a couple of purposes, to be able to sort the population by badness, with the least bad genomes coming first. The built-in `sort-by` function makes this easy:
;; **

;; @@
(sort-by badness [[1 1 1 1 1]
                  [1 2 3 4 5]
                  [100 20 1 1 3]])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>100</span>","value":"100"},{"type":"html","content":"<span class='clj-long'>20</span>","value":"20"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[100 20 1 1 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[1 1 1 1 1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[1 2 3 4 5]"}],"value":"([100 20 1 1 3] [1 1 1 1 1] [1 2 3 4 5])"}
;; <=

;; **
;;; Now let's implement a `select` function that will give us a genome from the population that is randomly chosen to some extent, but likely to be one of the better ones. 
;;; 
;;; We'll do this by conducting a 5-genome tournament: We'll choose 5 genomes completely randomly, and then return the least bad of those 5.
;;; 
;;; This is particularly simple if we require that the population being passed to the `select` function is already sorted by badness, with the least bad at the front. Then we can conduct the tournament by selecting 5 random _indices_, and returning the genome at the lowest of those indices:
;; **

;; @@
(defn select
  "Returns a best genome of a randomly selected 5 from the sorted population."
  [population]
  (let [pop-size (count population)]
    (nth population
         (apply min (repeatedly 5 #(rand-int pop-size))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ga/select</span>","value":"#'ga/select"}
;; <=

;; **
;;; Now we are ready to define the top-level `evolve` function. 
;;; 
;;; We'll write it to take the population size as an argument, and then to run up to 100 generations, starting with a random population and generating each successive generation by mutation (half of the time), crossover (a quarter of the time), and just plain survival/cloning (the remaining quarter of the time).
;;; 
;;; We'll also add some code to print our progress as we go, and to plot the best badnesses per generation at the end, using the Gorilla REPL `plot` function:
;; **

;; @@
(defn evolve
  "Runs a genetic algorithm to solve the silly problem."
  [pop-size]
  (println "Starting evolution...")
  (loop [generation 0
         best-badnesses [] ;; accumulate these for plot
         population (sort-by badness (repeatedly pop-size random-genome))]
    (if (> generation 100)
      (do (println "Failure :-(")
        (plot/list-plot best-badnesses))
      (let [best (first population)
            best-badness (badness best)]
        (println "======================")
        (println "Generation:" generation)
        (println "Best badness:" best-badness)
        (println "Best genome:" best)
        (println "Median badness:" (badness (nth population 
                                                 (int (/ pop-size 2)))))
        (if (zero? best-badness) ;; success!
          (do (println "Success:" best)
            (plot/list-plot (conj best-badnesses 0)))
          (recur 
            (inc generation)
            (conj best-badnesses best-badness)
            (sort-by badness      
              (concat
                (repeatedly (* 1/2 pop-size) #(mutate (select population)))
                (repeatedly (* 1/4 pop-size) #(crossover (select population)
                                                         (select population)))
                (repeatedly (* 1/4 pop-size) #(select population))))))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ga/evolve</span>","value":"#'ga/evolve"}
;; <=

;; **
;;; Now let's give it a whirl, with a population size of 1000:
;; **

;; @@
(evolve 1000)
;; @@
;; ->
;;; Starting evolution...
;;; ======================
;;; Generation: 0
;;; Best badness: 4.775
;;; Best genome: [63 4 7 1 10]
;;; Median badness: 4.999986
;;; ======================
;;; Generation: 1
;;; Best badness: 4.642857
;;; Best genome: [100 4 7 1 10]
;;; Median badness: 4.999815
;;; ======================
;;; Generation: 2
;;; Best badness: 3.2222223
;;; Best genome: [64 2 3 1 6]
;;; Median badness: 4.9985595
;;; ======================
;;; Generation: 3
;;; Best badness: 1.4444444
;;; Best genome: [64 1 3 1 6]
;;; Median badness: 4.9930267
;;; ======================
;;; Generation: 4
;;; Best badness: 0.78571427
;;; Best genome: [59 1 7 2 1]
;;; Median badness: 4.87013
;;; ======================
;;; Generation: 5
;;; Best badness: 0.23809524
;;; Best genome: [100 3 7 1 1]
;;; Median badness: 4.6571426
;;; ======================
;;; Generation: 6
;;; Best badness: 0.0
;;; Best genome: [60 1 6 1 2]
;;; Median badness: 4.0634923
;;; Success: [60 1 6 1 2]
;;; 
;; <-
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"01c341e5-667e-4fae-a66d-3cda18fc9bf4","values":[{"x":0,"y":4.775000095367432},{"x":1,"y":4.642857074737549},{"x":2,"y":3.222222328186035},{"x":3,"y":1.4444444179534912},{"x":4,"y":0.7857142686843872},{"x":5,"y":0.2380952388048172},{"x":6,"y":0}]}],"marks":[{"type":"symbol","from":{"data":"01c341e5-667e-4fae-a66d-3cda18fc9bf4"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"01c341e5-667e-4fae-a66d-3cda18fc9bf4","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"01c341e5-667e-4fae-a66d-3cda18fc9bf4","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"01c341e5-667e-4fae-a66d-3cda18fc9bf4\", :values ({:x 0, :y 4.775} {:x 1, :y 4.642857} {:x 2, :y 3.2222223} {:x 3, :y 1.4444444} {:x 4, :y 0.78571427} {:x 5, :y 0.23809524} {:x 6, :y 0})}], :marks [{:type \"symbol\", :from {:data \"01c341e5-667e-4fae-a66d-3cda18fc9bf4\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"01c341e5-667e-4fae-a66d-3cda18fc9bf4\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"01c341e5-667e-4fae-a66d-3cda18fc9bf4\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@

;; @@
