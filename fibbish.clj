;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns bad-wolf
  (:require [gorilla-plot.core :as plot]))

;;notes: clojure has an exponent function: (expt a b) = a^b
;;https://stackoverflow.com/questions/5057047/how-to-do-exponentiation-in-clojure
;;this will be useful/necessary for implementing binet formula
;;can also use exponents for square roots
;;kirsten idea: two badness functions? one binet, one ordered
;;average vs adding badness scores

;;returns a random genome with 4 random integers and 2 fibonacci numbers at the end
(defn random-genome
  []
  (repeatedly 6 #(inc (rand-int 100))))

(random-genome)

;;returns a number indicating how bad the genome is at being an ascending fibonaccish trio
(defn badness
  [genome]
  (let [member 0
        badscore 0]
    (loop [n member
           badness badscore]
      (if (> n 3) 
        (if (= genome (sort genome)) 
          badness
          (+ badness 100))
        (recur (+ 1 n) 
               (+ badness 
                  (Math/abs (float (- (+ (nth genome n) 
                                         (nth genome (+ n 1))) 
                                      (nth genome (+ n 2)))))))))))

(let [my-genome (random-genome)]
  (str "The badness of " my-genome " is " (badness my-genome)))

(defn mutate
  "Returns a mutated version of genome."
  [genome]
  (let [mutation-point (rand-int 6)]
    (vec (concat (take mutation-point genome)
                 [(->> ((rand-nth [inc dec]) (nth genome mutation-point))
                       (max 1)
                       (min 100))]
                 (drop (inc mutation-point) genome)))))

(defn crossover
  "Returns the result of crossing over genome1 and genome2."
  [genome1 genome2]
  (let [crossover-point (rand-int 6)]
    (vec (concat (take crossover-point genome1)
                 (drop crossover-point genome2)))))

(defn select
  "Returns a best genome of a randomly selected 5 from the sorted population."
  [population]
  (let [pop-size (count population)]
    (nth population
         (apply min (repeatedly 5 #(rand-int pop-size))))))

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
                       (repeatedly (* 3/4 pop-size) #(mutate (select population)))
                       (repeatedly (* 1/2 pop-size) #(crossover (select population)
                                                                (select population)))
                       (repeatedly (* 1/4 pop-size) #(select population))))))))))

(evolve 1000)
;; @@
;; ->
;;; Starting evolution...
;;; ======================
;;; Generation: 0
;;; Best badness: 141.0
;;; Best genome: (3 35 4 36 37 74)
;;; Median badness: 330.0
;;; ======================
;;; Generation: 1
;;; Best badness: 101.0
;;; Best genome: [20 20 22 58 61 71]
;;; Median badness: 230.0
;;; ======================
;;; Generation: 2
;;; Best badness: 27.0
;;; Best genome: [11 11 15 31 37 74]
;;; Median badness: 188.0
;;; ======================
;;; Generation: 3
;;; Best badness: 16.0
;;; Best genome: [1 13 14 39 57 96]
;;; Median badness: 160.0
;;; ======================
;;; Generation: 4
;;; Best badness: 16.0
;;; Best genome: [3 13 15 39 57 95]
;;; Median badness: 140.0
;;; ======================
;;; Generation: 5
;;; Best badness: 15.0
;;; Best genome: [3 13 15 38 57 95]
;;; Median badness: 92.0
;;; ======================
;;; Generation: 6
;;; Best badness: 8.0
;;; Best genome: [11 11 22 38 57 95]
;;; Median badness: 30.0
;;; ======================
;;; Generation: 7
;;; Best badness: 7.0
;;; Best genome: [11 11 22 35 57 97]
;;; Median badness: 21.0
;;; ======================
;;; Generation: 8
;;; Best badness: 4.0
;;; Best genome: [11 11 23 35 58 95]
;;; Median badness: 16.0
;;; ======================
;;; Generation: 9
;;; Best badness: 1.0
;;; Best genome: [4 17 21 37 58 95]
;;; Median badness: 13.0
;;; ======================
;;; Generation: 10
;;; Best badness: 1.0
;;; Best genome: [4 17 21 37 58 95]
;;; Median badness: 9.0
;;; ======================
;;; Generation: 11
;;; Best badness: 1.0
;;; Best genome: [11 12 23 35 58 94]
;;; Median badness: 7.0
;;; ======================
;;; Generation: 12
;;; Best badness: 0.0
;;; Best genome: [5 16 21 37 58 95]
;;; Median badness: 5.0
;;; Success: [5 16 21 37 58 95]
;;; 
;; <-
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"45a280c6-e666-491b-96cf-7f4eb51349ef","values":[{"x":0,"y":141.0},{"x":1,"y":101.0},{"x":2,"y":27.0},{"x":3,"y":16.0},{"x":4,"y":16.0},{"x":5,"y":15.0},{"x":6,"y":8.0},{"x":7,"y":7.0},{"x":8,"y":4.0},{"x":9,"y":1.0},{"x":10,"y":1.0},{"x":11,"y":1.0},{"x":12,"y":0}]}],"marks":[{"type":"symbol","from":{"data":"45a280c6-e666-491b-96cf-7f4eb51349ef"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"45a280c6-e666-491b-96cf-7f4eb51349ef","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"45a280c6-e666-491b-96cf-7f4eb51349ef","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"45a280c6-e666-491b-96cf-7f4eb51349ef\", :values ({:x 0, :y 141.0} {:x 1, :y 101.0} {:x 2, :y 27.0} {:x 3, :y 16.0} {:x 4, :y 16.0} {:x 5, :y 15.0} {:x 6, :y 8.0} {:x 7, :y 7.0} {:x 8, :y 4.0} {:x 9, :y 1.0} {:x 10, :y 1.0} {:x 11, :y 1.0} {:x 12, :y 0})}], :marks [{:type \"symbol\", :from {:data \"45a280c6-e666-491b-96cf-7f4eb51349ef\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"45a280c6-e666-491b-96cf-7f4eb51349ef\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"45a280c6-e666-491b-96cf-7f4eb51349ef\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@

;; @@

;; @@

;; @@
