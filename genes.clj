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

;;list of fibonacci numbers
(def fibonacci '(1 1))

;;adds next fibonacci number
(defn incfibo
  [fiblist]
  (cons (+' (first fiblist) (first (rest fiblist))) fiblist))
  
(def patternmap
  {:fibonacci {:pattern fibonacci
               :inc incfibo}})
               
;;increases a given pattern (patternkey) until its largest member is greater than or equal to a value (value)             
(defn findtofn
  [value patternkey]
  (let [pattern (get-in patternmap [patternkey :pattern])
        incx (get-in patternmap [patternkey :inc])]
    (loop [incy incx
           ptrn pattern
           vlu value]
      (cond 
        (or (> (first ptrn) vlu) (= (first ptrn) vlu)) ptrn
        (< (first ptrn) vlu) (recur incy (incy ptrn) vlu)))))

;;returns a random genome with 4 random integers and 2 fibonacci numbers at the end
(defn random-genome
  []
  (let [genes (repeatedly 4 #(inc (rand-int 100)))]
    (let [fibo (findtofn (apply max genes) :fibonacci)]
      (vec (reverse (cons (first fibo) (cons (first (rest fibo)) genes)))))))

(random-genome)

;;returns a number indicating how bad the genome is at being an ascending fibonaccish trio
(defn badness
  [genome]
  (let [member 0
        badscore 0]
    (loop [n member
           badness badscore]
      (if 
        (> n 3) (if 
                  (= genome (sort genome)) badness
                  (+ badness 100))
        (recur (+ 1 n) (+ badness (Math/abs (float (- (+ (nth genome n) (nth genome (+ n 1))) (nth genome (+ n 2)))))))))))

(let [my-genome (random-genome)]
  (str "The badness of " my-genome " is " (badness my-genome)))

(defn mutate
  "Returns a mutated version of genome."
  [genome]
  (let [mutation-point (rand-int 4)]
    (vec (concat (take mutation-point genome)
                 [(->> ((rand-nth [inc dec]) (nth genome mutation-point))
                       (max 1)
                       (min 100))]
                 (drop (inc mutation-point) genome)))))

(defn crossover
  "Returns the result of crossing over genome1 and genome2."
  [genome1 genome2]
  (let [crossover-point (rand-int 4)]
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
                       (repeatedly (* 1/2 pop-size) #(mutate (select population)))
                       (repeatedly (* 1/4 pop-size) #(crossover (select population)
                                                                (select population)))
                       (repeatedly (* 1/4 pop-size) #(select population))))))))))

(evolve 1000)
;; @@
;; ->
;;; Starting evolution...
;;; ======================
;;; Generation: 0
;;; Best badness: 125.0
;;; Best genome: [10 5 10 2 8 13]
;;; Median badness: 281.0
;;; ======================
;;; Generation: 1
;;; Best badness: 39.0
;;; Best genome: [12 15 44 47 89 144]
;;; Median badness: 220.0
;;; ======================
;;; Generation: 2
;;; Best badness: 10.0
;;; Best genome: [5 11 12 24 34 55]
;;; Median badness: 183.0
;;; ======================
;;; Generation: 3
;;; Best badness: 3.0
;;; Best genome: [6 6 14 20 34 55]
;;; Median badness: 151.0
;;; ======================
;;; Generation: 4
;;; Best badness: 4.0
;;; Best genome: [6 6 13 20 34 55]
;;; Median badness: 125.0
;;; ======================
;;; Generation: 5
;;; Best badness: 2.0
;;; Best genome: [6 7 13 20 34 55]
;;; Median badness: 39.0
;;; ======================
;;; Generation: 6
;;; Best badness: 1.0
;;; Best genome: [6 7 13 21 34 55]
;;; Median badness: 27.0
;;; ======================
;;; Generation: 7
;;; Best badness: 1.0
;;; Best genome: [6 7 13 21 34 55]
;;; Median badness: 9.0
;;; ======================
;;; Generation: 8
;;; Best badness: 0.0
;;; Best genome: [5 8 13 21 34 55]
;;; Median badness: 5.0
;;; Success: [5 8 13 21 34 55]
;;; 
;; <-
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"e41e06b4-1f09-42e2-b5e2-14d569f62c0e","values":[{"x":0,"y":125.0},{"x":1,"y":39.0},{"x":2,"y":10.0},{"x":3,"y":3.0},{"x":4,"y":4.0},{"x":5,"y":2.0},{"x":6,"y":1.0},{"x":7,"y":1.0},{"x":8,"y":0}]}],"marks":[{"type":"symbol","from":{"data":"e41e06b4-1f09-42e2-b5e2-14d569f62c0e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"e41e06b4-1f09-42e2-b5e2-14d569f62c0e","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"e41e06b4-1f09-42e2-b5e2-14d569f62c0e","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"e41e06b4-1f09-42e2-b5e2-14d569f62c0e\", :values ({:x 0, :y 125.0} {:x 1, :y 39.0} {:x 2, :y 10.0} {:x 3, :y 3.0} {:x 4, :y 4.0} {:x 5, :y 2.0} {:x 6, :y 1.0} {:x 7, :y 1.0} {:x 8, :y 0})}], :marks [{:type \"symbol\", :from {:data \"e41e06b4-1f09-42e2-b5e2-14d569f62c0e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"e41e06b4-1f09-42e2-b5e2-14d569f62c0e\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"e41e06b4-1f09-42e2-b5e2-14d569f62c0e\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@

;; @@

;; @@

;; @@
