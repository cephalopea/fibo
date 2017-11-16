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
(ns whispering-iceberg
  (:require [gorilla-plot.core :as plot]))

;;list of fibonacci numbers
(def fibonacci '(1 1))

;;adds next fibonacci number
(defn incfibo
  [fiblist]
  (cons (+' (first fiblist) (first (rest fiblist))) fiblist))

;;list of squares
(def squares '(4 1))

;;adds next square
(defn incsquares
  [squarelist]
  (cons (*' (inc (count squarelist)) (inc (count squarelist))) squarelist))

;;list of cubes
(def cubes '(8 1))

;;adds next cube
(defn inccubes 
  [cubelist]
  (cons (*' (inc (count cubelist)) (inc (count cubelist)) (inc (count cubelist))) cubelist))

;;list of primes
(def primes '(2 1))

;;adds next prime
(defn incprimes
  [primelist]
  (letfn [(thing 
            [nmbr dv ptlist]
            (loop [number nmbr
                   div dv
                   plist ptlist]
              (cond
                (and (= 0 div) (not= 0 (rem number (nth plist div)))) (cons number plist)
                :else (cond 
                        (= 0 (rem number (nth plist div))) (recur (inc' number) (- (count plist) 2) plist)
                        :else  (recur number (dec div) plist)))))]
    (thing (inc' (first primelist)) (- (count primelist) 2) primelist)))

(def triangles '(1 0))

(defn inctriangles
  [triangles]
  (cons (+ (first triangles) (count triangles)) triangles))

;;maps patterns and their incrementing functions
(def patternmap
  {:fibonacci {:pattern fibonacci
               :inc incfibo}
   :square {:pattern squares
            :inc incsquares}
   :cube {:pattern cubes
          :inc inccubes}
   :prime {:pattern primes
           :inc incprimes}
   :triangular {:pattern triangles
                :inc inctriangles}})

;;gives you a specific term (term) of a predefined pattern (patternkey)
(defn findn 
  [term patternkey]
  (let [pattern (get-in patternmap [patternkey :pattern])
        incx (get-in patternmap [patternkey :inc])]
    (loop [incy incx
           trm term
           ptrn pattern]
      (cond
        (< trm (count ptrn)) (nth ptrn (- (count ptrn) trm))
        (= trm (count ptrn)) (first ptrn)
        (> trm (count ptrn)) (recur incy trm (incy ptrn))
        :else "oops, findn is broken"))))

;;this is findn, except it gives the entire list up to the given term
(defn findton
  [term patternkey]
  (let [pattern (get-in patternmap [patternkey :pattern])
        incx (get-in patternmap [patternkey :inc])]
    (loop [incy incx
           trm term
           ptrn pattern]
      (cond
        (< trm (count ptrn)) (rest (split-at trm ptrn))
        (= trm (count ptrn)) ptrn
        (> trm (count ptrn)) (recur incy trm (incy ptrn))
        :else "oops, findton is broken"))))

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

;;combo finds numbers that fit two sequences: the first five primes in the fibonacci sequence, for example
;;safeguards against doing the thing for :square or :cube with :prime because i don't want to kill my computer
(defn combo
  [term patterna patternb]
  (let [a (get-in patternmap [patterna :pattern])
        b (get-in patternmap [patternb :pattern])
        inca (get-in patternmap [patterna :inc])
        incb (get-in patternmap [patternb :inc])]
    (letfn [(thing 
              [trm xx yy incxx incyy]
              (loop [term trm
                     x xx
                     y yy
                     incx incxx
                     incy incyy]
                (let [intersect (into () (clojure.set/intersection (set x) (set y)))
                      total (count intersect)]
                  (cond
                    (> total term) (first (split-at term intersect))
                    (= total term) intersect
                    (< total term) (recur term (incx x) (incy y) incx incy)
                    :else "oops, combo is broken"))))] 
      (if
        (or (= a primes) (= b primes)) (if
                                         (or (= a fibonacci) (= b fibonacci)) (sort (thing term a b inca incb))
                                         "aye, the haggis is in the fire for sure")
        (sort (thing term a b inca incb))))))

(combo 4 :prime :cube)

;;generates a list of random numbers between 0 and 100 with 100 members
(def randomlin 
  (repeatedly 10 #(rand-int 100)))

;;takes a list of numbers (numset), tells if its first member is contained within the fibonacci sequence (returns true or false).
(defn linfibo? 
  [numset]
  (let [maxx (last (sort numset))]
    (contains? (set (findtofn maxx :fibonacci)) (first numset))))

;;returns a sorted list of all fibonacci numbers from a list
(defn linsearch
  [goal space]
  (let [lump '()]
    (loop [remaining space
           lamp lump]
      (if (empty? remaining) 
        (sort lamp)
        (if (goal remaining) 
          (recur (rest remaining) (cons (first remaining) lamp))
          (recur (rest remaining) lamp))))))

;;tree with some randomish numbers
(def tree [[5] 38 [23 21 2] 4 86 34 2 75 64 8 45 [27 23 65 90 86 5 1 8 100] [9] 8 65 96 54 41 [77 34 66]])

;;tells whether a member of a collection is also a term of a given pattern
(defn ispattern? 
  [stuff patternkey] 
  (contains? (set (findtofn (apply max (flatten stuff)) patternkey)) (first stuff)))

;;depth-first search that returns list of all collection members in given pattern, takes tree and pattern from patternmap
(defn deepsearch
  [space patternkey]
  (letfn [(ispat? 
            [stuff] 
            (contains? (set (findtofn (apply max (flatten stuff)) patternkey)) (first stuff)))]
    (let [lamp '()]
      (loop [remaining space
             lump lamp]
        (if 
          (empty? remaining) (sort lump)
          (if 
            (sequential? (first remaining)) (recur (concat (first remaining) (rest remaining)) lump)
            (if
              (ispat? remaining) (recur (rest remaining) (cons (first remaining) lump))
              (recur (rest remaining) lump))))))))

;;a tree of 10 random numbers and their factors, if they aren't prime
;;original was [83 40 21 60 1 96 33 32 95 55] before factoring
(def factortree [[83 1] [40 [20 [10 [5 2] 2] 2] 1] [21 [7 3] 1] [60 [30 [15 [5 3] 2] 2] 1] [1 1] [96 [48 [24 [12 [6 [3 2] 2] 2] 2] 2] 1] [33 [11 3] 1] [32 [16 [8 [4 [2 2] 2] 2] 2] 1] [95 [19 5] 1] [55 [11 5] 1]])

;;depth search for prime factors (to be paired with factortree), returns number of steps to find all factors
(defn depthsearch
  [space]
  (let [lamp '()
        stops 0]
    (loop [remaining space
           lump lamp
           steps stops]
      (if 
        (empty? remaining) lump
        (if 
          (sequential? (first remaining)) (recur (concat (first remaining) (rest remaining)) lump (inc steps))
          (if
            (ispattern? remaining :prime) (recur (rest remaining) (cons (first remaining) lump) (inc steps))
            (recur (rest remaining) lump (inc steps))))))))

(depthsearch factortree)

(defn breadthsearch
  [space]
  (let [lamp '()
        stops 0]
    (loop [remaining space
           lump lamp
           steps stops]
      (if 
        (empty? remaining) lump
        (if 
          (sequential? (first remaining)) (recur (concat (rest remaining) (first remaining)) lump (inc steps))
          (if
            (ispattern? remaining :prime) (recur (rest remaining) (cons (first remaining) lump) (inc steps))
            (recur (rest remaining) lump (inc steps))))))))

(breadthsearch factortree)

;;not really sure what to do with this yet
(defn notpascal
  [layers patternkey]
  (let [lump []]
    (letfn ([makelayerx [x lamp]
             (loop [layer x
                    lomp lamp]
               (if
                 (> layer 0) (recur (dec layer) (cons (flatten (cons (sort (findton layer patternkey)) (findton layer patternkey))) lomp))
                 lomp))])
      (makelayerx layers lump))))

(notpascal 10 :prime)

;;combo, but using explicit search
;;way slower but works
;;also duplicates the 1 in fibonacci sequence
(defn recombo
  [amt patterna patternb]
  (let [a (get-in patternmap [patterna :pattern])
        b (get-in patternmap [patternb :pattern])
        inca (get-in patternmap [patterna :inc])
        incb (get-in patternmap [patternb :inc])]
    (loop [y b
           term amt]
      ;;search for members of one pattern in the search space of another pattern
      (if 
        (< (count (deepsearch (findtofn (apply max y) patterna) patternb)) term) (recur (incb y) term)
        (deepsearch (findtofn (apply max y) patterna) patternb)))))

(combo 4 :fibonacci :prime)

(recombo 5 :fibonacci :prime)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"(1 1 2 3 5)"}
;; <=

;; @@

;; @@
