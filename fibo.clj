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
                (= number div) (cons number plist)
                (or (= 0 (rem number div)) (< number div)) (recur (inc' number) 2 plist) 
                :else (recur number (inc' div) plist))))]
    (thing (inc' (first primelist)) 2 primelist)))

;;maps patterns and their incrementing functions
(def patternmap
  {:fibonacci {:pattern fibonacci
               :inc incfibo}
   :square {:pattern squares
            :inc incsquares}
   :cube {:pattern cubes
          :inc inccubes}
   :prime {:pattern primes
           :inc incprimes}})

;;gives you a specific term (term) of a predefined pattern (pattern)
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
        (< trm (count ptrn)) (first (split-at trm ptrn))
        (= trm (count ptrn)) ptrn
        (> trm (count ptrn)) (recur incy trm (incy ptrn))
        :else "oops, findton is broken"))))

;;combo finds numbers that fit two sequences: the first five primes in the fibonacci sequence, for example
;;don't use :square or :cube with :prime, results in stack overflow for all the reasons
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
      (sort (thing term a b inca incb)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;whispering-iceberg/combo</span>","value":"#'whispering-iceberg/combo"}
;; <=
