(ns input-random)

;;; This code is from this github repository - https://github.com/trystan/random-seed/tree/master/src/random_seed
(defonce rng (new java.util.Random))
(defn set-random-seed!
  "Sets the seed of the global random number generator."
  [seed]
  (.setSeed rng seed))
(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive). Works like clojure.core/rand except it
  uses the seed specified in set-random-seed!."
  ([] (.nextFloat rng))
  ([n] (* n (rand))))
(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive).
  Works like clojure.core/rand except it uses the seed specified in
  set-random-seed!."
  [n]
  (int (rand n)))
(set-random-seed! 10)

;; End of github code

; Products are simply numbers from 0 to 999.
(def products (range 1000))

; Generate store names
(defn gen-store-names [value]
  (str "Str", value))
; Stores are letters from A to Z.
(def stores (map gen-store-names (range 5)))
; Prices are randomly generated numbers between 1 and 100.
(def prices
  (vec
   (for [_p products]
     (vec
      (for [_s stores]
        (+ 1 (rand-int 100)))))))

; Stock are randomly generated numbers between 1 and 1000.
(def stock
  (vec
   (for [_p products]
      (for [_s stores]
        (ref (+ 1 (rand-int 1000)) :validator (fn [val] (>= val 0)))))))

; Customers are randomly generated too.
(def customers
  (for [id (range 10000)]
    (let [n-products
          (+ 1 (rand-int 100))
          ; Number of products in shopping list is random number between 1 and
          ; 100
          selected-products
          (distinct
           (for [_i (range n-products)]
             (rand-nth products)))
          ; Products in shopping list are randomly chosen from all products.
          ; We remove duplicates.
          products-and-number
          (for [product selected-products]
            [product (+ 1 (rand-int 10))])]
      ; We put between 1 and 10 of each item in our list.
      {:id id :products products-and-number})))

; Time in milliseconds between sales periods
(def TIME_BETWEEN_SALES 50)
; Time in milliseconds of sales period
(def TIME_OF_SALES 10)
