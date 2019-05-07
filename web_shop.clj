(ns web-shop
  (:require [clojure.pprint] ; For 'pretty-printing'
            ; Choose one of the input files below.
            ;[input-simple :as input]
            ;[input-random :as input]
            ))
;;;;;;; Input values
(def N-CUSTOMER (Integer/parseInt (nth *command-line-args* 0 "1000")))
(def N-PRODUCTS (Integer/parseInt (nth *command-line-args* 1 "10")))
(def N-STORES (Integer/parseInt (nth *command-line-args* 2 "5")))
(def MAX-ITEMS-IN-SHOPPING-LIST (Integer/parseInt (nth *command-line-args* 3 "100")))
(def MAX-COUNT-PER-SHOPPED-ITEM (Integer/parseInt (nth *command-line-args* 4 "10")))
(def N-THREADS (Integer/parseInt (nth *command-line-args* 5 "3")))
(def TIME_BETWEEN_SALES (Integer/parseInt (nth *command-line-args* 6 "50")))
; Time in milliseconds of sales period
(def TIME_OF_SALES (Integer/parseInt (nth *command-line-args* 7 "10")))
(def MAX_STOCK_STORE (Integer/parseInt (nth *command-line-args* 8 "50")))
;;;;;;;;;;;;;;;;;;;


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
(def products (range N-PRODUCTS))

; Generate store names
(defn gen-store-names [value]
  (str "Str", value))
; Stores are letters from A to Z.
(def stores (map gen-store-names (range N-STORES)))
; Prices are randomly generated numbers between 1 and 100.
(def prices
  (ref (vec
   (for [_p products]
     (vec
      (for [_s stores]
        (+ 1 (rand-int 100))))))))

; Stock are randomly generated numbers between 1 and 1000.
(def stock
  (vec
   (for [_p products]
     (for [_s stores]
       (ref (+ 1 (rand-int MAX_STOCK_STORE)) :validator (fn [val] (>= val 0)))))))

; Customers are randomly generated too.
(def customers
  (for [id (range N-CUSTOMER)]
    (let [n-products
          (+ 1 (rand-int MAX-ITEMS-IN-SHOPPING-LIST))
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
            [product (+ 1 (rand-int MAX-COUNT-PER-SHOPPED-ITEM))])]
      ; We put between 1 and 10 of each item in our list.
      {:id id :products products-and-number})))
;;;;;;;;;;;;;;;;;;


; Logging
(def logger (agent nil))
;(defn log [& msgs] (send logger (fn [_] (apply println msgs)))) ; uncomment this to turn ON logging
(defn log [& msgs] nil) ; uncomment this to turn OFF logging



; We simply copy the products from the input file, without modifying them.

(defn product-name->id [name]
  "Return id (= index) of product with given `name`.
  E.g. (product-name->id \"Apple\") = 0"
  (.indexOf products name))

(defn product-id->name [id]
  "Return name of product with given `id`.
  E.g. (product-id->name 0) = \"Apple\""
  (nth products id))


; We simply copy the stores from the input file, without modifying them.
(defn store-name->id [name]
  "Return id (= index) of store with given `name`.
  E.g. (store-name->id \"Aldi\") = 0"
  (.indexOf stores name))

(defn store-id->name [id]
  "Return name of store with given `id`.
  E.g. (store-id->name 0) = \"Aldi\""
  (nth stores id))


; We wrap the prices from the input file in a single atom in this
; implementation. You are free to change this to a more appropriate mechanism.
(defn- get-price [store-id product-id]
  "Returns the price of the given product in the given store."
  (nth (nth @prices product-id) store-id))

(defn- get-total-price [store-id product-ids-and-number]
  "Returns the total price for a given number of products in the given store."
  (reduce +
    (map
      (fn [[product-id n]]
        (* n (get-price store-id product-id)))
      product-ids-and-number)))

(defn- set-price [store-id product-id new-price]
  "Set the price of the given product in the given store to `new-price`."
   (alter prices assoc-in [product-id store-id] new-price))


; We wrap the stock from the input file in a single atom in this
; implementation. You are free to change this to a more appropriate mechanism.
(defn print-stock [stock]
  "Print stock. Note: `stock` should not be an atom/ref/... but the value it
  contains."
  (println "Stock:")
  ; Print header row with store names (abbreviated to four characters)
  (doseq [store stores]
    (print (apply str (take 5 store)) ""))
  (println)
  ; Print table
  (doseq [product-id (range (count stock))]
    ; Line of numbers
    (doseq [number-in-stock (nth stock product-id)]
      (print (clojure.pprint/cl-format nil "~4d " @number-in-stock)))
    ; Name of product
    (println (product-id->name product-id))))

(defn- product-available? [store-id product-id n]
  "Returns true if at least `n` of the given product are still available in the
  given store."

  (dosync (>= @(nth (nth stock product-id) store-id) n)))

(defn- buy-product [store-id product-id n]
  "Updates `stock` to buy `n` of the given product in the given store."
  (dosync (alter (nth (nth stock product-id) store-id)
          (fn [available]
            (- available n)))))

(defn- find-available-stores [product-ids-and-number]
  "Returns the id's of the stores in which the given products are still
  available."
  (dosync (filter
    (fn [store-id]
      (every?
        (fn [[product-id n]] (product-available? store-id product-id n))
        product-ids-and-number))
    (map store-name->id stores))))

(defn buy-products [store-id product-ids-and-number]
    (doseq [[product-id n] product-ids-and-number]
      ; Try to buy all products at the same time
      (buy-product store-id product-id n)))

;; Retries
(def retries (atom 0))
(defn- process-customer [customer]
  "Process `customer`. Consists of three steps:
  1. Finding all stores in which the requested products are still available.
  2. Sorting the found stores to find the cheapest (for the sum of all products).
  3. Buying the products by updating the `stock`.
  Note: because this implementation is sequential, we do not suffer from
  inconsistencies. That will be different in your implementation."
  (dosync
   (swap! retries inc)
   (let
     [product-ids-and-number
          (map (fn [[name number]] [(product-name->id name) number])
            (:products customer))
        available-store-ids  ; step 1
          (find-available-stores product-ids-and-number)
        cheapest-store-id  ; step 2
          (first  ; Returns nil if there's no available stores
            (sort-by
              ; sort stores by total price
              (fn [store-id] (get-total-price store-id product-ids-and-number))
              available-store-ids))]
     ; (println (str "\n\n\n-- ", (customer :id), "-- Before" ,(transpose @prices)))
    (if (nil? cheapest-store-id)
      (log "Customer" (:id customer) "could not find a store that has"
        (:products customer))
      (do
        (buy-products cheapest-store-id product-ids-and-number) ;  step 3
        (log "Customer" (:id customer) "bought" (:products customer) "in"
          (store-id->name cheapest-store-id) ))))
;   "@price - " (get-total-price cheapest-store-id product-ids-and-number)
   ;(println (str "\n\n\n ---", (customer :id), "After" ,(transpose @prices)))
   )
  )

(def finished-processing?
  "Set to true once all customers have been processed, so that sales process
  can end."
  (atom false))

(defn process-customers [customers]
  "Process `customers` one by one. In this code, this happens sequentially. In
  your implementation, this should be parallelized."
  (def WORKSIZE (quot (count customers) N-THREADS))
  (def REM (mod (count customers) N-THREADS))
  (def processed (atom 0))
  (def futures (atom []))
  (doseq  [i (range 1 N-THREADS)]
    (if (<= i  REM)
      (do
        (swap! futures conj (future
                             (doseq [k (range @processed (+ @processed WORKSIZE 1))]
                               (println "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
                               (process-customer (nth customers k)))))
        (reset! processed (+ @processed WORKSIZE 1))
       )
      (do
        (swap! futures conj (future
                             (doseq [j (range @processed (+ @processed WORKSIZE))]
                               (println "=======================================================")
                               (process-customer (nth customers j)))))
        (reset! processed (+ @processed WORKSIZE))
        )))

  (doseq [f @futures]
    @f)
;  (dotimes [i N-THREADS]
;    (if (<= i REM)
;      (swap! futures conj (future
;       (doseq [c [subvec @processed (+ @processed WORKSIZE 1)]])))
;      (swap! futures conj (future
;       (doseq [c [subvec @processed (+ @processed WORKSIZE)]])))))
;  (println (count @futures))
;  (doseq [i @futures]
;    @i)



  ;(doall (pmap process-customer customers))
  (reset! finished-processing? true)
  )




(defn start-sale [store-id]
  "Sale: -10% on `store-id`."
  (dosync
   (log "Start sale for store" (store-id->name store-id))
   (doseq [product-id (range (count products))]
     (set-price store-id product-id (* (get-price store-id product-id) 0.90)))
   ))

(defn end-sale [store-id]
  "End sale: reverse discount on `store-id`."
  (dosync (log "End sale for store" (store-id->name store-id))
          (doseq [product-id (range (count products))]
            (set-price store-id product-id (/ (get-price store-id product-id) 0.90)))))

(defn sales-process []
  "The sales process starts and ends sales periods, until `finished-processing?`
  is true."
  (loop []
    (let [store-id (store-name->id (rand-nth stores))]
      (Thread/sleep TIME_BETWEEN_SALES)
      (start-sale store-id)
      (Thread/sleep TIME_OF_SALES)
      (end-sale store-id))
    (if (not @finished-processing?)
      (recur))))


(defn main []
  (print-stock stock)
  (let [f1 (future (process-customers customers))
        f2 (future (sales-process))
        start (System/nanoTime)]
    @f1
    (println (str (/ (- (System/nanoTime) start) 1e6) "," @retries "\n"))
    @f2
    (log @retries)
    (await logger))

  (print-stock stock)
  )


(def rets (atom []))
(dotimes [_ 1]
  @(future (main))
  (reset! retries 0)
  (swap! rets conj retries)
  )


(shutdown-agents)
