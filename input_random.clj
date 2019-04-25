(ns input-random)

; Products are simply numbers from 0 to 999.
(def products (range 2000))

; Generate store names
(defn gen-store-names [value]
  (str "Str", value))
; Stores are letters from A to Z.
(def stores (map gen-store-names (range 20)))


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
      (ref (vec
        (for [_s stores]
          (+ 1 (rand-int 1000))))))))

; Customers are randomly generated too.
(def customers
  (for [id (range 1000)]
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