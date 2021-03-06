(ns input-simple)

; Products
(def products
  ["Apple" "Avocado" "Banana" "Pear"])

; Stores
(def stores
  ["Aldi" "Carrefour" "Colruyt" "Delhaize" "Lidl"])

; The price of each item in each store.
;
; An apple costs €0.25 in Aldi, €0.30 in Carrefour, etc.
(def prices
   ;Aldi Carr Colr Delh Lidl
  [[0.25 0.30 0.28 0.29 0.27] ; Apple
   [1.37 1.20 1.25 1.20 1.32] ; Avocado
   [0.41 0.35 0.35 0.36 0.45] ; Banana
   [0.19 0.21 0.19 0.25 0.18] ; Pear
  ])

; The number of items left of each item in each store.
;
; E.g. there are 150 apples left at Aldi, 250 apples at Carrefour, etc.
; Make sure individual types of products can be locked independently
(def stock
        ; Aldi Carr Colr Delh Lidl
  [[  (ref 15)  (ref 25)   (ref 30)   (ref 29)   (ref 15)] ; Apple
   [  (ref 5)   (ref 7)    (ref 6)    (ref 10)   (ref 2)] ; Avocado
   [  (ref 2)   (ref 10)   (ref 20)   (ref 17)   (ref 8)] ; Banana
   [  (ref 25)  (ref 17)   (ref 31)   (ref 18)   (ref 20)] ; Pear
  ])

; Customers and the products they want to buy.
;
; E.g. customer 0 wants to buy 5 bananas and 7 apples.
(def customers
  [{:id 0 :products [["Banana" 5] ["Apple" 7]]}
   {:id 1 :products [["Banana" 7] ["Apple" 9]]}
   {:id 2 :products [["Pear" 15]]}
   {:id 3 :products [["Apple" 15] ["Avocado" 4] ["Banana" 7] ["Pear" 2]]}
   {:id 4 :products [["Apple" 5] ["Apple" 2]]}
   {:id 5 :products [["Banana" 4]]}
   {:id 6 :products [["Avocado" 2] ["Apple" 1]]}
   {:id 7 :products [["Banana" 3] ["Apple" 5] ["Pear" 7]]}
   {:id 8 :products [["Pear" 1] ["Banana" 4]]}
   {:id 9 :products [["Apple" 6] ["Pear" 4] ["Avocado" 3]]}
  ])

; Time in milliseconds between saxles periods
(def TIME_BETWEEN_SALES 20)
; Time in milliseconds of sales period
(def TIME_OF_SALES 10)
