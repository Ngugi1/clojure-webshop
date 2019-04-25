(ns sample)

(def logger (agent nil))
(dosync
    (send logger 
        (fn [_] (println "Hello world")))
)

(println (sort-by > [3 5 2 1]))

(shutdown-agents)

