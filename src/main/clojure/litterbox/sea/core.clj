(ns litterbox.sea.core
  (:use [datomic.api :as d]))

(comment

(def uri "datomic:mem://sea")

(d/delete-database uri)
(d/create-database uri)

(def conn (d/connect uri))

;(def schema-attrs (q '[:find ?name :where [_ :db.install/attribute ?a] [?a :db/ident :entity]]))

(def sea-schema (read-string (slurp "/home/bhalbert/code/clj/litter-box/resources/sea-schema.edn")))

(d/transact conn sea-schema)

(defn new-name
  ([name]
     (new-name (new-entity) name))
  ([e name]
     (merge e
            {:attribute.name/name name})))

(defn new-position
  ([x y]
     (new-position (new-entity) x y))
  ([e x y]
     (merge e
            {:attribute.position/x x
             :attribute.position/y y})))

(defn new-entity []
  {:db/id #db/id[:attributes]})

(d/transact conn bob)

(def named-entities-q '[:find ?e ?n :where [?e :attribute/name ?n]])

(def positioned-entities-q
  '[:find ?e ?p-x ?p-y
    :where [?e :attribute.position/x ?p-x]
    [?e :attribute.position/y ?p-y]])

(def named-rs (d/q named-entities-q (d/db conn)))
(def positioned-rs (d/q positioned-entities-q (d/db conn)))

(defn set-attr [e & kvs]
  [(apply assoc {:db/id (:db/id e)} kvs)])

)