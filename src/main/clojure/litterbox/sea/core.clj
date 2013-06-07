(ns 'litterbox.sea.core
  (use [datomic.api :only [q db] :as d]))

(def uri "datomic:mem://sea")

(d/create-database uri)

(def conn (d/connect uri))

(def schema-attrs (q '[:find ?name :where [_ :db.install/attribute ?a] [?a :db/ident :entity]]))
