(ns litterbox.sea.entities
  (use [datomic.api :only [q db] :as d]))


(def entity-schema "[ {:db/id #db/id[:db.part/db]
  :db/ident :entity
  :db/valueType :db.type/long
  :db/cardinality :db.cardinality/one
  :db/doc \"An entity's id\"
  :db.install/_attribute :db.part/db}]")

