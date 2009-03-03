;;;; Implementation of the solution to the Santa Claus problem
;;;; from the beautiful_concurrency.pdf by Simon Peyton-Jones
;;;;
;;;; For this first iteration, we're going to keep it as close to his
;;;; implementation as Clojure and our understanding of both will allow

(ns 'santa)

(defstruct elf ::id ::group ::task)

(defn make-elf [id grp]
  (struct elf id grp #(print-str "Elf" id "meeting in the study")))

(defstruct deer ::id ::group ::task)

(defn make-deer [id grp]
  (struct deer id grp #(print-str "Deer" id "delivering toys to all the bad little girls & boys!")))

(defstruct gate ::capacity ::rem-capacity)

(defn make-gate [n]
  (struct gate n (ref n)))

(defstruct group-state ::openings ::ingate ::outgate)

(defn make-group-state [n in out]
  (struct group-state n in out))

(defstruct group ::capacity ::state)

(defn make-group [n]
  (dosync
   (struct group n (ref (make-group-state n (make-gate n) (make-gate n))))))
  
(defn workflow [grp task]
  (do
   (let [[ingate outgate] (join-grp grp)]
     (do
       (pass-gate ingate)
       (task)
       (pass-gate outgate)))))
  
(defmacro check [pred tx]
  (when-not pred (throw (RuntimeException. "Failed check!"))))

(defn pass-gate [{rem ::rem-capacity}]
  (dosync
   (while (zero? @rem)
     ; put sleep here
     (Thread/sleep 500)
     )
   (alter rem dec)))

(defn operate-gate [{rem ::rem-capacity} :as gate]
 (do
   (dosync
    (ref-set rem (::capacity gate)))
   (dosync
    (while (pos? @rem)
      ;this is a blocking loop
     (Thread/sleep 500)
      ;may need to sleep or something here...
      ))))

(defn join-grp [{state ::state} :as grp]
  (dosync
   (while (zero? (::openings @state))
      ;this is a blocking loop
     (Thread/sleep 500)
      ;may need to sleep or something here...
     )
   (alter state #(assoc % ::openings (dec (::openings %))))
   (let [curstate @state]
     (vector (::ingate state)(::outgate state)))))

(defn await-grp [{state ::state} :as grp]
  (do
   (while (pos? (::openings @state))
      ;this is a blocking loop
     (Thread/sleep 500)
      ;may need to sleep or something here...
     )
   (let [n (::capacity grp)
	 new-ingate (make-gate n)
	 new-outgate (make-gate n)]
     (ref-set state (make-group-state n new-ingate new-outgate))
     (vector new-ingate new-outgate))))

(defn main [n-elfs-in-grp n-deer-in-grp]
  (let [elf-grp (make-group n-elfs-in-grp)
	deer-grp (make-group n-deer-in-grp)]
    (do
      (for [n (range 1 10)] (make-elf n elf-grp))
      (for [n (range 1 9)] (make-deer n deer-grp))
      (forever (make-santa elf-grp deer-grp)))))