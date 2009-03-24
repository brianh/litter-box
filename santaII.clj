;;;; Implementation of the solution to the Santa Claus problem
;;;; from the beautiful_concurrency.pdf by Simon Peyton-Jones
;;;;
;;;; For this second attempt, I'm going to implement it as best
;;;; I can in the Clojure idioms (given my current, limited knowledge
;;;; of such).  I'm sure I'll be corrected.... ;)

(ns litterbox.santaII
  (:use litterbox.utils))

(comment
  (load-file "C:\\home\\lisp\\clj\\src\\git-repos\\litterbox\\santaII.clj")
)

(def output-lock (ref 0))
(def screw-off-time 10000) ;in milliseconds

(defmulti screw-off :Type)
(defmulti queue-up :Type)

(defmethod screw-off :elf [s]
  (do
    (display "Elf" (:id s) "pretending to do something...")
    (ignore [InterruptedException] (sleep-rnd screw-off-time))
    (display "Elf" (:id s) "done pretending...")
    (send-off s queue-up)
    s))
(defmethod screw-off :deer [s]
  (do
    (display "Deer" (:id s) "off on holiday...")
    (ignore [InterruptedException] (sleep-rnd screw-off-time))
    (send-off s queue-up)
    s))

(defmethod queue-up :elf [s]
  (do
    (display "Elf" (:id s) "figures it's time to see the fat old bastard...")
    (handle [Exception (do
			 (display "Elf" (:id s) "tried it's best.  Better luck next time.")
			 (send-off s screw-off))]
	    (dosync
	     (let [place-ref (:mtg-place s)
		   place @place-ref]
	       (if (:full @place-ref)
		 (throw (Exception. "No room"))
		 (do
		   (let [q (:queue place)
			 newq (conj q s)
			 cap (:capacity place)]
		     (ensure place-ref)
		     (alter place-ref assoc :queue newq :full (< (count newq) cap))))))))
    s))

(defn main [elf-cap deer-cap]
  (let [study (ref {:capacity elf-cap :full nil :queue [] :in []}); :validator valid-mtg-state?)
	sleigh (ref {:capacity elf-cap :full nil :queue [] :in []})
        elves (for [n (range 1 10)] 
		(agent {:Type :elf
			:id n
			:mtg-place study}))
	deer (for [n (range 1 9)]
	       (agent {:Type :deer
		       :id n
		       :mtg-place sleigh}))
	santa (agent {:active-place nil :study study :sleigh sleigh})]
    (map deref elves)
    (map (fn [a] (send-off a screw-off)) elves)
    [study sleigh elves deer santa]))
    ;(add-watch sleigh  )
    ;(add-watch study )
    ;()))

(defn display [& args]
  (let [output (apply str (interpose " " args))]
    (dosync
     (ensure output-lock)
     (println output))))


;;==============================================================
;;==============================================================

(comment
(defn queue-up [slave]
  (let [mtg-place @(:mtg-place slave)]
    (if (or (.contains (:in mtg-place) slave)
	    (:full mtg-place))
      nil
      (send (:mtg-place slave) #(conj (:queue %) slave)))))

(def study (ref {:capacity 3 :full nil :queue [] :in []}))

(def elf1 (agent {:Type :elf
		  :id 55
		  :mtg-place study}))

(defn enter [slave]
  ())
				       
      
(defn add-to-q [mtg-place]())

	    
(defn meet-the-master [slave]
  (let [mtg-place @(:meeting-place slave)]
    (if (or (.contains (:in mtg-place))
	    (:full mtg-place))
      nil
      (dosync
       (let [mtg-place @(:meeting-place slave)]
	 (alter meet-ref conj slave)
	 (if (= (:capacity slave) (count @q-ref))
	   (alter slave-ref assoc :full :true)))))))

(defn slave-monitor [slave task]
  (agent {:slave slave :task task}))

(defn beat-slave [{slave :slave task :task}]
  (send slave task))

(defn new-task [state task]
  (assoc state :task task))

(defn enter-mtg-place [slave]
  ())
  
(defn workflow [grp task]
  (do
   (let [[ingate outgate] (join-grp grp)]
     (do
       (pass-gate ingate)
       (task)
       (pass-gate outgate)))))

(defmacro check [pred tx]
  (when-not pred (throw (RuntimeException. "Failed check!"))))

(defn pass-gate [{rem :rem-capacity}]
  (dosync
   (while (zero? @rem)
     ; put sleep here
     (Thread/sleep 500)
     )
   (alter rem dec)))

(defn operate-gate [{rem :rem-capacity} :as gate]
 (do
   (dosync
    (ref-set rem (:capacity gate)))
   (dosync
    (while (pos? @rem)
      ;this is a blocking loop
     (Thread/sleep 500)
      ;may need to sleep or something here...
      ))))

(defn join-grp [{state :state} :as grp]
  (dosync
   (while (zero? (:openings @state))
      ;this is a blocking loop
     (Thread/sleep 500)
      ;may need to sleep or something here...
     )
   (alter state #(assoc % :openings (dec (:openings %))))
   (let [curstate @state]
     (vector (:ingate state)(:outgate state)))))

(defn await-grp [{state :state} :as grp]
  (do
   (while (pos? (:openings @state))
      ;this is a blocking loop
     (Thread/sleep 500)
      ;may need to sleep or something here...
     )
   (let [n (:capacity grp)
	 new-ingate (make-gate n)
	 new-outgate (make-gate n)]
     (ref-set state (make-group-state n new-ingate new-outgate))
     (vector new-ingate new-outgate))))
)
