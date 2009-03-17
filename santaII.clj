;;;; Implementation of the solution to the Santa Claus problem
;;;; from the beautiful_concurrency.pdf by Simon Peyton-Jones
;;;;
;;;; For this second attempt, I'm going to implement it as best
;;;; I can in the Clojure idioms (given my current, limited knowledge
;;;; of such).  I'm sure I'll be corrected.... ;)

(ns 'santa)

(def *sleep-msecs* 60000)

(defn start [slave]
  (fn []
    (do
      (sleep-rnd *sleep-secs*)
      (send slave get-in-line)
      (await slave))))

(defn valid-mtg-state? [{cap ::capacity full ::full q ::queue in ::in}]
  (and (< (count q) cap)
       (< (count in) cap)))

(defn whine [agent]
  (try (sleep-rnd )
       (catch InterruptedException e
	 (print-str "whip sleep interupted"))
       (finally
	(send agent next))))

(def screw-off-time 100000) ;in milliseconds
(defmulti screw-off ::Slave)
(defmethod screw-off ::Elf [e]
  (do
    (ignore [InterruptedException] (sleep-rnd screw-off-time))
    ()))



(defn main [elf-cap deer-cap]
  (let [study (agent {::capacity elf-cap ::full nil ::queue #{} ::in #{} } :validator valid-mtg-state?)
	sleigh (agent {::capacity elf-cap ::full nil ::queue #{} ::in #{} })
	elves (for [n (range 1 10)] 
		(agent {::Type ::elf ::id n
		;	::job (cycle [queue-up 
			::mtg-place study
			::task #(print-str "Elf" n "getting his ass handed to him by the boss!")}))
	deer (for [n (range 1 9)] 
	       (agent {::Type ::deer ::id n
		       ::mtg-place sleigh
		       ::task #(print-str "Deer" n "delivering toys to all the bad little girls & boys!")}))
	santa (agent {::active-place nil ::study study ::sleigh sleigh})]
    (add-watch sleigh )
    (add-watch study )
    ()))

(defn queue-up [slave]
  (let [mtg-place @(::mtg-place slave)]
    (if (or (.contains (::in mtg-place) slave)
	    (::full mtg-place))n
      nil
      (send (::mtg-place slave) #(conj (::queue %) slave)))))

(def study (agent {::capacity 8 ::full nil ::queue #{} ::in #{} }))

(def elf1 (agent {::id 5
			::mtg-place study
			::task #(print-str "Elf" 5 "getting his ass handed to him by the boss!")}))

(defn enter [slave]
  (
				       
      
(defn add-to-q [mtg-place]

	    
(defn meet-the-master [slave]
  (let [mtg-place @(::meeting-place slave)]
    (if (or (.contains (::in mtg-place))
	    (::full mtg-place))
      nil
      (dosync
       (let [mtg-place @(::meeting-place slave)]
	 (alter meet-ref conj slave)
	 (if (= (::capacity slave) (count @q-ref))
	   (alter slave-ref assoc ::full :true)))))))

(defn slave-monitor [slave task]
  (agent {::slave slave ::task task}))

(defn beat-slave [{slave ::slave task ::task}]
  (send slave task))

(defn new-task [state task]
  (assoc state ::task task))

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



(defn guarded-txn [ref pred? tx]
  (let [thread (Thread/currentThread)
	tx-fn #(dosync %)]
    (if (pred? @ref)
      tx-fn
      (do
	(add-watch ref thread #(
			       ))))))

(def x (ref 1))

(defn inc-ref [r]
  (dosync
   (alter r inc)))

(def inc-x (partial inc-ref x))

(defn dtx []
  (do
    (prn "we're here...")
    (inc-x)))


(defn guarded-txn [ref pred? tx]
  (let [thread (Thread/currentThread)
	obj (Object.)]
    (do
      (prn "get ready...")
      (while (not (pred? @ref))
	(do
	  (add-watch ref thread #(if (pred? %4)
				   (dosync
				    (prn "in the tx...")
				    (if (pred? @ref)
				      (do
					(tx)
					(.notify thread))))))
	  (try (sleep 10000)
	       (catch InterruptedException e ())
	       (finally (remove-watch ref thread)))))
      (prn "we're done..."))))

(def g-inc-x (fn []
	       (guarded-txn x #(zero? (rem % 7)) dtx)))

(defn tx [n f]
	(fn [] (dosync
	 (prn "tx" n "starting")
	 (Thread/sleep (* n 2000))
	 (f)
	 (prn "tx" n "ending"))))
