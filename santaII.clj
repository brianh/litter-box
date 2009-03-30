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

(declare screw-off)

(def screw-off-time 20000) ;in milliseconds

(defstruct task :name :delay :start-txt :fun :args)

(defn whip-slave [{:keys [type id task] :as state}]
  (let [{:keys [start-txt fun args]} task]
    ;(if start-txt
     ; (display type id start-txt))
    (display "Crackin' the whip on ol'" type id)
    (if args
      (do
	;(display fun "..." args)
	(apply fun state args))
      (do
	;(display fun "... no args")
	(fun state)))))

(defn set-task [task]
  (fn [state]
    (assoc state :task task)))

(def wait-patiently (struct task
			    :wait-patiently
			    nil
			    "wasting time in line..."
			    identity
			    nil))
			    
(defn get-in-line [state]
  (handle [Exception e (do
			 (display (:type state) (:id state)
				  "tried it's best but no more room.  Better luck next time.")
			 (assoc state :task screw-off))]
	  (let [mtg-place (:mtg-place state)]
	    (dosync
	     ;(ensure mtg-place) not sure if this is needed...
	     (let [place @mtg-place]
	       (if (:full place)
		 (throw (Exception. "No room"))
		 (let [q (:queue place)
		       newq (conj q state)
		       cap (:capacity place)]
		   (alter mtg-place assoc :queue newq :full (= (count newq) cap))
		   (assoc state :task wait-patiently))))))))

(def queue-up (struct task
		      :queue-up 
		      5000
		      "figures it's time to go see the old bastard..."
		      get-in-line
		      nil))

(def screw-off (struct task
		       :screw-off
		       screw-off-time
		       "pretending to do something..."
		       assoc
		       [:task queue-up]))
(defn refkey= [k v]
  (fn [aref] (= v (k @aref))))

(def entrez-vous-biatches (struct task
				  :entrez-vous-biatches 
				  5000
				  "'Bout time the fat man got off his ass!"
				  (fn [{place-ref :mtg-place id :id :as state}]
				    (let [place @place-ref
					  {q :queue in :in} place
					  id= (refkey= :id id)];(fn [aref] (= id (:id @aref)))]
				      (if-let [a-ref (first (filter id= q))]
					(dosync (alter place-ref assoc :queue (filter (complement id=) q))
						(alter place-ref assoc :in (conj in a-ref))))))))
					  
(def choose (struct task
		    :choose 
		    4000
		    "Damnit!  Why can't they just leave me alone!"
		    (fn [{active :active-place :as state}]
		      (if active
			state
			(let [elves-ready (:full (:study state))
			      deer-ready (:full (:sleigh state))
			      new-active (if deer-ready (:study state) (:sleigh state))]
			  (assoc state :active-place new-active)
			  (map (fn [a] (send a (set-task entrez-vous-biatches)))
			       (:queue new-active)))))))

(defn exit-place [{place-ref :mtg-place id :id :as state}]
  (dosync
   (let [in (:in @place-ref)]
     (if-let [aref (first (filter (refkey= :id id) in))]
       (do
	 (alter place-ref assoc :in (remove (refkey= :id id) in))
	 (assoc state :task screw-off))
       state))))

(def kick-em-out (struct task
			 :kick-em-out
			 20000
			 "Yeah, yeah.  Don't get your panties in a wad.... I'm on it."
			 exit-place))

(def work-em (struct task
		    :work-em 
		    2000
		    "Time to make the lazy bastards earn their keep!"
		    (fn [{active :active-place :as state}]
		      (dosync (alter active assoc :full nil)
			      (map (fn [a] (send a (set-task kick-em-out))))))))

(defn new-task-trigger [e]
  (add-watch e :new-task-watch (fn [k r {{oldname :name} :task}
				    {{newname :name delay :delay} :task type :type id :id txt :start-txt}]
				 (when-not (= oldname newname)
				   (future
				     (display type id txt)
				     (if delay
				       (ignore [InterruptedException] (sleep-rnd delay)))
				     (send r whip-slave))))))

(defn map-difference2 [m1 m2]
  (reduce (fn [m entry] (let [k (key entry)
			      v1 (val entry)
			      v2 (k m2)]
			  (if (= v1 v2)
			    (dissoc m k)
			    m)))
	  m1 m1))

(defn map-difference [m1 m2]
  (let [ks (keys m1)]
    (reduce dissoc m1 (filter identity (map (fn [k] (if (= (k m1) (k m2))
						      k
						      nil))
					    ks)))))

;(reduce #(apply assoc %1 %2) {} (filter (fn [e] (> (val e) 60)) mb))

(defn place-watcher [e asanta]
  (add-watch e :place-watch (fn [k r old {:keys [full capacity queue in] :as new}]
			      (let [cmap (map-difference new old)
				    incnt (count in)
				    qcnt (count queue)]
				(if (:full cmap)
				  (cond
				    (zero? incnt) (send asanta (set-task choose))
				    (zero? qcnt) (send asanta (set-task work-em)))
				  (if (zero? qcnt) (send asanta (set-task work-em))))))))
				
(defn place-watcher-old [e asanta]
  (add-watch e :place-watch (fn [k r old {:keys [full capacity queue in] :as new}]
			      (let [qcnt (count queue)
				    icnt (count in)
				    oldicnt (count (:in old))]
				(if full
				  (cond
				    (= capacity qcnt) (send asanta (set-task choose))
				    (= 0 qcnt) (send asanta (set-task work-em))
				    :true (display "Emptying the" "queue"))
				  ;(cond
				    ;((> qcnt 0) (display "one more entered the queue")))))))
				    (display "one more entered the queue"))))))

(defn main [elf-cap deer-cap]
  (let [study (ref {:capacity elf-cap :full nil :queue [] :in []})
	sleigh (ref {:capacity deer-cap :full nil :queue [] :in []})
        elves (for [n (range 1 5)] 
		(agent {:type "Elf" :id n :task screw-off :mtg-place study}))
;	deer (for [n (range 1 9)]
;	       (agent {:type "Deer" :id n :task screw-off :mtg-place sleigh}))
	santa (agent {:type "Master" :active-place nil :task screw-off :study study :sleigh sleigh})]
    (place-watcher study santa)
;    (place-watcher sleigh santa)
    (map new-task-trigger elves)
    (new-task-trigger santa)
    
    ;(map deref elves)
    ;(map (fn [a] (send-off a screw-off)) elves)
    [study sleigh elves santa]))
;    [study sleigh elves deer santa]))
    ;(add-watch sleigh  )
    ;(add-watch study )
    ;()))


;;==============================================================
;;==============================================================

(comment
(def study (ref {:capacity 3 :full nil :queue [] :in []}))
(def study-old (ref {:capacity 3 :full :true :queue [] :in []}))

(remove-watch study :place-watch)
				  
(def elf (agent {:type "Elf" :id 55 :task screw-off :mtg-place study}))
(def elftwo (agent {:type "Elf" :id 77 :task screw-off :mtg-place study}))
(def elftwo (agent {:type "Elf" :id 77 :task wait-patiently :mtg-place study}))
(def ta {:task {:name :ta :v "nthei"}})
(def tb {:task {:name :tb :v "aoei"}})

(send elf whip-slave)

(set! *print-level* 4)

(dosync (alter study assoc :queue [] :full nil))

)

