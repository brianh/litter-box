;;;; Implementation of the solution to the Santa Claus problem
;;;; from the beautiful_concurrency.pdf by Simon Peyton-Jones
;;;;
;;;; For this second attempt, I'm going to implement it as best
;;;; I can in the Clojure idioms (given my current, limited knowledge
;;;; of such).  I'm sure I'll be corrected.... ;)

(ns litterbox.santaII
  (:use litterbox.utils))

(comment
  (load-file "/home/brian/code/clj/s4/src/main/clojure/litterbox/santaII.clj")

)

(declare screw-off wait-patiently entrez-vous-biatches kick-em-out)

(def screw-off-time 20000) ;in milliseconds

(defstruct task :name :delay :start-txt :fun :args)

(defn whip-slave [{:keys [type id task] :as state}]
  (let [{:keys [start-txt fun args]} task]
    (if start-txt
      (display type id start-txt))
	;(display "Crackin' the whip on ol'" type id)
    (if args
      (do
	;(display fun "..." args)
	(apply fun state args))
      (do
	;(display fun "... no args")
	(fun state)))))

(defn refkey= [k v]
  (fn [aref] (= v (k @aref))))

(defn set-task [task]
  (fn [state]
    (assoc state :task task)))

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
	     (prn ".... setting to wait-patiently" "....")
	     (assoc state :task wait-patiently))))))))

(defn enter-place [{place-ref :mtg-place id :id :as state}]
  (let [place @place-ref
	{q :queue in :in} place
	id= (refkey= :id id)]
    (if-let [a-ref (first (filter id= q))]
      (dosync (alter place-ref assoc :queue (filter (complement id=) q))
	      (alter place-ref assoc :in (conj in a-ref))))))

(defn pick-group [{active :active-place :as state}]
  (if active
    (do
      (display "There's already an active state. Do nothing." (:name state))
      state)
    (do
      (display "Time to try picking a group.")
      (let [elves-ready (:full (:study state))
	    deer-ready (:full (:sleigh state))
	    new-active (if deer-ready (:sleigh state) (:study state))]
	(display "One potatoe, two potatoe, three...." elves-ready deer-ready (:name new-active))
	(map (fn [a] (send a (set-task entrez-vous-biatches)))
	     (:queue new-active))
	(assoc state :active-place new-active)))))

(defn exit-place [{place-ref :mtg-place id :id :as state}]
  (dosync
   (let [in (:in @place-ref)]
     (if-let [aref (first (filter (refkey= :id id) in))]
       (do
	 (alter place-ref assoc :in (remove (refkey= :id id) in))
	 (assoc state :task screw-off))
       state))))

(defn get-rid-of-em [{active :active-place :as state}]
  (dosync (alter active assoc :full nil)
	  (map (fn [a] (send a (set-task kick-em-out))))))

(def wait-patiently (struct task :wait-patiently nil "wasting time in line..." identity nil))
(def queue-up (struct task :queue-up 5000 "figures it's time to go see the old bastard..."
		      get-in-line nil))
(def zombie (struct task :zombie nil "Brains..... brains.... we want brains....." identity nil))
(def screw-off (struct task :screw-off screw-off-time "pretending to do something..."
		       (set-task queue-up) nil))
(def entrez-vous-biatches (struct task :entrez-vous-biatches
				  5000 "'Bout time the fat man got off his ass!" enter-place))
(def choose (struct task :choose 4000 "Damnit!  Why can't they just leave me alone!" pick-group))
(def kick-em-out (struct task :kick-em-out 20000
			 "Yeah, yeah.  Don't get your panties in a wad.... I'm on it." exit-place))
(def work-em (struct task :work-em 2000 "Time to make the lazy bastards earn their keep!"
		     get-rid-of-em))

(defn new-task-trigger [e]
  (add-watch e :new-task-watch (fn [k r {{oldname :name} :task}
				    {{newname :name delay :delay} :task
				     type :type id :id txt :start-txt}]
				 (when-not (= oldname newname)
				   (future
				     ;(display "FUTURE:  New task trigger firing...")
				     ;(display "\t" type id txt)
				     (if delay
				       (ignore [InterruptedException] (sleep-rnd delay)))
				     (send r whip-slave))))))

(defn place-watcher [e asanta]
  (add-watch e :place-watch (fn [k r old {:keys [full capacity queue in] :as new}]
			      (let [cmap (map-difference new old)
				    incnt (count in)
				    qcnt (count queue)]
				(display "Place watcher triggered for" (:name new) (:full cmap))
				(if (:full cmap)
				  (cond
				    (zero? incnt) (do
						    (display "Santa's gotta choose")
						    (send asanta (set-task choose)))
				    (zero? qcnt) (do
						   (display "They're all in...")
						   (send asanta (set-task work-em))))
				  (if (zero? qcnt) (do
						     (display "They're all in...")
						     (send asanta (set-task work-em)))))))))

(defn main [elf-cap deer-cap]
  (let [study (ref {:name :study :capacity elf-cap :full nil :queue [] :in []})
	sleigh (ref {:name :sleigh :capacity deer-cap :full nil :queue [] :in []})
        elves (for [n (range 1 5)] 
		(agent {:type "Elf" :id n :task zombie :mtg-place study}))
;	deer (for [n (range 1 9)]
;	       (agent {:type "Deer" :id n :task zombie :mtg-place sleigh}))
	santa (agent {:type "Master" :active-place nil :task zombie :study study :sleigh sleigh})]
    (place-watcher study santa)
;    (place-watcher sleigh santa)
    (map new-task-trigger elves)
    (new-task-trigger santa)

    (map #(send % (set-task screw-off)) elves)
    (send santa (set-task screw-off))

    [study sleigh elves santa]))


;;==============================================================
;;==============================================================

(comment
(def study (ref {:name :study :capacity 2 :full nil :queue [] :in []}))

(def elf (agent {:type "Elf" :id 55 :task zombie :mtg-place study}))

(def elf1 (agent {:type "Elf" :id 77 :task zombie :mtg-place study}))

(def elf2 (agent {:type "Elf" :id 11 :task zombie :mtg-place study}))

(def sleigh (ref {:name :sleigh :capacity 9 :full nil :queue [] :in []}))

(def santa (agent {:type "Master" :active-place nil :task zombie :study study :sleigh sleigh}))

(do
  (place-watcher study santa)
  (new-task-trigger santa)
  (new-task-trigger elf)
  (new-task-trigger elf1)
  (new-task-trigger elf2)
  )


(send elf (set-task screw-off))

(remove-watch study :place-watch)

(def study-old (ref {:capacity 3 :full :true :queue [] :in []}))
(def elftwo (agent {:type "Elf" :id 77 :task wait-patiently :mtg-place study}))
(def ta {:task {:name :ta :v "nthei"}})
(def tb {:task {:name :tb :v "aoei"}})

(send elf whip-slave)

(set! *print-level* 4)

(dosync (alter study assoc :queue [] :full nil))

)

