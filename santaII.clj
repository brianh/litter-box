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

(def output-lock (ref 0))
(def screw-off-time 20000) ;in milliseconds

(defn display [& args]
  "Ensures write order"
  (let [output (apply str (interpose " " args))]
    (dosync
     (ensure output-lock)
     (println output))))

(defstruct task :delay :start-txt :fun :args)
;(defstruct task :delay :start-txt :transitions); :fn :args)
;(defstruct transition :delay :start-txt :xtions); :fn :args)

(defn whip-slave [{:keys [type id task] :as state}]
  (let [{:keys [start-txt fun args]} task]
    (if start-txt
      (display type id start-txt))
    (if args
      (do
	;(display fun "..." args)
	(apply fun state args))
      (do
	;(display fun "... no args")
	(fun state)))))

(def wait-patiently (struct task
		      nil
		      "wasting time in line..."
		      identity
		      nil))
			    
(defn get-in-line [state]
  (handle [Exception e (do
			 (display (:type state) (:id state)
				  "tried it's best.  Better luck next time.")
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
		   (alter mtg-place assoc :queue newq :full (< (count newq) cap))
		   (assoc state :task wait-patiently))))))))

(def queue-up (struct task
		      nil
		      "figures it's time to go see the fat old bastard..."
		      get-in-line
		      nil))

(def screw-off (struct task
		       screw-off-time
		       "pretending to do something..."
		       assoc
		       [:task queue-up]))

(defn main [elf-cap deer-cap]
  (let [study (ref {:capacity elf-cap :full nil :queue [] :in []})
	sleigh (ref {:capacity elf-cap :full nil :queue [] :in []})
        elves (for [n (range 1 10)] 
		(agent {:type "Elf"
			:id n
			:task screw-off
			:mtg-place study}))
	deer (for [n (range 1 9)]
	       (agent {:type "Deer"
		       :id n
		       :task screw-off
		       :mtg-place sleigh}))
	santa (agent {:type "Master"
		      :active-place nil
		      :task screw-off
		      :study study
		      :sleigh sleigh})]
    ;(map deref elves)
    ;(map (fn [a] (send-off a screw-off)) elves)
    [study sleigh elves deer santa]))
    ;(add-watch sleigh  )
    ;(add-watch study )
    ;()))


;;==============================================================
;;==============================================================

(comment
(def study (ref {:capacity 3 :full nil :queue [] :in []}))

(add-watch study :study-watch (fn [k r old new]
				(let [{:keys [full capacity queue]} new
				      qcnt (count queue)]
				  (if full
				    (cond
				      (= capacity qcnt) (display "Newly full" "study queue")
				      (= 0 qcnt) (display "Empty" "study queue")
				      :true (display "Emptying the" "study queue"))))))

(remove-watch study :study-watch)
				  
(def elf (agent {:type "Elf"
		  :id 55
		  :task screw-off
		  :mtg-place study}))

(send elf whip-slave)
(dosync (alter study assoc :queue [] :full nil))

(defn slave-monitor [slave task]
  (agent {:slave slave :task task}))

)

