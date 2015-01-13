(ns geist.bdi-agent
  (:require [clojure.core.async :refer [onto-chan chan go go-loop >! <!]]
            [serializable.fn :as sfn]
            [geist.core :as geist])
  (:import (clojure.lang PersistentVector Keyword)
           (javaff.planning HelpfulAction)
           (javaff.data.strips PDDLObject)))

(defrecord Fact [predicate arguments]
  Object
  (toString [this]
    (str "(" (name (:predicate this)) " "
         (clojure.string/join " " (:arguments this)) ")")))
(defrecord AgentState [aid beliefs desires intentions plan])

(defmethod clojure.core/print-method Fact [x writer]
  (.write writer (str x)))

(defn fact [predicate & args]
  (Fact. predicate (into [] args)))

(def percepts [[:at :r :w0]])
(def ch-percepts (chan))
(def ch-actions (chan))

(defn create-agent [aid beliefs intentions]
  (AgentState. aid beliefs [] intentions []))

(defmulti fact->pddl class)

(defmethod fact->pddl Keyword [k]
  (read-string (name k)))

(defmethod fact->pddl PersistentVector [v]
  (map fact->pddl v))

(defmethod fact->pddl :default [x]
  x)

(defmulti pddl->fact class)

(defmethod pddl->fact PDDLObject [po]
  (keyword (.getName po)))

(defmethod pddl->fact HelpfulAction [a]
  (vector (keyword (str (.getName a)))
          (map pddl->fact (.getParameters a))))

(defn brf [beliefs percept]
  (conj beliefs percept))

(defn reconsider? [intentions beliefs]
  true)

(defn succeded? [intentions beliefs]
  false)

(defn impossible? [intentions beliefs]
  false)

(defn fulfilled? [plan intentions beliefs]
  (or (empty? plan)
      (succeded? intentions beliefs)
      (impossible? intentions beliefs)))

(defn sound? [plan intentions beliefs]
  true)

(defn options [beliefs intentions]
  intentions)

(defn filter-intentions [beliefs desires intentions]
  intentions)

(defn plan [beliefs intentions actions]
  (map pddl->fact (geist/plan (map fact->pddl beliefs) (map fact->pddl intentions))))

(defn execute! [action]
  (go (>! ch-actions action)))

(defn reason [actions state percept]
  (let [beliefs (brf (:beliefs state) percept)
        is-fulfilled? (fulfilled? (:plan state) (:intentions state) beliefs)
        [desires intentions]
        (if (or is-fulfilled? (reconsider? (:intentions state) beliefs))
          (let [new-desires (options beliefs (:intentions state))]
            [new-desires (filter-intentions beliefs new-desires
                                            (:intentions state))])
          [(:desires state) (:intentions state)])
        plan (if (or is-fulfilled? (not (sound? (:plan state) intentions beliefs)))
               (plan beliefs intentions actions) ;; PDDL 3.0
               (:plan state))]
    (if (not (empty? plan))
      (execute! (first plan))) ;; HTN planning + asynchronous execution
    (merge state {:beliefs beliefs
                  :desires desires
                  :intentions intentions
                  :plan (into [] (rest plan))})))

(go
  (onto-chan ch-percepts percepts false))

(go-loop [percept (<! ch-percepts)
          state (create-agent "agent-0" [] [:at :r :w1])]
  (let [new-state (reason [] state percept)]
    (println new-state)
    (recur (<! ch-percepts) new-state)))

(go-loop [action (<! ch-actions)]
  (println "Executing " action)
  (recur (<! ch-actions)))
