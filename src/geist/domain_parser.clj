(ns geist.domain-parser
  (:require [clojure.walk :as w]
            [serializable.fn :as s]))

(defmulti list->map (fn [k & vs] k))

(defmethod list->map :predicates [k & vs]
  nil)

(defn formula->fn [f]
  )

(defmethod list->map :action [k & vs]
  {(first vs) (apply merge (map #(hash-map (first %)
                                           (formula->fn (second %)))
                                (partition 2 (rest vs))))})

(defmethod list->map :types [k & vs]
  (let [pairs (map vector (drop-last 1 vs) (drop 1 vs))
        sets (partition-by
               #(= (ns-resolve *ns* (first %)) (ns-resolve *ns* `-))
               pairs)
        correspondents (partition 2 sets)
        mappings (conj
                   (map #(vector (map second (drop-last 1 (first %)))
                                 (second (first (second %))))
                        (drop 1 correspondents))
                   (vector (map first (first (first correspondents)))
                           (second (first (second (first correspondents))))))
        m (apply merge (map #(hash-map (second %) (into [] (first %))) mappings))]
    {:types m}))

(defmethod list->map :default [k & vs]
  (apply hash-map k vs))

(defmacro define [& args]
  (apply merge-with merge (map #(apply list->map %) args)))

(defn file->symbols [file-name]
  (into #{}
        (remove #(= % "")
                (clojure.string/split
                  (slurp
                    (clojure.java.io/resource
                      file-name))
                  #"\(|\)|(\s+)"))))

(defn st->symbol [st-symbol]
  {st-symbol (condp = (first st-symbol)
               \: (keyword (apply str (drop 1 st-symbol)))
               \" (keyword st-symbol)
               (ns-resolve *ns* (symbol st-symbol)))})

#_(let [st-symbols (file->symbols "problems/depots/domain.pddl")
        symbols (apply merge (map st->symbol st-symbols))
        free-strings (keys (get (group-by val symbols) nil))
        free-symbols (map symbol free-strings)]
    (doall (map #(eval `(def ~% ~(keyword %))) free-symbols)))

#_(eval
    (read-string
      (slurp
        (clojure.java.io/resource
          "problems/depots/domain.pddl"))))

