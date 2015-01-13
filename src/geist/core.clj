(ns geist.core
  (:use clojure.java.io
        clojure.pprint)
  (:require [clojure.pprint :as pp])
  (:import (javaff JavaFF)
           (java.io File StringWriter)))

(defn build-def []
  '(domain Rover))

(defn desc->domain [desc]
  (list 'define
        (build-def)
        '(:requirements :adl)
        '(:types rover - object
                 waypoint - object
                 spot - waypoint)
        '(:predicates (at ?x - rover
                          ?y - waypoint))
        '(:action navigate
                  :parameters (?x - rover
                                  ?y - waypoint
                                  ?z - waypoint)
                  :precondition (at ?x ?y)
                  :effect (at ?x ?z))))

(alter-var-root #'pp/*print-suppress-namespaces* (constantly true))

(defn plan [bs is]
  (let [domain (desc->domain {})
        domain-file "/tmp/test.pddl"
        problem-file "/tmp/pfile01"]
    (spit domain-file domain)
    (spit problem-file (let [s (StringWriter.)]
                         (binding [*out* s]
                          (pp/pprint `(define (problem rover1)
                                        (:domain Rover)
                                        (:objects r - Rover
                                                  w0 w1 - Spot)
                                        (:init ~@bs)
                                        (:goal (and ~is))))
                          s)))
    (into [] (.getActions (.plan (JavaFF. (File. domain-file) nil) (File. problem-file))))))

#_(marshal)
#_(.plan (JavaFF. (File. "/tmp/test.pddl") nil) (File. "/tmp/pfile01"))
#_(.plan (JavaFF. (File. "/Users/sergio/Documents/Research/JavaFF/resources/problems/rovers/domain.pddl") nil)
   (File. "/Users/sergio/Documents/Research/JavaFF/resources/problems/rovers/pfile08"))
