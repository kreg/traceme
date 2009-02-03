;;  Copyright (c) Craig McDaniel, Feb 2009. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;  which can be found in the file epl-v10.html at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.

;; another tracing library for Clojure
;; see bottom of file for user interface

;;;; utility functions

(ns traceme)

;;;; trace entry/exit functions

(defn trace-entry [id nm args]
  (println (str "TRACE " id ": " nm " called with " (pr-str args))))

(defn trace-exit [id nm returns]
  (println (str "TRACE " id ": " nm " returned " (pr-str returns))))

;;;; wrapper to call the tracing functions 

(def
 #^{:doc "Prevents tracing of functions we use for the trace itself"}
 prevent-trace false)

(defn trace-wrap
  [key f]
  (fn [& args]
    (if prevent-trace
      (apply f args)
      (binding [prevent-trace true]
        (let [id (gensym "t")]
          (trace-entry id key args)
          (binding [prevent-trace false]
            (let [v (apply f args)]
              (binding [prevent-trace true]
                (trace-exit id key v))
              v)))))))

;;;; map of trace functions
(defstruct traced-fn-struct :orig :traced)
(def traced-map (ref {}))

;;;; enable/disable trace for given function

(defn- enable-trace [key f]
  (let [traced-fn (trace-wrap key f)]
    (println "tracing" key)
    (dosync (alter traced-map assoc key 
                   (struct-map traced-fn-struct :orig f :traced traced-fn)))
    (.doReset key traced-fn)))

(defn- disable-trace [key traced-fn]
  (println "untracing" key)
  (.doReset key (:orig traced-fn))
  (dosync (alter traced-map dissoc key)))

;;;; lookup function in traced map to determine what to do for
;;;; toggle/on/off action

(defn trace-aux [f key action]
  (when (not (fn? f))
    (throw (Exception. "not a function")))
  (let [traced-fn (@traced-map key)]
    (if (and traced-fn (= f (:traced traced-fn)))

      ;; found
      (if (= action :on)
        (println key "is already being traced")
        (disable-trace key traced-fn))

      ;; not found
      (if (= action :off)
        (println key "was not being traced")
        (enable-trace key f))))
  nil)

;;;; 
;;;; The user interace starts here
;;;; 

(defmacro toggle-trace
  "Toggles tracing of function f"
  [f]
  `(trace-aux ~f (var ~f) :toggle))

(defmacro trace-on
  "Turn trace on for function f"
  [f]
  `(trace-aux ~f (var ~f) :on))

(defmacro trace-off
  "Turn trace off for function f"
  [f]
  `(trace-aux ~f (var ~f) :off))

(defn untrace-all 
  "untrace everything and clean out traced-map"
  []
  (doseq [[key traced-fn] @traced-map]
    (when (= (:traced traced-fn) (deref key))
      (disable-trace key traced-fn)))
  (dosync (ref-set traced-map {}))
  nil)
