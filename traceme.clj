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

(defn nqsym
  "symbol -> namespaced-qualified symbol"
  [sym]
  (symbol (.substring (.toString (resolve sym)) 2)))

(defn root-bound?
  "is the symbol root bound?"
  [sym]
  (.hasRoot sym))

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
    (alter traced-map assoc 
           key (struct-map traced-fn-struct :orig f :traced traced-fn))
    (intern (symbol (namespace key)) (symbol (name key)) traced-fn)))

(defn- disable-trace [key traced-fn]
  (println "untracing" key)
  (intern (symbol (namespace key)) (symbol (name key)) (:orig traced-fn))
  (alter traced-map dissoc key))

;;;; lookup function in traced map to determine what to do for
;;;; toggle/on/off action

(defn trace-aux [f key action]
  (when (not (fn? f))
    (throw (Exception. "not a function")))
  (dosync
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
   nil))

(defn- trace-ns-aux [ns action]
  (dosync
   (doseq [[k v] (ns-interns ns)]
     (when (and (root-bound? v) (fn? (deref v)))
       (trace-aux (deref v) (symbol (str ns) (str k)) action)))))

;;;; 
;;;; The user interace starts here
;;;; 

(defmacro toggle-trace
  "Toggles tracing of function f"
  [f]
  (let [key (gensym)]
    `(let [~key (nqsym (quote ~f))]
       (trace-aux ~f ~key :toggle))))

(defmacro trace-on
  "Turn trace on for function f"
  [f]
  (let [key (gensym)]
    `(let [~key (nqsym (quote ~f))]
       (trace-aux ~f ~key :on))))

(defmacro trace-off
  "Turn trace off for function f"
  [f]
  (let [key (gensym)]
    `(let [~key (nqsym (quote ~f))]
       (trace-aux ~f ~key :off))))

(defn trace-ns
  "trace everything in the namespace ns. Don't trace clojure.core!"
  [ns]
  (trace-ns-aux ns :on))

(defn untrace-ns
  "untrace everything in the namespace ns"
  [ns]
  (trace-ns-aux ns :off))

(defn untrace-all 
  "untrace everything and clean out traced-map"
  []
  (dosync
   (doseq [[key traced-fn] @traced-map]
     (when (= (:traced traced-fn) (deref (resolve key)))
       (disable-trace key traced-fn)))
   (ref-set traced-map {})
   nil))
