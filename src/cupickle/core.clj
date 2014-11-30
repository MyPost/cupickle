(ns cupickle.core
  "
  Defines functions for working with Cucumber from Clojure.

  cupickle.core/main is the standard entry point for this namespace.  
  "
  (:require [bultitude.core       :as b]
            [cemerick.pomegranate :as p]
            [cupickle.gherkin      :as g])
  (:use     [clojure.java.io]
            [clojure.pprint]))

; Printing is done with cuc-print so that lein-cupickle can overwrite the printer with the lein printer

(def ^:dynamic *debug* false)
(def ^:dynamic *quiet* true)
(def ^:dynamic cuc-print (fn [& stuff] (println (apply str stuff))))

(defmacro debug
  "
  Can be used to print debugging info, such as what feature is currently being processed.
  Does not print unless cupickle.core/*debug* is set to true.
  By default this prints nothing.
  "
  [& args]
  `(if *debug*
     (cuc-print ~@args)))

(defmacro info
  "
  Can be used to print information, such as testing progress.
  Does not print unless cupickle.core/*quiet* is set to false.
  By default this prints information.
  "
  [& args]
  `(if (or *debug* (not *quiet*))
     (cuc-print ~@args)))

; Result wrappers
(defn error   [& {:keys [] :as body}] [(merge {:type :error  } body)])
(defn success [& {:keys [] :as body}] [(merge {:type :success} body)])
(defn missing [& {:keys [] :as body}] [(merge {:type :missing} body)])

; Check for success
(defn children-pass [results] (empty? (filter #(= :error (:type %)) results)))

; Concatty for doall thing
(defmacro focat [bindings & body]
  `(doall (apply concat (for ~bindings (do ~@body)))))

; Feature, Scenario, Step, and Annotation functions
(declare run-step)

(defn dispatch
  "
  Used to intercept annotations to items in a gherkin syntax tree.
  These are represented in Gherkin by \"@something\" before an item.
  The annotation related items are simply run as regular steps.
  The items are:
  
  * before-annotation-@something  
  * @something  
  * after-annotation-@something  

  If there is no annotation, the regular item-function is dispatched to,
  thus the name.
  "
  [[text & body] fun steps]
  (debug "In dispatch: " text)
  (if (re-matches #"^@.*" text)
    (do (debug "In annotation: " text)
        [(run-step (str "before-annotation-" text) body steps)
         (run-step text body steps)
         (dispatch (first body) fun steps)
         (run-step (str "after-annotation-" text) body steps)])
    (fun text body steps)))

(defn missing-definition
  "
  Called when a step in a gherkin file is missing a corresponding clojure definition.

  This is not considered an error, however an info message is output regarding the
  missing definition.
  "
  [step]
  (info "Missing definition for step [" step "]")
  (missing :level :step :step step))

(defn run-matching
  "
  Runs a step against its corresponding definition and returns information about the result.
  If there is an error, then this execption unrolls back to the scenario in which the
  step is contained.
  "
  [step body function-details]
  (let [pattern (:pattern function-details)
        match   (re-matches pattern step)
        _       (debug "Found match for step [" step "] with re [" pattern "]")
        matchl  (if (string? match) [] (rest match))
        bodyl   (if body body [])
        args    (concat [step] matchl bodyl)
        res     (apply (:function function-details) args)
        ]
    (info ".")
    (success :level :step :result res :step step :body body :function function-details)))

(defn step-matches [step function-details]
  (re-matches (:pattern function-details) step))

(defn run-step [step body functions]
  (let [matching (filter (partial step-matches step) functions)
        num      (count matching)]
    (condp = num
      0        (missing-definition step)
      1        (run-matching step body (first matching))
      :default (throw (Throwable. (str "Too many matching functions for step [" step "] - [" matching "]"))))))

(defn failed-step [scenario step steps]
  (cuc-print "Scenario failed [" scenario "] step [" step "].")
  (error :level :step :scenario scenario :failing-step step))

(defn run-scenario [decl other steps]
  (debug "Scenario: " decl)

  (let [scenario-info [:level :scenario :scenario decl :total-steps (count other)]
        results       (focat [step other]
                             (try
                               (dispatch step run-step steps)
                               (catch Throwable e
                                 (failed-step decl step steps))))
        allpassed?    (empty? (filter #(= :error (:type %)) results))
        result        (if allpassed?
                        (concat (apply success scenario-info) results)
                        (concat (apply error   scenario-info) results))]

    (debug "")
    (debug "Scenario Info:")
    (debug "")
    (debug result)
    (debug "")

    result))

(defn run-feature [decl other steps]
  (debug "Feature: " decl)

  ; TODO: Why do I need to apply concat here? This should not be needed.
  (let [results (apply concat (focat [s other]
                                     (dispatch s run-scenario steps)))
        passed  (children-pass results)]

    (debug "passed? " passed)
    (if passed
      (concat (success :level :feature :feature decl) results)
      (concat (error   :level :feature :feature decl) results))))


; File and Directory-Structure handlers

(defn run-feature-file [feature-file steps]
  "
  Process each feature from a .gherkin file.

  Traditionally there is only one feature per file, however, cupickle allows more than one.
  "
  (try
    (let [text   (slurp feature-file)
          parsed (g/parse-gherkin text)]

      (focat [f parsed] (dispatch f run-feature steps)))

    (catch Exception e
      (cuc-print "Caught an exception while processing cucumber file" :file (.getPath feature-file))
      (error :level :feature-file :message "Caught an exception while processing cucumber file" :file (.getPath feature-file)))))

(defn namespace-info
  "
  Get the set of function-vars from a namespace that have the ^:cupickle-pattern metadata property.
  Returns each var in a map containing :namespace, :function-symbol, :function, and :pattern.
  "
  [n]
  (let [path (clojure.string/replace (str n) "." "/")
        
        _ (clojure.core/load path)

        info (doall
              (filter :pattern
                      (for [[k v] (ns-publics n)]
                        (do (debug "Collecting function: " n k v (meta v))
                            {:namespace       n
                             :function-symbol k
                             :function        v
                             :pattern         (-> v meta :cupickle-pattern)}))))]
    info))

(defn run-steps-and-features [namespaces features]
  (let [steps (focat [n namespaces] (namespace-info n))]
    
    (focat [f features]
           (run-feature-file f steps))))

(defn walk [dirpath pattern] ; Thanks Stack-Overflow!
  (doall (filter #(re-matches pattern (.getName %))
                 (file-seq (file dirpath)))))

(defn stats
  "
  Outputs some statistics about the testing results.

  Features:  (x/y) passed.
  Scenarios: (x/y) passed.
  Steps:     (x/y) passed.
  Errors:    n

  Testing was a [success!|failure...]
  "
  [results]
  
  (let [features         (filter #(= :feature  (:level %)) results)
        passed-features  (filter #(= :success  (:type  %)) features)
        scenarios        (filter #(= :scenario (:level %)) results)
        passed-scenarios (filter #(= :success  (:type  %)) scenarios)
        steps            (filter #(= :step     (:level %)) results)
        passed-steps     (filter #(= :success  (:type  %)) steps)
        missing-steps    (filter #(= :missing  (:type  %)) steps)
        step-count       (apply + (map :total-steps scenarios))
        errors           (filter #(= :error    (:type  %)) results)]

    (info "")
    (info "Cupickle Report")
    (info "===============")
    (info "")
    (info "  Features: " (count passed-features)  "/" (count features)  "passed.")
    (info "  Scenarios:" (count passed-scenarios) "/" (count scenarios) "passed.")
    (info "  Steps:    " (count passed-steps)     "/" step-count        "passed.")
    (info "  Errors:   " (count errors))
    (info "  Missing:  " (count missing-steps))
    (info "")
    (info "Testing" (if (empty? errors) "was a success!" "failed..."))
    (info "")))

(defn main
  "
  Run a set of features against a set of definitions.

  This returns true if the tests passed, false if not
  and has a side-effect of printing the test-run information.

  The default feature-path is \"features\"
  
  Options are provided as keyword-value pairs.

  For example:

  (main :feature-path \"my-special-cucumber-features\")
  ; => true

  Other flags:
  
  * quiet
  * debug
  "

  [ & {:keys [feature-path step-path] :as cupickle}]

  (binding [*debug* (:debug cupickle)
            *quiet* (:quiet cupickle)]

    (let [feature-path (or feature-path "features")
          step-path    (or step-path feature-path)
          namespaces   (b/namespaces-on-classpath :classpath step-path)
          features     (walk feature-path #".*\.feature")
          _            (p/add-classpath feature-path)
          result       (run-steps-and-features namespaces features)]

      (debug "")
      (debug "Final Result:")
      (debug "")
      (debug result)
      (stats result)
      result)))

(defn help []
  (cuc-print "Usage:")
  (cuc-print "  lein cupickle [-h] [--help] [help true] [quiet true] [debug true] [feature-path FEATURE_PATH] [step-path STEP_PATH]"))
