(ns cljs.tagged-literals
  (:require
   #?(:clj [clojure.instant :as inst]
      :cljs [cljs.core])
   [clojure.string :as string]))

(defn read-queue
  [form]
  (when-not (vector? form)
    (throw #?(:clj (RuntimeException. "Queue literal expects a vector for its elements.")
              :cljs (ex-info "Queue literal expects a vector for its elements."
                             {:type :runtime-exception}))))
  (list 'cljs.core/into 'cljs.core.PersistentQueue.EMPTY form))

#?(:cljs
   (defn parse-uuid [name]
     (let [components (string/split name #"-")]
       (when (not= (count components) 5)
         (throw (ex-info (str "Invalid UUID string: " name)
                         {:type :illegal-argument})))
       (cljs.core/uuid name))))

(defn read-uuid
  [form]
  (when-not (string? form)
    (throw #?(:clj (RuntimeException. "UUID literal expects a string as its representation.")
              :cljs (ex-info "UUID literal expects a string as its representation."
                             {:type :runtime-exception}))))
  (try
    (#?(:clj java.util.UUID/fromString :cljs parse-uuid) form)
    (catch Throwable e
      (throw (RuntimeException. (.getMessage e))))))

(defn read-inst
  [form]
  (when-not (string? form)
    (throw #?(:clj (RuntimeException. "Instance literal expects a string for its timestamp.")
              :cljs (ex-info "Instance literal expects a string for its timestamp."
                             {:type :runtime-exception}))))
  (try
    (#?(:clj inst/read-instant-date :cljs js/Date.) form)
    (catch #?(:clj Throwable :cljs js/Error) e
      (throw #?(:clj (RuntimeException. (.getMessage e))
                :cljs (ex-info (.-message e) {:type :runtime-exception}))))))

(defn valid-js-literal-key? [k]
  (or (string? k)
      (and (keyword? k)
           (nil? (namespace k)))))

(deftype JSValue [val])

(defn read-js
  [form]
  (when-not (or (vector? form) (map? form))
    (throw (RuntimeException. "JavaScript literal must use map or vector notation")))
  (when-not (or (not (map? form))
                (every? valid-js-literal-key? (keys form)))
    (throw (RuntimeException. "JavaScript literal keys must be strings or unqualified keywords")))
  (JSValue. form))

(def ^:dynamic *cljs-data-readers*
  {'queue read-queue
   'uuid  read-uuid
   'inst  read-inst
   'js    read-js})
