(ns reagent-native.util)

(defn spit->camel
  "Takes a spit-case-string and makes it camel-case."
  [s]
  (let [[first & rest] (clojure.string/split s "-")]
    (apply str
           first
           (map clojure.string/capitalize rest))))
