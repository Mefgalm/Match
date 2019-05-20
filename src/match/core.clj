(ns match.core
  (:require [clojure.string :as str]))

(defn raw-param-to-typed [line]
  (let [[name value] (str/split line #"=")]
    {:name name :value value}))

(defn param-to-keyword [[f & tail]]
  (when (and (= \? f) (seq? tail))
    (keyword (apply str tail))))

(defn chain
  ([x f] (f x))
  ([x f & fs]
   (->> (conj fs f)
        (map #(% x))
        (filter some?)
        (first))))

(defn host-matcher [[command pattern]]
  (when (= command "host")
    {:command :host :pattern pattern}))

(defn path-matcher [[command pattern]]
  (when (= command "path")
    (println "path-matcher" pattern)
    {:command :path :pattern (str/split pattern #"/")}))

(defn param-matcher [[command pattern]]
  (when (= command "queryparam")
    (let [[param-name keyword] (str/split pattern #"=")]
      {:command :queryparam :pattern {:param-name param-name :keyword (param-to-keyword keyword)}})))

(defn url-chain [x]
  (chain x
         host-matcher
         path-matcher
         param-matcher))

(defn command-pattern [str]
  (rest (re-matches #"(\w+)\(([A-Za-z0-9\/\?=\.]+)\)" str)))

(defn new-pattern [line]
  (let [components (->> (str/split line #";")
                        (map (comp command-pattern str/trim))
                        (map url-chain))]
    (when (every? some? components)
      components)))

(defn split-raw-params [line]
  (str/split line #"&"))

(defn split-raw-path [line]
  (str/split line #"/"))

(defn pattern-to-command [str-pattern]
  (let [[_ command pattern] (re-matches #"(\w+)\(([A-Za-z0-9\/\?=\.]+)\)" str-pattern)]
    {:command (keyword command) :pattern pattern}))

(defn scramble-url [url]
  (let [url-pattern #"(?:http[s]?:\/\/)?(?:www\.)?([A-Za-z0-9\-\.]+\.\w+)(?:\/?)((?:[A-Za-z\-0-9\.]+(?:\/)?)*)\??((?:(?:\w+)=(?:\w+)&?)*)"
        [_ host path params] (re-matches url-pattern url)
        query-map (map raw-param-to-typed (split-raw-params params))]
    {:host host :path path :queryparams query-map}))

(defn build-path-component
  [pattern-comp url-comp]
  (if (= (first pattern-comp) \?)
    {:type :path-param :result [(param-to-keyword pattern-comp) url-comp]}
    {:type :path-match :result (= pattern-comp url-comp)}))

(defn get-path
  [pattern path]
  (let [paths-compnents (split-raw-path path)
        paths (map build-path-component pattern paths-compnents)]
    (when (and (= (count paths-compnents) (count pattern))
               (every? true? (map :result (filter #(= (:type %) :path-match) paths))))
      (map :result (filter #(= (:type %) :path-param) paths)))))

(defn get-queryparams
  [{:keys [param-name keyword]} url-queryparams]
  (let [{:keys [name value] :as url-param} (->> url-queryparams
                                                (filter #(= param-name (:name %)))
                                                (first))]
    (when url-param
      [keyword value])))

(defn ok-result [value]
  {:ok value})

(defn fail-result [value]
  {:error value})

(defn apply-command
  [{:keys [host path queryparams]} {ok :ok :as result} {:keys [command pattern]}]
  (if ok
    (cond
      (= command :host)
      (if (= host pattern)
        (ok-result ok)
        (fail-result "host mismatch"))

      (= command :path)
      (let [get-path-result (get-path pattern path)]
        (if get-path-result
          (ok-result (reduce conj ok get-path-result))
          (fail-result "path mismatch")))

      (= command :queryparam)
      (let [query-param (get-queryparams pattern queryparams)]
        (if query-param
          (ok-result (conj ok query-param))
          (fail-result (str (:param-name pattern) " queryparam missing"))))

      :else (fail-result (str "command not found! " command)))

    result))

(defn recognize
  [commands url]
  (let [url-info (scramble-url url)]
    (reduce (partial apply-command url-info) (ok-result []) commands)))

(def twitter (new-pattern "host(twitter.com); path(?user/status/?id);"))

(new-pattern "host(twitter.com); path(?user/status/?id);")

(scramble-url "http://twitter.com/bradfitz/status/562360748727611392")

(recognize twitter "http://twitter.com/bradfitz/status/562360748727611392")
;; => [[:id 562360748727611392] [:user "bradfitz"]]

(def dribbble (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"))

(recognize dribbble
           "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
;; => [[:id "1905065-Travel-Icons-pack"] [:offset "1"]]
(recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
;; => nil ;; host mismatch
(recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users")
;; => nil ;; offset queryparam missing

(def dribbble2 (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type);"))

(recognize dribbble2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=dgdfg")