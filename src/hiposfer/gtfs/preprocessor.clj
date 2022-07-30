(ns hiposfer.gtfs.preprocessor
  "parse the Markdown GTFS spec definition and returns it as Clojure data structures.

  Uses several heuristics to guess how to interpret the data.

  Useful to avoid monkey patching"
  (:require [clojure.string :as str]
            [markdown2clj.core :as md]
            [clojure.pprint :as pprint]
            [clojure.edn :as edn]))

(def url "https://raw.githubusercontent.com/google/transit/master/gtfs/spec/en/reference.md")

(defn sections [md]
  (let [prev (volatile! (first (:document md)))]
    (partition-by (fn [v] (if (:heading v)
                            (vreset! prev v)
                            (deref prev)))
                  (:document md))))

(defn zipify [section]
  (let [table   (some :table-block section)
        head    (some :table-head table)
        body    (some :table-body table)
        headers (->> (tree-seq coll? seq head)
                     (filter string?)
                     (map str/lower-case)
                     (map #(str/replace % " " "-"))
                     (map keyword))
        rows    (for [row (map :table-row body)]
                  (for [cell row]
                    (->> (tree-seq coll? seq cell)
                         (filter map?)
                         (filter :text)
                         (map :text)
                         (str/join ""))))]
    (map #(zipmap %1 %2) (repeat headers) rows)))

(defn- primary-key [section]
  (reduce (fn [ret block]
            (if-let [p (:paragraph block)]
              (if (re-find #"^Primary key" (:text (first p)))
                (reduced (->> (filter :code (rest p))
                              (mapv #(:text (first (:code %))))))
                ret)))
          []
          section))

(defn- header [section] (-> section first :heading second :text))

(defn- assoc-or-append [m k v]
  (if (m k)
    (update m k #(str (str/trimr %) " " (str/trim v)))
    (assoc m k v)))

(defn- trim-prefix [s p]
  (str/replace s p ""))

(defn- parse-conditional-presence [field ptype]
  (let [split-re (re-pattern (str "Conditionally " ptype ":"))
        [description conditions] (str/split (:description field) split-re)]
    [(str/trimr description)
     (when (not-empty conditions)
       (reduce #(condp re-find %2
                  #"^Required" (assoc-or-append %1 :required (trim-prefix %2 #"Required\s*"))
                  #"^Optional" (assoc-or-append %1 :optional (trim-prefix %2 #"Optional\s*"))
                  #"^Forbidden" (assoc-or-append %1 :forbidden (trim-prefix %2 #"Forbidden\s*"))
                  %1)
               {}
               (map str/trimr (str/split conditions #"-\s*"))))]))

(defn- parse-presence-edge-case [f]
  (let [[description conditions] (str/split (:description f) #"REQUIRED\s*")]
    [(str/trimr description)
     (when (not-empty conditions) {:required (str/trimr conditions)})]))

(defn- conditional-presence? [f]
  (re-find #"^Conditionally (.+)" (:presence f)))

(defn- update-for-conditional-presence [f description conditions]
  (cond-> (assoc f :description description)
    (not-empty conditions) (assoc :presence-conditions conditions)))

(defn- parse-presence
  ([f]
   (parse-presence f false))
  ([f edge-case]
   (if-let [conditional (conditional-presence? f)]
     (let [[description conditions] (if edge-case
                                      (parse-presence-edge-case f)
                                      (parse-conditional-presence f (nth conditional 1)))]
       (update-for-conditional-presence f description conditions))
     f)))

(defn- parse-suffix [file-name field-name description]
  (if-let [find-re (case file-name
                     "routes.txt" (when (or (= field-name "continuous_pickup") (= field-name "continuous_drop_off"))
                                    #"(.+?)(Values for .+)")
                     "trips.txt" (when (= field-name "direction_id")
                                   #"(.+?)(Example:.+)")
                     "stop_times.txt" (when (or (= field-name "continuous_pickup") (= field-name "continuous_drop_off"))
                                        #"(.+?)(If this field .+)")
                     "calendar_dates.txt" (when (= field-name "exception_type")
                                            #"(.+?)(Example: .+)")
                     "fare_transfer_rules.txt" (when (= field-name "fare_transfer_type")
                                                 #"(.+?)(Cost processing interactions .+)")
                     "translations.txt" (when (= field-name "table_name")
                                          #"(.+?)(Any file .+)")
                     "attributions.txt" (when (= field-name "is_producer")
                                          #"(.+?)(At least .+)")
                     nil)]
    (rest (re-find find-re description))
    [description nil]))

(defn- enum-value [m val-match text]
  (cond-> (assoc m (edn/read-string (nth val-match 1)) (str/trimr text))
    (some? (last val-match)) (assoc nil (str/trimr text))))

(defn- numeric-enum-values [valstr]
  (let [enum-re #"(\d+)(\s+\(?or (?:blank|empty)\)?)?\s*-\s+"
        val-matches (re-seq enum-re valstr)
        texts (rest (str/split valstr enum-re))]
    (reduce-kv enum-value {} (zipmap val-matches texts))))

(defn- wheelchair-boarding-enum-values [valstr]
  (let [enums-re #"For parentless stops:\s*(.*)For child stops:\s*(.*)For station entrances/exits:\s*(.*)"]
    (zipmap [:parentless-stops :child-stops :station-entrances-and-exits]
            (map numeric-enum-values (rest (re-find enums-re valstr))))))

(defn- enum-values [field-name valstr]
  (case field-name
    "table_name" (let [values (str/split valstr #"\s*-\s+")
                       len (count values)
                       trimmed (update values (- len 1) #(re-find #"\w+" %))]
                   (reduce (fn [m v] (assoc m (str/trimr v) nil)) {} (subvec trimmed 1 len)))
    "wheelchair_boarding" (wheelchair-boarding-enum-values valstr)
    (numeric-enum-values valstr)))

(defn- parse-enum [field file-name]
  (if (= (:type field) "Enum")
    (let [field-name (:field-name field)
          [description suffix] (parse-suffix file-name field-name (:description field))
          split-re (if (= field-name "table_name")
                     #"Allowed values are:\s*"
                     #"Valid options are:\s*")
          [description valstr] (str/split description split-re)
          ;; for pathways.txt is_bidirectional
          valstr (or valstr (-> (re-find #".+?(\d+.+)" description)
                                (nth 1)))]
      (cond-> (assoc field :description (str/trimr (str description suffix)))
        valstr (assoc :values (enum-values field-name valstr))))
    field))

(defn- update-day-of-week [field mon-desc mon-vals]
  (if-let [day (re-find #".+day$" (:field-name field))]
    (let [day (str/capitalize day)]
      (-> (assoc field :description (str/replace mon-desc "Monday" day))
          (assoc :values (zipmap (keys mon-vals)
                                 (map #(str/replace % "Monday" day) (vals mon-vals))))))
    field))

(defn- update-entity [field description values]
  (if-let [entity (last (re-find #"^is_(.+)$" (:field-name field)))]
    (-> (assoc field
               :description
               (str/replace description "is producer" (str "is " entity)))
        (assoc :values values))
    field))

(defn- parse-wildcard-primary-keys [file-data]
  (if (= (first (:primary-key file-data)) "*")
    (assoc file-data
           :primary-key
           (mapv :field-name (:fields file-data)))
    file-data))

(defn- parse-enum-edge-cases [file-data]
  (case (:file-name file-data)
    "calendar.txt"
    (let [monday (first (filter #(= (:field-name %) "monday") (:fields file-data)))]
      (update file-data :fields (fn [fields]
                                  (map #(update-day-of-week % (:description monday) (:values monday))
                                       fields))))
    "attributions.txt"
    (let [producer (first (filter #(= (:field-name %) "is_producer") (:fields file-data)))]
      (update file-data :fields (fn [fields]
                                  (map #(update-entity % (:description producer) (:values producer))
                                       fields))))
    file-data))

(defn- parse-unique-id [field]
  (if (= (:type field) "Unique ID")
    (assoc field :unique true)
    field))

(defn- parse-foreign-id [field]
  (if-let [references (last (re-matches #"Foreign ID referencing\s+(.*)" (:type field)))]
    (assoc field :references references)
    field))

(defn- parse [raw]
  (let [content    (md/parse raw)
        parts      (sections content)
        feed-files (some #(when (= "Dataset Files" (header %)) %) parts)
        files      (filter #(when (str/ends-with? (header %) ".txt") %)
                           (sections content))
        files-data (for [file files]
                     (let [file-name (header file)]
                       (-> {:file-name file-name
                            :primary-key (primary-key file)
                            :fields (->> (zipify file)
                                         (map #(parse-presence % (= file-name "transfers.txt")))
                                         (map #(parse-enum % file-name))
                                         (map parse-unique-id)
                                         (map parse-foreign-id))}
                           parse-wildcard-primary-keys
                           parse-enum-edge-cases)))]
    {:feed-files (map parse-presence (zipify feed-files))
     :field-definitions files-data}))

(defn -main [out]
  (spit out (with-out-str (pprint/pprint (parse (slurp url))))))

(comment
  (-main "resources/reference.edn"))
