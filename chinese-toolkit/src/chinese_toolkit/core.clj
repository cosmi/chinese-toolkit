(ns chinese-toolkit.core
  (:require [clojure.string :as s]))

(defn load-csv [filename]
  (let [lines (s/split-lines (slurp filename))]
    (map #(s/split % #"," 4) lines)))


(defn load-tsv [filename]
  (let [lines (s/split-lines (slurp filename))]
    (map #(s/split % #"\t") lines)))


(defn prepare-hsk []
  (let [inp (load-csv "hsk.txt")
        inp (drop 2 inp)
        dict (atom {})]
    (doseq [[level hanzi pinyin eng :as all] inp
            :let [level (Integer/parseInt level)]
            zi hanzi]
      (swap! dict update-in [zi] conj {:level level :hanzi hanzi :pinyin pinyin :eng eng})
      )
    @dict
    )
 )

(defn separate
  "Returns a vector: [(filter f s), (filter (complement f) s) ]"
  [f s]
  [(filter f s) (filter (complement f) s)])

(defn basicness [word]
  (+ (word :level) (count (word :hanzi)))
  )

(defn sort-words [char words]
  (let [cnt (count words)
        [exact words] (separate #(-> % :hanzi count (= 1) ) words)
        [prefices words] (separate #(-> % :hanzi first (= char)) words)
        [sufices words] (separate #(-> % :hanzi last (= char)) words)
        prefices (sort-by basicness prefices)
        words (sort-by basicness words)
        sufices (sort-by basicness sufices)
        ]
    (concat exact
            (take 2 prefices)
            (take 1 words)
            (take 1 sufices)
            (drop 2 prefices)
            (drop 1 words)
            (drop 1 sufices))
  ))

(defn sort-dict [dict]
  (into {} (for [[k,words] dict]
             [k (sort-words k words)]))
  )

(defn main- [args]
  
  )