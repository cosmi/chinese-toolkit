(ns chinese-toolkit.core
  (:require [clojure.string :as s]))

(defn separate
  "Returns a vector: [(filter f s), (filter (complement f) s) ]"
  [f s]
  [(filter f s) (filter (complement f) s)])

(defn trim-comma [line]
  (let [line (s/trimr line)]
    (if (= (last line) \,) (subs line 0 (dec (count line))) line)
    ))

(defn load-csv [filename]
  (let [lines (s/split-lines (slurp filename))]
    (map #(s/split (trim-comma %) #"," 4) lines)))


(defn load-tsv [filename]
  (let [lines (s/split-lines (slurp filename))]
    (map #(s/split % #"\t") lines)))

(def tones {0 [\a \e \i \o \u \ü]
            1 [\ā \ē \ī \ō \ū \ǖ]
            2 [\á \é \í \ó \ú \ǘ]
            3 [\ǎ \ě \ǐ \ǒ \ǔ \ǚ]
            4 [\à \è \ì \ò \ù \ǜ]})

(def to-tones {\a [\a \ā \á \ǎ \à],
               \e [\e \ē \é \ě \è],
               \i [\i \ī \í \ǐ \ì],
               \o [\o \ō \ó \ǒ \ò],
               \u [\u \ū \ú \ǔ \ù],
               \ü [\ü \ǖ \ǘ \ǚ \ǜ]})

(def vowels (set (apply concat (vals tones))))
;; (into {}
;;       (for [[i, letter] (map-indexed vector [\a \e \i \o \u \ü])]
;;         [letter (vec (for [j (range 5)] (get-in tones [j i])))])))

(defn correct-syllabe [syl]
  (let [syl (s/replace (s/lower-case syl) #"v" "ü")]
    (assert (re-matches #"[a-züń]{1,6}[1-5]?" syl) syl)
    (let [tone (Integer/parseInt (or (re-find #"\d" syl) "5"))
          base (re-find #"[a-züń]+" syl)
          iu (re-find #"iu" syl)
          dir (if iu
                [\a \e \u \ü \i \o \ą]
                [\a \e \i \o \u \ü \ą])]
      (if (= 5 tone)
        base
        (loop [old (seq base) base (seq base) dir dir]
          (if (not= old base)
            (apply str base)
            (recur base
                   (replace {(first dir) (get-in to-tones [(first dir) tone])} base)
                   (rest dir))))))))


(defn join-syllabes [syls]
        (loop [syls syls out []]
          (if (seq syls)
            (let [fst (first syls) oth (seq (rest syls))]
              (recur oth
                     (if (and oth (-> fst last vowels) (-> (first oth) first vowels))
                       (conj out fst \')
                       (conj out fst))))
            (apply str out))))


(def mw-re #"CL:\s*((?:(?:(?:\S\|)*\S\[[a-züń]{1,6}[1-5]?\]),?\s*)+)")
(defn measure-word [syl]
  (when (re-find #"CL" syl)
    (assert (re-matches mw-re syl) syl)
    (let [[all, ins] (re-matches mw-re syl)
          mws (s/split ins #",\s")
          mws (for [mw mws] (let [[all, simpl, pinyin] (re-matches #"(?:\S\|)?(\S)\[([a-züń]{1,6}[1-5]?)\]" mw)]
                              (str simpl \[ (correct-syllabe pinyin) \])))]
      mws)))

(defn prepare-hsk []
  (let [inp (load-csv "hsk.txt")
        inp (drop 2 inp)
        dict (atom {})]
    (doseq [[level hanzi pinyin eng :as all] inp
            :let [level (Integer/parseInt level)
                  pinyin (join-syllabes (map correct-syllabe (s/split pinyin #"\s")))
                  eng (remove empty? (s/split eng  #"\"|(;\s*)"))
                  mw nil;[mw eng] (separate measure-word eng)]
                  ]
            zi hanzi]
      (assert (<= (count mw) 1) (vec mw))
      (swap! dict update-in [zi] conj {:level level :hanzi hanzi :pinyin pinyin :eng eng :mw (map measure-word mw)})
      )
    @dict
    )
 )

(defn basicness [word]
  (+ (word :level) (count (word :hanzi)))
  )

(defn exhaustive-interleave [colls]
  (when-let [colls (seq (remove empty? colls))]
    (concat (map first colls) (exhaustive-interleave (map rest colls)))
    ))





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
            (take 1 prefices)
            (take 1 words)
            (take 1 sufices)
            (exhaustive-interleave [(drop 1 prefices)
                                    (drop 1 words)
                                    (drop 1 sufices)]))))

(defn sort-dict [dict]
  (into {} (for [[k,words] dict]
             [k (sort-words k words)])))

(defn gen-csv [sep values]
  (apply str
         (interpose "\n"
                    (for [value values] (apply str (interpose sep value))))))


(defn format-definition [eng]
  (let [[mws defs] (separate measure-word eng)]
    (interpose "; " (concat (map measure-word mws) defs))))



(defn print-word [word]
  (apply str  (word :level) \) (word :hanzi) \[ (word :pinyin) "]: " (interpose "; " (concat (word :mw) (word :eng))) ))

(defn print-dict [filename dict]
  (spit filename
        (gen-csv "\t"
                 (for [[k words] (sort dict)]
                   (list* k (map print-word words))
                   ))))


(defn main- [args]
  
  )