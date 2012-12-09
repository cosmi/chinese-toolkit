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


(def comp-types #{\一 ;= Graphical primitive, non composition (second character is always *)
                  \吅 ;= Horizontal composition (when repetition, the second character is *)
                  \吕 ;= Vertical composition (when repetition, the second character is *)
                  \回 ;= Inclusion of the second character inside the first (门, 囗, 匚...)
                  \咒 ;= Vertical composition, the top part being a repetition.
                  \弼 ;= Horizontal composition of three, the third being the repetition of the first.
                  \品 ;= Repetition of three.
                  \叕 ;= Repetition of four.
                  \冖 ;= Vertical composition, separated by "冖".
                  \+ ;= Graphical superposition or addition.
                  \? ;= Unclear, seems compound but ...
                  \* ;= Vertical combination, but atypical.
                  })

(defn load-decomp [filename]
  (let [text (slurp filename)
        matcher (re-matcher #"(?m)<pre>.*?\n([\s\S]*?)</pre>" text)
        pres (map second (take-while boolean (repeatedly #(re-find matcher))))
        _ (spit "test.txt" (apply str pres))
        lines (s/split-lines (apply str pres))
        lines (remove #(.startsWith % "#") lines)]
    (doseq [line lines c line] (when (<= 0xD800 (int c) 0xD8FF) (prn (seq line))))
    (into {}
          (for [[_ hanzi strokes mode p1 s1 v1 p2 s2 v2 cangjie radical :as all] (map #(s/split % #"\t") lines)]
            (do
              (assert (empty? _) (prn-str [_]) )
              (assert (= 1 (count mode)) [mode])
              (assert (comp-types (get mode 0)) [mode])
              (assert (= (count all) 12))
              (let [[v1 v2] (map #(case % "?" false "？" false "" true)[v1,v2])
                    is-radical? radical
                    ]
                [hanzi {:hanzi hanzi :strokes strokes :mode mode
                        :cangjie cangjie :radical radical
                        :comp [{:hanzi p1 :strokes s1 :valid v1}
                               {:hanzi p2 :strokes s2 :valid v2}]}]
                ))))))


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
                  eng [eng];(remove empty? (s/split eng  #"\"|(;\s*)"))
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