(ns chinese-toolkit.core)

(defn load-csv [filename]
  (let [lines (split-lines (slurp filename))]
    (map #(split % ";") lines)))


(defn load-tsv [filename]
  (let [lines (split-lines (slurp filename))]
    (map #(split % "\t") lines)))


