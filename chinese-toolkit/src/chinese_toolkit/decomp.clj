(ns chinese-toolkit.decomp
  (:require [clojure.string :as s]
            [clojure.set :as sets]))

(defn- maybe-integer [s]
  (cond (re-matches #"[0-9]+" s)
        (Integer/parseInt s)
        (< 1 (count s))
        (do (assert (Character/isHighSurrogate (first s)))
            (assert (Character/isLowSurrogate (second s)))
            (+
             100000
             (* 10000 (- (int (first s)) 0xD800))
             (* 1 (- (int (second s)) 0xDC00))
             ))
        :else s))

(defn load-heisig
  ([filename] (s/split-lines (slurp filename)))
  ([] (load-heisig "hanzi.txt")))

(defn load-decomp
  ([] (load-decomp "cjk-decomp-0.4.0.txt"))
  ([filename ]
     (let [lines (s/split-lines (slurp filename))]
       (for [line lines]
         (let [[line sym method args] (re-matches #"(.|[0-9]+):([a-z0-9/])+\(((?:(?:.|[0-9]+)(?:,.|.[0-9]+)*)?)\)" line)
               args (mapv maybe-integer (remove empty? (s/split args #",")))
               sym (maybe-integer sym)]
           {:sym sym
            :def line
            :method method
            :args args})
         ))))

(defn calculate-comp [decomp-map]
  (let [ret (atom {})]
    (doseq [[sym {:keys [def method args]}] decomp-map arg args]
      (swap! ret assoc arg (concat [sym] (get @ret arg))))
    (deref ret)))



(defn init []
  (let [dec (load-decomp "cjk-decomp-0.4.0.txt")
        dec-correct (load-decomp "cjk-decomp-corr.txt")
        decomp (zipmap (map :sym dec) dec)
        decomp-correct (zipmap (map :sym dec-correct) dec-correct)
        _ (print :! (count decomp) (count decomp-correct))
        decomp (merge decomp decomp-correct)
        dec (vals decomp)
        comp (calculate-comp decomp)
        heisig* (load-heisig)
        heisigs (set heisig*)
        decomp-map* (zipmap (map :sym dec) (map :args dec))]
    (def dict-decomp decomp)
    (def dict-comp comp)
    (def heisig-set heisigs)
    (def heisig-ordered (vec heisig*))
    (def heisig-ordering (zipmap heisig* (iterate inc 0)))
    (def heisig-comp (zipmap (keys comp) (map #(filter heisigs %) (vals comp))))
    (def decomp-map decomp-map*)
    (def comp-map comp)
    (def heisig-decomp (zipmap (keys decomp-map*) (map #(filter heisigs %) (vals decomp-map*))))
    
    ))



(defn get-decomp-set [sym]
  (let [que (transient [sym])
        out (transient #{})]
    (while (< 0 (count que))
      (let [head (que (dec (count que)))]
        (when-not (contains? out head)
          (pop! que)
          (conj! out head)
          (assert (dict-decomp head) (prn-str head))
          (doseq [el ((dict-decomp head) :args)]
            (conj! que el)))))
    (persistent! out)))


(defn get-similar-heisigs [sym]
  (assert (-> sym dict-decomp))
  (let [sub (cons sym (-> sym dict-decomp :args))]
    (concat
     [(filter heisig-set (rest sub))]
     (for [s sub]
       (do (sort-by heisig-ordering
                    (filter heisig-set (dict-comp s))))))))



(defn init2 []
  (init)
  (let [similars (zipmap heisig-ordered (map get-similar-heisigs heisig-ordered))]
    (def dict-similar similars)
    (def dict-similar-concat (zipmap (keys similars) (map #(apply concat %) (vals similars))))
    ))

(defn get-composing-heisigs [min-composity]
  (->> heisig-comp
       (filter #(-> % first heisig-set))
       (filter #(-> % val count (>= min-composity)))
       keys
       (map (juxt identity #(-> % heisig-comp count)))))


(defn decompose-into-common-subelements [sym common-elements]
  (let [que (transient (vec (cons sym (decomp-map sym))))
        out (transient #{})]
    (while (< 0 (count que))
      (let [head (que (dec (count que)))]
        (when-not (contains? out head)
          (pop! que)
          (conj! out head)
          (assert (decomp-map head) (prn-str head))
          (when-not (common-elements head)
            (doseq [el (decomp-map head)]
              (conj! que el))))))
    (persistent! out))
  )


(def recursive-comp)
(defn recursive-comp*
  ([sym]
     (let [comps (dict-comp sym)]
       (set (apply concat [sym]
                   (for [comp comps :when (not= comp sym)]
                     (recursive-comp comp)))))))

(defn recursive-comp [& args] (apply recursive-comp* args))
(def recursive-comp (memoize recursive-comp))

(defn recursive-comp-heisig-lim [sym limit]
  ;; DZIALA
  (let [comps (cons sym (dict-comp sym))
        filtered (filter heisig-set comps)]
    (list* filtered
           (when (< (count filtered) limit)
             (doall (map #(recursive-comp-heisig-lim % (dec limit)) (remove #(= sym %) comps)))))))




;; (defn recursive-decomp-heisig-lim [sym limit]
;;   (let [comps (cons sym (dict-comp sym))
;;         filtered-comps (filter heisig-set comps)]
;;     (if 
;;     ))


(defn recursive-comp-heisig [sym]
  (->> sym recursive-comp (filter heisig-set)))

;;(defn find-similar-heisig2
;;   ([sym limit]
;;      (let [comps (dict-comp sym)
;;            decomps (decomp-map sym)]
;;        (if (>= (count direct) mode-boundary)
;;          direct
;;          (map #(filter heisig-set %)
;;               (map #(find-similar-heisig2 % (dec mode-boundary)) (remove #(= % sym) (decomp-map sym))))))))



;; (defn find-similar-heisig2
;;   ([sym mode-boundary]
;;      (let [direct (flatten (recursive-comp-heisig-lim sym mode-boundary))]
;;        (if (>= (count direct) mode-boundary)
;;          direct
;;          (map #(filter heisig-set %)
;;               (map #(find-similar-heisig2 % (dec mode-boundary)) (remove #(= % sym) (decomp-map sym)))))))
;;   ([sym] (find-similar-heisig2 sym 4)))

(defn create-decomp-graph []
  (into {} (for [[k decomp] decomp-map]
             [k (zipmap decomp (repeat (list :decomp)))])))


(defn add-relation [graph from to relation]
  (update-in graph [from to] conj relation))

(defn augment-graph [graph' fn]
  (let [graph (atom graph')]
    (doseq [[k neibors] graph'
            [neibor relations] neibors
            relation relations
            :let
            [new-relation (fn k neibor relation)]]
      (when new-relation (swap! graph #(apply add-relation % new-relation))))
    @graph))

(defn create-comp-decomp-graph []
  (-> (create-decomp-graph) (augment-graph (fn [from to relation]
                                             (when (= relation :decomp)
                                               [to from :comp])))))


(def plus-similar [["天" "夭"] ["凑" "奏"] ["壬" "王"]["干" "千"] [63648 "里"] ["土" "士"] ["见" "贝"] ["曰" "日"]])

(defn calculate-recursive-map [from-map initial-set]
  (loop [done #{}
         out {}
         que initial-set]
    (if (not-empty que)
      (let [sym (first que)]
        (if (done sym)
          (recur done out (rest que))
          (let [not-done (remove done (from-map sym))]
            (if (empty? not-done)
              (recur 
               (conj done sym)
               (assoc out sym (apply sets/union (set (from-map sym)) (map out (from-map sym))))
               (rest que))
              (recur done out  (concat not-done que))))))
      out)))


(defn init3 []
  (init2)
  (let [rec-comps (zipmap heisig-ordered (map #(-> (recursive-comp-heisig-lim % 4) flatten set) heisig-ordered))]
    (def rec-comps1 rec-comps)
    (def rec-decomps (calculate-recursive-map decomp-map (keys decomp-map)))
    (def rec-comps (calculate-recursive-map comp-map (keys comp-map)))

    )
  (let [graph (create-comp-decomp-graph)
        graph (reduce (fn [graph set]
                        (reduce #(add-relation % (first %2) (second %2) :similar) graph
                                (for [s1 set s2 set :when (not= s1 s2)]
                                  [s1 s2])))
                      graph plus-similar)]
    (def graph graph)))

(defn find-path
  ([from to max-rec]
     (cond (= from to)
           [[]]
           (= 0 max-rec)
           []
           (> max-rec 0)
           (for [rels (graph from)
                 :let [[sym relations] rels]
                 relation relations
                 :let [paths (find-path sym to (dec max-rec))]
                 path paths]
             (cons [relation sym] path))))

  ([from to]
     (let [wrong (last (take-while #(empty? (find-path from to %)) (range 0 6)))]
       (find-path from to (+ 2 wrong)))))


(defn calculate-params [sym]
  (let [p {:sym sym
           :component (-> sym rec-comps count)
           :hei-component (->> sym rec-comps (filter heisig-set) count)
           :subcomponent (->> (rec-comps sym) (map rec-comps) (map count) sort reverse)
           :hei-subcomponent (->> (rec-comps sym) (map rec-comps) (map #(filter heisig-set %)) (map count) sort reverse)
           }]
    (assoc p
      :comp-sub (/  (or (-> p :subcomponent first ) 0) (max 1 (p :component)))
      :hei-comp-sub (/ (or (-> p :hei-subcomponent first) 0) (max 1 (p :hei-component)))
      
      )))



(defn main- [args]
  
  )
