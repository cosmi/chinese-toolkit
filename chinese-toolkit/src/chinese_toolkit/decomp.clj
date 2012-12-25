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
         (let [[line sym method args] (re-matches #"(.|[0-9]+):((?:[a-z0-9/])+)\(((?:(?:.|[0-9]+)(?:,.|.[0-9]+)*)?)\)" line)
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
     (let [wrong (last (take-while #(empty? (find-path from to %)) (range 0 4)))]
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


(defn calculate-classes [sym]
  (set (remove nil?
          (list
           (when (heisig-set sym) :heisig)
           (if (number? sym) :extra :basic)
           (when (not-empty (filter heisig-set (rec-comps sym)))
             :partial)
           (when (< 1 (count (filter heisig-set (rec-comps sym))))
             :combining)
           (when (< 3 (count (filter heisig-set (rec-comps sym))))
             :primitive)
             ))))

(defn path-weight [path]
  (let [types (map first path)
        syms (drop-last (map second path))]
    (count path)))

(defn calculate-similarity [sym1 sym2]
  (when (> 1 (rand 10)) (prn :! sym1 sym2))
  (->> (find-path sym1 sym2 3)
       (map path-weight)
       sort
       (map * (iterate inc 1))
       (reduce #(/ (* %1 %2) (+ %1 %2)) 100000)
       ))

(defn find-closest [sym]
  (->>
   (for [sym2 heisig-ordered]
    [sym2 (calculate-similarity sym sym2)])
   (sort-by second)))


(defn get-tree [sym]
  [sym
   (-> sym
       dict-decomp
       :method)
   (->> sym
        dict-decomp
        :args
        (map get-tree))])

(defn method-difference [m1 m2]
  (cond (= m1 m2) 0
        (= (first m1) (first m2)) 1/2
        :else 1))

(def tree-difference)
(defn children-difference [c1 c2 count-limit]
  (let [n1 (count c1)
        n2 (count c2)
        diff (mapv (fn [ch1] (mapv #(tree-difference ch1 % count-limit) c2)) c1)]
    
  ))


(defn tree-difference
  ([t1 t2 count-limit]
     (if (< count-limit 0) 0
         (let [[s1 m1 a1] t1
               [s2 m2 a2] t2]
           (if (= s1 s2) 0
               (min
                (+ (method-difference m1 m2)
                   (
                    )
                   
                   ))))))
  ([t1 t2] (tree-difference t1 t2 1000)))


(defn simple-diff [s1 s2]
  (let [d1 (rec-decomps s1)
        d2 (rec-decomps s2)]
    (/ (count (sets/union d1 d2)) (+ (count d1) (count d2)) 2)))

(defn decomp-steps [sym d remove-costs]
  ;; ile kroków dekompozycji, aby osiągnąć fragmenty zawarte w d,
  ;; usuwanie elementu kosztuje remove-cost kroków
  (if (contains? d sym) 0
     (min (first remove-costs)
          (+ 1
             (->> sym dict-decomp :args
                  (map #(decomp-steps % d (rest remove-costs)))
                  (reduce + 0))))))

(defn decomposition-diff [s1 s2 cost-limit]
  (let [d1 (conj (rec-decomps s1) s1)
        d2 (conj (rec-decomps s2) s2)
        remove-costs (cons (* 4 cost-limit) (iterate #(* % 0.8) cost-limit))]
    (+ (decomp-steps s1 d2 remove-costs)
       (decomp-steps s2 d1 remove-costs))))


(defn sym-class [sym]
  (cond (string? sym)
        (let [ch (int (first sym))]
          (cond
            (< ch 0x2e80) (throw (Exception.))
            (<= ch 0x2eff) :cjk-radical
            (<= ch 0x2fdf) (throw (Exception.)) ;;:kangxi-radical
            (<= ch 0x2fff) (throw (Exception.)) ;; :ideo-desc-char
            (<= ch 0x303f) (throw (Exception.)) ;; :punctuation
            (<= ch 0x31bf) (throw (Exception.)) ;; hiragana etc.
            (<= ch 0x31ef) :cjk-stroke
            (<= ch 0x33ff) (throw (Exception.)) ;; enclosed letters, compability etc.
            (<= ch 0x4dbf) :cjk-extA
            (<= ch 0x4dff) (throw (Exception.)) ;; yijing
            (<= ch 0x9fff) :cjk-unified
            (<= ch 0xf8ff) (throw (Exception.)) ;; various
            (<= ch 0xfaff) :cjk-compability
            :else (throw (Exception.))
            ))
        (< sym 100000) :component
        (>= sym 100000) :multibyte)
  )

(defn sym-function [sym]
  (case (sym-class sym)
    (:cjk-radical :kangxi-radical) :radical
    :cjk-stroke :stroke
    :component :component
    (:cjk-extA :cjk-unified :cjk-compability :multibyte) (if (heisig-set sym) :common :uncommon)))

(def smart-is-primitive?)

(defn smart-is-primitive?* [sym]
  (let [k (->> sym comp-map (map smart-is-primitive?)  frequencies)
        {prims :primitive finals :final strokes :stroke inters :intermediate
         :or {prims 0 finals 0 strokes 0 inters 0}} k
        k1 (->> sym rec-comps (map smart-is-primitive?)  frequencies)
        {prims1 :primitive finals1 :final strokes1 :stroke inters1 :intermediate
         :or {prims1 0 finals1 0 strokes1 0 inters1 0}} k1
        all (reduce + 0 (vals k))
        all1 (reduce + 0 (vals k1))
        ]
    (cond (< finals1 2)
          (if (= :common (sym-function sym)) :final :intermediate)
          (> prims (+ finals))
          :stroke
          :else
          :primitive)))

(def smart-is-primitive? (memoize smart-is-primitive?*))

(defn break-into-primitives [sym]
  (if (= :primitive (smart-is-primitive? sym))
    #{sym}
    (apply sets/union (map break-into-primitives (decomp-map sym)))))



(defn main- [args]
  
  )
