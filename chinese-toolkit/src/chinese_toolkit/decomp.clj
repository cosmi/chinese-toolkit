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






(defn break-into-primitives-old [sym]
  (if (= :primitive (smart-is-primitive? sym))
    #{sym}
    (apply sets/union (map break-into-primitives (decomp-map sym)))))


(def hard-primitives #{"几"  "亠" "开" "欠" "宀" "讠"  "彡" "刂" "乂"
                       "厂" "饣" "七"  "心" "口"  "斤" "两"
                       "卄" "忄" "古"  "钅" "工" "日" "旦" "分" 
                       "万" "门"  "雨" "月"  "又"  "酉"  "⻊"
                         "火" "冫" "下" "虫" "立"  "卫" 
                       "灬"  "扌"  "本" "再" "犭" "与" "艮" "⺮" "目"
                       "田"  "子" "少" "山" "云" "疒" "互" "耳"
                       "女" "石" "且" "穴" "五" "更" "氵" "匕" "肖"
                        "辶" "厶" "户" "囗" "尸" "丘" "寸" "方" "丙" "⺙"
                       "隹" "歹"  "人" "画" "礻" "力" "亻"  "卜"
                       "贝" "阝" "丝" "丽" "白"  "巾" "百" "长"  "广"
                       "纟" "皿"  
                       "舟" "牛" "弓" "皮" "中"
                       "令" "占" "刀" "鬼" "勹"
                       "由" "小"  "犬" "舌" "里" "合" "戋"
                       (maybe-integer "𠂇") ;左右 740135
                       "佥" 37024 "非"  "匚" "示" "龙" "申" "己" "共" "彐"
                       "京" "莫" "文" "包" "夂" "见" "㇠"  "区" "凶"  "冈"
                       
                       "九" "世" "自" "千" "龺" "卓" "了" "罒" "元"
                       "⺹" "母" "畐" "可" "毌"
                       "兄" "夕"  "光"   "水" "永" "求" "泉"
                       "早" "先"
                       "尤"  
                       "冗"   "只" 
                       (maybe-integer "𤽄") ; spring 原
                       (maybe-integer "𠮛") ;
                       (maybe-integer "𠂉")
                       37386
                       63648
                       37044 ;学觉
                       "冖" 
                       "丬" "卂" "兆"  37045 ;亨 itp.
;; uniques:
                       "四" "凹" "凸" "升" "曰" "伞" "个" "丹" "书" "民" "丐" 37088
;; potentials
                       "帀" "亡" "尢" "龸" "覀"  "兀" "㐄" "内" 
                       "丰" "⺈"  "其"  "免"  "公" "而"    "甫" "片" "用"  
                       "甲" "幺" "井" "巳" "㔾" "西" "辛" "乃" "臼"  "手" "缶"
                         "去" "至"  "氏" "㠯"  "丂" "也"

                       38073 ;; zoo
                       (maybe-integer "𧘇") ;; frendzle
                       ;; BROŃ
                       "戈" "弋" 37427 "戊" 
                       ;;FRENDZLE
                       "农"  "衣" "衤"
                       ;;T
                       "于" "丁"  "干"
                       ;;DRZEWA
                       "朿" "禾" "末" "木" "束" "乐"  "朱" "央" "未" "米" "朩"
                       ;;DUŻY
                       "夭" "夫" "矢" "大" "天" "夹"
                       ;;ZIEMIA
                       "壬" "土" "生" "士" "上" "王"
                       ;;WODA
                       "氺" 
                       "川" "州" "巛" 37110 ;;jak mojżesz
                       ;; OWCA
                       "羊" "车" "午" "年"
                       ;; ŚWINIA
                       (maybe-integer "𠃓") "豕" 99972 "勿"
                       ;; PTAK
                       37154 "乌" "马"
                       ;; ZWIERZA
                       37230 ;ryba bez nogi
                       ;;FRAMUGA
                       "同" "向" "尚" "周""冋"                       
                       ;;DROGA
                       "正"  "止" "走" "疋" "廴"
                       ;; HACZYK
                       "气" "乞" "飞" "㇟" ;haczyk
                       ;; SERCE
                       
                       ;;MOWA
                       "言"
                       ;; AZJA
                        37456  "业" "亚"
                       ;; księżyce
                       38068 ; na/nar
                       "禺"
                       ;; RAMIĘ
                       "习" "司" "刁" 
                       ;; GENIUSZ
                       "才" "牙" "矛" "予"
                       ;;INNE
                       "屰"
                       "彳" "亍" "竹"
                       "肉" "头"
                       "瓦"
                       "今" "亥" "丩" "之" "乡" "发"
                       "旡" "无"
                       ;; SREBRO
                       37084
                       ;TODO
                       "商"  "㇗"  "⺺" "匃"

                       "册" "韦" "身" "屯"  37276 "亼" "父" "卪" "卩" 37086 37313 "曹" "曲" "令"
                       }) 

(def soft-primitives #{"一" "十" "三" "二" "㇔" "八" "丷" "㇑" "儿"  "六" "⺊" "䒑"
                       "冃"
                       "丆" "㇓" 
                       (maybe-integer "𦣻") ;"夏" 
                       "㇐" ;- kreska jak yi (jeden)
                       "㇏" 
                       "㇒" "㇖" 37143 37116 37508 37712
                        (maybe-integer "𠕁")
                       })
(def combined-primitives #{"殳" "台" "青" "昔" "圭" "交" "各" "羽"  "林" "风" "凡" "吾" "昌" "胃" "丸"  "串" "朝" 
                           "贞" "朋" "勺" 
                           "旬" "匀" "句"
                           "玉" "皇" "全" "主"
                           "具" "直" "真"
                           "页" "相" "孙" "式"
                           "刃" "左" "右" "有" "切" "负" "召" "哥" "则" "巩" "如" "贯"
                           "克" "多" "名" "罗" "太" "奇" "胡" "寺" "炎" "昭" "厓" "黑" "冒" "亘" "宣"
                           "安" "宁" "呆" "查" "若" "苗" "明" "军" "然" "告" "臭" "介" "荅" "金" 37310
                           "首" "夏" "亨" "享"
                           "亏" 
                           "回" "舛" "从"
                           "必" "吅" "豆" "匆" "角"
                           "官" (maybe-integer "𠂤")
                           "俞" "前" "刖" "尧"
                           "袁" 37432
                           "危" "厄" "仓" "及" "者" "或"
                           "玨"
                           "㐬"  "岛"
                           48108 ; góra Mojżesza
                           "⺪" "朔"
                           "行"
                           37271 "桼"
                           "买" "并" "鱼"
                           "那" "象" "竭""彖"
                           "卬" "既"
                           "亦" "赤" "争" "唐" "聿" "事"
                           "鸟" "糸" "扁" 37176 "卵" "卯" "祭"
                           }) 


(def hard-coded {"真" ["真" "直" "具" 37386]
                 "克" ["十" "兄" "古"]
                 "巛" ["巛" "川"]
                 "事" ["事" "⺺"]
                 "局" ["尸" "句"]})
                 

(def all-primitives (sets/union hard-primitives soft-primitives combined-primitives))
(def equivalent-primitives {"曰" "日"
                            "⺊" "卜"
                            "㇐" "一"
                            "毌" "母"
                            63648 "里"
                            38262 "舌"
                            37044 "龸"
                            "币" "帀"
                            37110 "川"
                            99972 "勿"
                            37508 "⺺"
                            37276 "东"
                            (maybe-integer "𤽄")"泉"})

(def similar-sets '[("丝" "纟" "乡" "幺")
                   ("勿" 99972)
                   ("衣" 1030519 "农")
                   ("旡" "牙" "尢" "无")
                   ("区" "凶" "冈")
                   ])
  


(assert (empty? (sets/intersection hard-primitives soft-primitives)) (sets/intersection hard-primitives soft-primitives))
(assert (empty? (sets/intersection hard-primitives combined-primitives))(sets/intersection hard-primitives combined-primitives))
(assert (empty? (sets/intersection combined-primitives soft-primitives)))

(defn break-into-primitives* [sym]
  (cond ;(equivalent-primitives sym) (break-into-primitives (equivalent-primitives sym))
    (hard-coded sym) (hard-coded sym)
        (hard-primitives sym) [sym]
        (soft-primitives sym) [sym]
        (combined-primitives sym) (cons sym (remove soft-primitives (mapcat break-into-primitives* (decomp-map sym))))
        :else (if (empty? (decomp-map sym))
                [:bad sym]
                (mapcat break-into-primitives* (decomp-map sym)))))

(defn break-into-primitives [sym]
  (map #(equivalent-primitives % %) (break-into-primitives* sym)))





(defn find-primitives []
  (->> heisig-set (mapcat decomp-map) frequencies (sort-by second) reverse))

(defn break-good [sym]
  (or (hard-primitives sym) (every? #(or (hard-primitives %) (combined-primitives %)) (decomp-map sym))))

(defn potential-primitive?* [sym]
  (and (->> sym rec-comps (filter heisig-set) count (< 1))
       (let [sets (->> sym comp-map (map rec-comps) (map #(filter heisig-set %)) (sort-by count) reverse (map set))
             ]
         (and (not-empty (first sets))
              (not-empty (sets/difference (apply sets/union (rest sets)) (first sets)))
              (count (filter heisig-set (rec-comps sym)))
              ))))

(def potential-primitive? (memoize potential-primitive?*))

(defn check-breaking ([from to]
                        (->> heisig-ordered
                             (map (juxt identity break-into-primitives))
                             (remove break-good)
                             (drop from) (take to)
                             (map (juxt identity #(->> (first %) rec-comps (filter heisig-set) count)))
                             clojure.pprint/pprint)))

(defn init4 []
  (init3)
  (let [pri-map* (into {} (map (juxt identity #(-> % break-into-primitives set)) heisig-set))]
    (doseq [[k,v] pri-map*] (when (v :bad) (prn "Źle:" k (decomp-map k) (map decomp-map (decomp-map k)) v)))
    (def pri-map pri-map*)
    (def rev-pri-map (into {} (->> decomp-map
                                   keys
                                   (map (juxt identity
                                              (fn [sym] (filter #((pri-map* %) sym) heisig-set)))))))))

(defn main- [args]
  
  )

