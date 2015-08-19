(ns problems)

(defn split-a-sequence [value collection]
  (vector
    (take value collection)
    (drop value collection)))

(def advanced-destructuring
  [1 2 [3 4 5] [1 2 3 4 5]])

(defn map-construction [x y]
  (->> (map vector x y)
       (flatten)
       (apply hash-map)))

(defn comparisons [less-than x y]
  (if (less-than x y) :lt (if (less-than y x) :gt :eq)))

(defn greatest-common-divisor [x y]
  (->> (range (min x y))
       (map inc)
       (filter #(and (= 0 (mod x %)) (= 0 (mod y %))))
       (apply max)))

(defn a-half-truth [& args]
  (= #{true false} (set args)))

(def intro-to-destructuring [2 4])

(defn drop-every-nth-item [collection value]
  (->> (partition-all value collection)
       (map #(take (dec value) %))
       (reduce concat)))

(defn pack-a-sequence [collection]
  (partition-by identity collection))

(defn interpose-a-sequence [interposer collection]
  (->> (interleave collection (repeat interposer))
       (drop-last)))

(def intro-to-iterate '(1 4 7 10 13))

(def contain-yourself 4)

(defn last-element [collection]
  (first (reverse collection)))

(defn compress-a-sequence [xs]
  (map first (partition-by #(identity %) xs)))

(defn interleave-two-sequences [xs ys]
  (flatten (map vector xs ys)))

(defn factorial-fun [x]
  (->> (range x)
       (rest)
       (reduce * x)))

(defn flatten-a-sequence [xs]
  (reduce (fn flattenize [x y]
            (if (coll? y)
              (reduce flattenize x y)
              (conj x y)))
          []
          xs))

(defn implement-range [x y]
  (take (- y x) (iterate inc x)))

(def intro-to-some 6)

(defn duplicate-a-sequence [xs]
  (apply concat (map #(take 2 (repeat %)) xs)))

(defn get-the-caps [x]
  (apply str (re-seq #"[A-Z]" x)))

(defn maximum-value [& xs]
  (last (sort xs)))

(defn fibonacci-sequence [n]
  (loop [fib [1 1]]
    (if (< (count fib) n)
      (recur (conj fib (reduce + (take-last 2 fib))))
      fib)))

(defn palindrome-detector [xs]
  (= (sequence xs) (reverse xs)))

(defn reverse-a-sequence [xs]
  (loop [in xs
         out []]
    (if (not-empty in)
      (recur
        (drop-last in)
        (conj out (last in)))
      out)))

(defn find-the-odd-numbers [xs]
  (filter (fn [x] (= 1(mod x 2))) xs))

(defn sum-it-all-up [xs] (reduce + xs))

(defn count-a-sequence [xs]
  (->> (map vector xs (range))
       (take-last 1)
       (flatten)
       (take-last 1)
       (first)
       (inc)))

(defn nth-element [xs x]
  (first (drop x xs)))

(defn penultimate-element [xs]
  (first (take-last 2 xs)))

(defn set-intersection [xs ys]
  (->> (filter xs ys)
       (set)))

(defn reimplement-iterate [f x]
  (cons x (lazy-seq (reimplement-iterate f (f x)))))

(defn simple-closures [n]
  (fn [x]
    (->> (repeat x)
         (take n)
         (apply *))))

(defn product-digits [x y]
  (->> (str (* x y))
       (sequence)
       (map str)
       (map read-string)))

(defn cartesian-product [xs ys]
  (->> (map #(map vector xs (repeat %)) ys)
       (apply concat)
       (set)))

(defn dot-product [xs ys]
  (reduce + (map * xs ys)))

(defn least-common-multiple [x & xs]
  (->> (iterate (partial + x) x)
       (filter (fn [y] (every? #(zero? (rem y %)) xs)))
       (first)))

(defn to-tree-or-not-to-tree [[_ & children :as node]]
  (and (= 3 (count node))
       (every? #(if (coll? %)
                  (to-tree-or-not-to-tree %)
                  (nil? %))
               children)))

(defn beauty-is-symmetry [[_ l r]]
  (letfn [(mirror [t]
            (when-let [[v l r] t]
              [v (mirror r) (mirror l)]))]
    (= l (mirror r))))

(defn group-a-sequence [f xs]
  (group-by f xs)
  #_(->> (map hash-map (map f xs) xs)
         (apply merge-with vector)))

(defn read-a-binary-number [xs]
  (->> (reverse (seq xs))
       (map #(Integer/parseInt (str %)))
       (map-indexed #(bit-shift-left %2 %1))
       (reduce +)))

(def sequential-destructuring-2 3)

(defn symmetric-differences [xs ys]
  (clojure.set/union
    (clojure.set/difference xs ys)
    (clojure.set/difference ys xs)))

(defn pascals-triangle [n]
  (if (= n 1)
    '(1)
    (let [xs (concat '(0) (pascals-triangle (dec n)) '(0))]
      (->> (interleave xs (rest xs))
           (partition 2)
           (map (partial reduce +))))))

(defn pairwise-disjoint-sets [xs]
  (->> (apply concat xs)
       (apply distinct?))
  #_(letfn [(disjoint? [xs ys]
              (every? false? (map (partial contains? ys) xs)))
            (distributor [f xs]
              (when-not (empty? xs)
                (concat
                  (map #(f (first xs) %) (rest xs))
                  (distributor f (rest xs)))))]
      (every? true? (distributor disjoint? xs))))

(defn inflix-calculator [& xs]
  (->> (partition-all 2 (rest xs))
       (reduce #((first %2) %1 (second %2)) (first xs))))

(defn sum-of-square-of-digits [xs]
  (letfn [(squared-digits [x]
            (->> (seq (str x))
                 (map #(Integer/parseInt (str %)))
                 (map #(* % %))
                 (reduce +)))]
    (count (filter #(> (squared-digits %) %) xs))))

(defn indexing-sequences [xs]
  (map vector xs (range)))

(defn reimplement-map [f xs]
  (rest (reductions #(f %2) nil xs))
  #_(lazy-seq
      (when-let [[x & s] (seq xs)]
        (cons (f x) (reimplement-map f s)))))

(defn trees-into-tables [xs]
  (into {} (for [[k v] xs [v-k v-v] v] [[k v-k] v-v])

        #_(for [[k v] xs]
            (->> (map #(cons k %) v)
                 (map #(hash-map (vec (drop-last %)) (last %)))
                 (apply merge)))))

(defn recognize-playing-cards [[s r]]
  (let [suits {\H :heart, \D :diamond, \C :club, \S :spade}
        ranks (zipmap "23456789TJQKA" (range 13))]
    {:suit (get suits s), :rank (get ranks r)}))

(defn pascals-trapezoid [xs]
  (iterate #(vec (map +' (concat [0] %) (concat % [0]))) xs))

(defn prime-numbers [n]
  (->> (range)
       (drop 2)
       (filter (fn [x] (every? #(pos? (rem x %)) (range 2 x))))
       (take n)))

(defn rotate-a-sequence [n xs]
  (let [r (mod n (count xs))]
    (concat (drop r xs) (take r xs))))

(defn juxtaposition [& fs]
  (fn [& xs]
    (for [f fs]
      (apply f xs))))

(defn filter-perfect-squares [s]
  (->>
    (clojure.string/split s #",")
    (map #(Integer/parseInt %))
    (filter #(= 0.0 (mod (Math/sqrt %) 1)))
    (interpose ",")
    (apply str)))

(defn perfect-numbers [x]
  (->>
    (range 1 x)
    (filter #(zero? (mod x %)))
    (reduce +)
    (= x)))

(defn flipping-out [f]
  (fn [x y]
    (f y x)))

(defn reverse-interleave [xs n]
  (apply map vector (partition n xs)))

(defn split-by-type [xs]
  (vals (group-by type xs)))

(defn count-occurences [xs]
  (let [m (group-by identity xs)]
    (zipmap (keys m) (map count (vals m)))))

(defn find-distinct-items [xs]
  (->>
    (group-by identity xs)
    (keys)
    (sort-by #(.indexOf xs %))))

(defn function-composition [& fs]
  (fn [& xs]
    (first (reduce #(vector (apply %2 %1)) xs (reverse fs)))))

(defn partition-a-sequence [x xs]
  (for [k (range (quot (count xs) x))
        :let [n (* k x)]]
    (take x (drop n xs))))

(defn word-sorting [s]
  (sort-by #(clojure.string/lower-case %) (re-seq #"\w+" s)))

(defn black-box-testing [xs]
  (let [x (flatten (seq (conj xs [:x :x] [:x :x] [:x :y])))
        nx (count x)
        y (flatten (seq xs))
        ny (count y)]
    (cond
      (= (- nx 2) ny) :map
      (= (- nx 4) ny) :set
      (not= (last x) :y) :list
      (= (last x) :y) :vector)))

(def intro-to-trampoline
  (letfn
    [(foo [x y] #(bar (conj x y) y))
     (bar [x y] (if (> (last x) 10)
                  x
                  #(foo x (+ 2 y))))]
    (trampoline foo [] 1)))

(defn anagram-finder [xs]
  (->> (vals (group-by sort xs))
       (filter #(> (count %) 1))
       (map set)
       (set)))

(defn sequence-reductions
  ([f xs]
   (sequence-reductions f (first xs) (rest xs)))
  ([f x xs]
   (lazy-seq
     (cons x
           (if (empty? xs) [] (sequence-reductions f (f x (first xs)) (rest xs)))))))

(defn merge-with-a-function [f & xs]
  (let [ys (group-by first (mapcat seq xs))]
    (->> (vals ys)
         (map #(map second %))
         (map #(reduce f %))
         (zipmap (keys ys)))))

(defn into-camel-case [xs]
  (let [split-xs (clojure.string/split xs #"-")]
    (->> (rest split-xs)
         (map clojure.string/capitalize)
         (cons (first split-xs))
         (apply str))))

(defn happy-numbers [x]
  (letfn [(happy [xs]
            (apply + (for [i (str xs)]
                       (#(* % %) (Integer/parseInt (str i))))))
          (happy? [s x]
            (cond
              (= x 1) true
              (contains? s x) false
              :else (recur (conj s x) (happy x))))]
    (happy? #{} x)))

(defn eulers-totient-function [x]
  (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
    (->> (map #(gcd % x) (range 1 (inc x)))
      (filter #{1})
      (count))))

(defn power-set [xs]
  (let [n (count xs)
        range-combinations (range (Math/pow 2 n))
        decimal->binary (partial clojure.pprint/cl-format nil "~v,'0B" n)]
    (set (for [y (map decimal->binary range-combinations)]
           (->> (map vector y xs)
                (filter #(= \1 (first %)))
                (map #(second %))
                (set))))))

(defn the-balance-of-n [n]
  (let [seq-n (map int (str n))
        halve #(int (/ % 2))
        len (count seq-n)
        half-len (halve len)]
    (->> [(take half-len seq-n) (take-last half-len seq-n)]
         (map #(reduce + %))
         (apply =))))

(defn digits-and-bases [n b]
  (if (< n b) [n] (conj (digits-and-bases (quot n b) b) (mod n b))))

(defn identify-keys-and-values [[x & xs]]
  (if x
    (assoc
      (identify-keys-and-values (drop-while number? xs))
      x
      (take-while number? xs))
    {}))

(defn palindromic-numbers [k]
  (let [sequify (fn [n] (->> n str seq (map str) (map read-string)))
        indices (fn [n] (-> n sequify count (/ 2) int range))
        mirrorize (fn [n i]
                    (let [seq-n (sequify n)
                          a (nth seq-n i)
                          b (nth (reverse seq-n) i)
                          factor (reduce * 1 (repeat i 10))
                          offset (mod (- a b) 10)]
                      (+ n (* offset factor))))
        palindromize (fn [n] (reduce #(mirrorize %1 %2) n (indices n)))
        next-palindrome (fn [n] (-> (palindromize (inc n)) (palindromize)))]
    (rest (iterate next-palindrome (dec k)))))

(defn balanced-primes [n]
  (let [mean #(/ (+ %1 %2) 2)
        prime? (fn [n]
                 (and
                   (> n 1)
                   (->> (range 2 n)
                        (map #(mod n %))
                        (not-any? zero?))))
        first-prime #(first (filter prime? %))
        next-prime (fn [n] ( first-prime (map #(+ % n 1) (range))))
        prev-prime #(first-prime (range (dec %) 0 -1))]
    (and (prime? n) (> n 2) (= n (mean (prev-prime n) (next-prime n))))))
