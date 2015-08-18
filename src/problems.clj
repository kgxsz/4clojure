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

; ; interleave-two-sequences
;
; ; Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc.
; ; Special Restrictions - interleave
;
; (defn interleave-two-sequences [xs ys]
;   (flatten (map vector xs ys)))
;
;
;
;
;
;
;
; ; factorial-fun
;
; ; Write a function which calculates factorials.
;
; (defn factorial-fun [x]
;   (->> (range x)
;        (rest)
;        (reduce * x)))
;
;
;
;
;
;
;
; ; flatten-a-sequence
;
; ; Write a function which flattens a sequence.
; ; Special Restrictions - flatten
;
; (defn flatten-a-sequence [xs]
;   (reduce (fn flattenize [x y]
;             (if (coll? y)
;               (reduce flattenize x y)
;               (conj x y)))
;           []
;           xs))
;
;
;
;
;
;
;
; ; implement-range
;
; ; Write a function which creates a list of all integers in a given range.
; ; Special restrictions - range
;
; (defn implement-range [x y]
;   (take (- y x) (iterate inc x)))
;
;
;
;
;
; ; intro-to-some
;
; ; The some function takes a predicate function and a collection.
; ; It returns the first logical true value of (predicate x) where x is an item in the collection.
;
; (def intro-to-some 6)
;
;
;
;
;
; ; duplicate-a-sequence
;
; ; Write a function which duplicates each element of a sequence.
;
; (defn duplicate-a-sequence [xs]
;   (apply concat (map #(take 2 (repeat %)) xs)))
;
;
;
;
;
;
;
; ; get-the-caps
;
; ; Write a function which takes a string and returns a new string containing only the capital letters.
;
; (defn get-the-caps [x]
;   (apply str (re-seq #"[A-Z]" x)))
;
;
;
; ; maximum-value
;
; ; Write a function which takes a variable number of parameters and returns the maximum value.
; ; Special Restrictions - max, max-key
;
; (defn maximum-value [& xs]
;   (last (sort xs)))
;
;
;
;
;
;
; ; fibonacci-sequence
;
; ; Write a function which returns the first X fibonacci numbers.
;
; (defn fibonacci-sequence [n]
;   (loop [fib [1 1]]
;     (if (< (count fib) n)
;       (recur (conj fib (reduce + (take-last 2 fib))))
;       fib)))
;
;
;
;
;
;
; ; palindrome-detector
;
; ; Write a function which returns true if the given sequence is a palindrome.
; ; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
;
; (defn palindrome-detector [xs]
;   (= (sequence xs) (reverse xs)))
;
;
; ; reverse-a-sequence
;
; ; Write a function which reverses a sequence.
; ; Special Restrictions - reverse, rseq
;
; (defn reverse-a-sequence [xs]
;   (loop [in xs
;          out []]
;     (if (not-empty in)
;       (recur
;         (drop-last in)
;         (conj out (last in)))
;       out)))
;
;
;
;
;
;
; ; find-the-odd-numbers
;
; ; Write a function which returns only the odd numbers from a sequence.
;
; (defn find-the-odd-numbers [xs]
;   (filter (fn [x] (= 1(mod x 2))) xs))
;
;
;
;
;
;
;
; ; sum-it-all-up
;
; ; Write a function which returns the sum of a sequence of numbers.
;
; (defn sum-it-all-up [xs] (reduce + xs))
;
;
;
;
;
;
;
;
; ; count-a-sequence
;
; ; Write a function which returns the total number of elements in a sequence.
; ; Special Restrictions - count
;
; (defn count-a-sequence [xs]
;   (->> (map vector xs (range))
;        (take-last 1)
;        (flatten)
;        (take-last 1)
;        (first)
;        (inc)))
;
;
;
;
;
;
;
;
; ; nth-element
;
;
; ; Write a function which returns the Nth element from a sequence.
; ; Special Restrictions - nth
;
; (defn nth-element [xs x]
;   (first (drop x xs)))
;
;
;
;
;
; ; penultimate-element
;
; ; Write a function which returns the second to last element from a sequence.
;
; (defn penultimate-element [xs]
;   (first (take-last 2 xs)))
;
;
;
;
;
;
; ; set-intersection
;
; ; Write a function which returns the intersection of two sets.
; ; The intersection is the sub-set of items that each set has in common.
; ; Special Restrictions - intersection
;
; (defn set-intersection [xs ys]
;   (->> (filter xs ys)
;        (set)))
;
;
;
;
; ; reimplement-iterate
;
; ; Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
; ; Special Restrictions - iterate
;
; (defn reimplement-iterate [f x]
;   (cons x (lazy-seq (reimplement-iterate f (f x)))))
;
;
;
;
;
;
; ; simple-closures
;
; ; Lexical scope and first-class functions are two of the most basic building blocks of a
; ; functional language like Clojure. When you combine the two together, you get something
; ; very powerful called lexical closures. With these, you can exercise a great deal of control
; ; over the lifetime of your local bindings, saving their values for use later, long after the
; ; code you're running now has finished. It can be hard to follow in the abstract,
; ; so let's build a simple closure. Given a positive integer n, return a function (f x)
; ; which computes x^n. Observe that the effect of this is to preserve the value of n for use
; ; outside the scope in which it is defined.
;
; (defn simple-closures [n]
;   (fn [x]
;     (->> (repeat x)
;          (take n)
;          (apply *))))
;
;
; ; product-digits
;
; ; Write a function which multiplies two numbers and returns the result as a sequence of its digits.
;
; (defn product-digits [x y]
;   (->> (str (* x y))
;        (sequence)
;        (map str)
;        (map read-string)))
;
;
;
;
;
;
; ; cartesian-product
;
; ; Write a function which calculates the Cartesian product of two sets.
;
; (defn cartesian-product [xs ys]
;   (->> (map #(map vector xs (repeat %)) ys)
;        (apply concat)
;        (set)))
;
;
; ; Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same length.
;
; (defn dot-product [xs ys]
;   (reduce + (map * xs ys)))
;
;
;
;
;
;
;
; ; least-common-multiple
;
; ; Write a function which calculates the least common multiple. Your function should accept
; ; a variable number of positive integers or ratios.
;
; (defn least-common-multiple [x & xs]
;   (->> (iterate (partial + x) x)
;        (filter (fn [y] (every? #(zero? (rem y %)) xs)))
;        (first)))
;
; ; to-tree-or-not-to-tree
;
; ; Write a predicate which checks whether or not a given sequence represents a binary tree.
; ; Each node in the tree must have a value, a left child, and a right child.
;
; (defn to-tree-or-not-to-tree [[_ & children :as node]]
;   (and (= 3 (count node))
;        (every? #(if (coll? %)
;                   (to-tree-or-not-to-tree %)
;                   (nil? %))
;                children)))
;
;
; ; beauty-is-symmetry
;
; ; Let us define a binary tree as "symmetric" if the left half of the tree is the
; ; mirror image of the right half of the tree. Write a predicate to determine whether
; ; or not a given binary tree is symmetric. (see To Tree, or not to Tree for a reminder
; ; on the tree representation we're using).
;
; (defn beauty-is-symmetry [[_ l r]]
;   (letfn [(mirror [t]
;             (when-let [[v l r] t]
;               [v (mirror r) (mirror l)]))]
;     (= l (mirror r))))
; ; group-a-sequence
;
; ; Given a function f and a sequence s, write a function which returns a map.
; ; The keys should be the values of f applied to each item in s.
; ; The value at each key should be a vector of corresponding items in the order they appear in s.
; ; Special Restriction - group-by
;
; (defn group-a-sequence [f xs]
;   (->> (map hash-map (map f xs) xs)
;        (apply merge-with vector)))
;
;
; ; read-a-binary-number
;
; ; Convert a binary number, provided in the form of a string, to its numerical value.
;
; (defn read-a-binary-number [xs]
;   (->> (reverse (seq xs))
;        (map #(Integer/parseInt (str %)))
;        (map-indexed #(bit-shift-left %2 %1))
;        (reduce +)))
;
;
;
;
;
;
;
;
;
;
; ; intro-to-destructuring-2
;
;
; ; Sequential destructuring allows you to bind symbols to parts of sequential things
; ; (vectors, lists, seqs, etc.): (let [bindings* ] exprs*) Complete the bindings
; ; so all let-parts evaluate to 3.
;
;
;    (let [[f args] [+ (range 3)]] (apply f args))
;    (let [[[f args] b] [[+ 1] 2]] (f args b))
;    (let [[f args] [inc 2]] (f args)))
;
;
; ; symmetric-differences
;
; ; Write a function which returns the symmetric difference of two sets.
; ; The symmetric difference is the set of items belonging to one but not both of the two sets.
;
; (defn symmetric-differences [xs ys]
;   (clojure.set/union
;     (clojure.set/difference xs ys)
;     (clojure.set/difference ys xs)))
;
;
;
;
;
;
;
;
; ; pascals-triangle
;
; ; Pascal's triangle is a triangle of numbers computed using the following rules:
; ; - The first row is 1.
; ; - Each successive row is computed by adding together
; ; adjacent numbers in the row above, and adding a 1 to the beginning and end of the row.
; ; Write a function which returns the nth row of Pascal's Triangle.
;
; (defn pascals-triangle [n]
;   (if (= n 1)
;     '(1)
;     (let [xs (concat '(0) (pascals-triangle (dec n)) '(0))]
;       (->> (interleave xs (rest xs))
;            (partition 2)
;            (map (partial reduce +))))))
;
;
;
; ; pairwise-disjoint-sets
;
; ; Given a set of sets, create a function which returns true if no two of those sets have
; ; any elements in common and false otherwise. Some of the test cases are a bit tricky,
; ; so pay a little more attention to them.
; ; Such sets are usually called pairwise disjoint or mutually disjoint.
;
; (defn pairwise-disjoint-sets [xs]
;   (->> (apply concat xs)
;        (apply distinct?))
;   #_(letfn [(disjoint? [xs ys]
;               (every? false? (map (partial contains? ys) xs)))
;             (distributor [f xs]
;               (when-not (empty? xs)
;                 (concat
;                   (map #(f (first xs) %) (rest xs))
;                   (distributor f (rest xs)))))]
;       (every? true? (distributor disjoint? xs))))
;
; ; inflix-calculator
;
; ; Your friend Joe is always whining about Lisps using the prefix notation for math.
; ; Show him how you could easily write a function that does math using the infix notation.
; ; Is your favorite language that flexible, Joe?
; ; Write a function that accepts a variable length mathematical expression consisting of
; ; numbers and the operations +, -, *, and /. Assume a simple calculator that does not do
; ; precedence and instead just calculates left to right.
;
; (defn inflix-calculator [& xs]
;   (->> (partition-all 2 (rest xs))
;        (reduce #((first %2) %1 (second %2)) (first xs))))
;
;
;
;
;
;
;
; ; sum-of-square-of-digits
;
; ; Write a function which takes a collection of integers as an argument.
; ; Return the count of how many elements are smaller than the sum of their
; ; squared component digits. For example: 10 is larger than 1 squared plus 0 squared;
; ; whereas 15 is smaller than 1 squared plus 5 squared.
;
; (defn sum-of-square-of-digits [xs]
;   (letfn [(squared-digits [x]
;             (->> (seq (str x))
;                  (map #(Integer/parseInt (str %)))
;                  (map #(* % %))
;                  (reduce +)))]
;     (count (filter #(> (squared-digits %) %) xs))))
;
;
;
;
;
;
;
;
; ; indexing-sequences
;
; ; Transform a sequence into a sequence of pairs containing the original elements along
; ; with their index.
;
; (defn indexing-sequences [xs]
;   (map vector xs (range)))
;
;
;
;
;
;
; ; reimplement-map
;
; ; Map is one of the core elements of a functional programming language. Given a function f and
; ; an input sequence s, return a lazy sequence of (f x) for each element x in s.
; ; Special Restrictions - map, map-indexed, mapcat, for
;
; (defn reimplement-map [f xs]
;   (rest (reductions #(f %2) nil xs))
;   #_(lazy-seq
;       (when-let [[x & s] (seq xs)]
;         (cons (f x) (reimplement-map f s)))))
;
;
;
; ; trees-into-tables
;
; ; Because Clojure's for macro allows you to "walk" over multiple sequences in a nested fashion,
; ; it is excellent for transforming all sorts of sequences. If you don't want a sequence as your
; ; final output (say you want a map), you are often still best-off using for, because you can
; ; produce a sequence and feed it into a map, for example.
; ; For this problem, your goal is to "flatten" a map of hashmaps. Each key in your output map
; ; should be the "path"* that you would have to take in the original map to get to a value,
; ; so for example {1 {2 3}} should result in {[1 2] 3}. You only need to flatten one
; ; level of maps: if one of the values is a map, just leave it alone.
;
; ; * That is, (get-in original [k1 k2]) should be the same as (get result [k1 k2])
;
; (defn trees-into-tables [xs]
;   (into {} (for [[k v] xs [v-k v-v] v] [[k v-k] v-v])
;
;         #_(for [[k v] xs]
;             (->> (map #(cons k %) v)
;                  (map #(hash-map (vec (drop-last %)) (last %)))
;                  (apply merge)))))
;
;
; ; recognize-playing-cards
;
; ; A standard American deck of playing cards has four suits - spades, hearts, diamonds, and clubs -
; ; and thirteen cards in each suit. Two is the lowest rank, followed by other integers up to ten;
; ; then the jack, queen, king, and ace.
; ; It's convenient for humans to represent these cards as suit/rank pairs, such as H5 or DQ:
; ; the heart five and diamond queen respectively. But these forms are not convenient for programmers,
; ; so to write a card game you need some way to parse an input string into meaningful components.
; ; For purposes of determining rank, we will define the cardzs to be valued from 0 (the two) to 12
; ; (the ace)
;
; ; Write a function which converts (for example) the string "SJ" into a map of {:suit :spade, :rank 9}.
; ; A ten will always be represented with the single character "T", rather than the two characters "10".
;
; (defn recognize-playing-cards [[s r]]
;   (let [suits {\H :heart, \D :diamond, \C :club, \S :spade}
;         ranks (zipmap "23456789TJQKA" (range 13))]
;     {:suit (get suits s), :rank (get ranks r)}))
;
;
;
;
; ; pascals-trapezoid
;
; ; Write a function that, for any given input vector of numbers, returns an infinite lazy sequence of vectors,
; ; where each next one is constructed from the previous following the rules used in Pascal's Triangle.
; ; For example, for [3 1 2], the next row is [3 4 3 2].
;
; ; Beware of arithmetic overflow! In clojure (since version 1.3 in 2011), if you use an arithmetic operator
; ; like + and the result is too large to fit into a 64-bit integer, an exception is thrown. You can use +'
; ; to indicate that you would rather overflow into Clojure's slower, arbitrary-precision bigint.
;
; (defn pascals-trapezoid [xs]
;   (iterate #(vec (map +' (concat [0] %) (concat % [0]))) xs))
