; EASY

; split-a-sequence
; Special Restrictions - split-at

(defn split-a-sequence [value collection]
  (vector
    (take value collection)
    (drop value collection)))

(split-a-sequence 3 [1 2 3 4 5 6])

(= (split-a-sequence 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
(= (split-a-sequence 1 [:a :b :c :d]) [[:a] [:b :c :d]])
(= (split-a-sequence 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])


; advanced-destructuring

(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] [1 2 3 4 5]] [a b c d]))

; map-construction
; Special Restrictions - zipmap

(defn map-construction [x y]
  (->> (map vector x y)
       (flatten)
       (apply hash-map)))

(= (map-construction [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
(= (map-construction [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
(= (map-construction [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})


; comparisons

(defn comparisons [less-than x y]
  (if (less-than x y) :lt (if (less-than y x) :gt :eq)))

(= :gt (comparisons < 5 1))
(= :eq (comparisons (fn [x y] (< (count x) (count y))) "pear" "plum"))
(= :lt (comparisons (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
(= :gt (comparisons > 0 2))


; greatest-common-divisor

(defn greatest-common-divisor [x y]
  (->> (range (min x y))
       (map inc)
       (filter #(and (= 0 (mod x %)) (= 0 (mod y %))))
       (apply max)))

(= (greatest-common-divisor 2 4) 2)
(= (greatest-common-divisor 10 5) 5)
(= (greatest-common-divisor 5 7) 1)
(= (greatest-common-divisor 1023 858) 33)


; a-half-truth

; Write a function which takes a variable number of booleans.
; Your function should return true if some of the parameters are true,
; but not all of the parameters are true.
; Otherwise your function should return false.

(defn a-half-truth [& args]
  (= #{true false} (set args)))

(= false (a-half-truth false false))
(= true (a-half-truth true false))
(= false (a-half-truth true))
(= true (a-half-truth false true false))
(= false (a-half-truth true true true))
(= true (a-half-truth true true true false))


; intro-to-destructuring

; Let bindings and function parameter lists support destructuring.

(= [2 4] (let [[a b c d e f g] (range)] [c e]))


; drop-every-nth-item

; Write a function which drops every Nth item from a sequence.

(defn drop-every-nth-item [collection value]
  (->> (partition-all value collection)
       (map #(take (dec value) %))
       (reduce concat)))

(= (drop-every-nth-item [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
(= (drop-every-nth-item [:a :b :c :d :e :f] 2) [:a :c :e])
(= (drop-every-nth-item [1 2 3 4 5 6] 4) [1 2 3 5 6])


; pack-a-sequence

; Write a function which packs consecutive duplicates into sub-lists.

(defn pack-a-sequence [collection]
  (partition-by identity collection))

(= (pack-a-sequence [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
(= (pack-a-sequence [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
(= (pack-a-sequence [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))


; interpose-a-sequence

; Write a function which separates the items of a sequence by an arbitrary value.
; Special Restrictions - interpose

(defn interpose-a-sequence [interposer collection]
  (->> (interleave collection (repeat interposer))
       (drop-last)))

(= (interpose-a-sequence 0 [1 2 3]) [1 0 2 0 3])
(= (apply str (interpose-a-sequence ", " ["one" "two" "three"])) "one, two, three")
(= (interpose-a-sequence :z [:a :b :c :d]) [:a :z :b :z :c :z :d])


; intro-to-iterate

; The iterate function can be used to produce an infinite lazy sequence.

(def intro-to-iterate '(1 4 7 10 13))

(= intro-to-iterate (take 5 (iterate #(+ 3 %) 1)))

; contain-yourself

; The contains? function checks if a KEY is present in a given collection.
; This often leads beginner clojurians to use it incorrectly with numerically indexed collections like vectors and lists.

(def contain-yourself 4)

(contains? #{4 5 6} contain-yourself)
(contains? [1 1 1 1 1] contain-yourself)
(contains? {4 :a 2 :b} contain-yourself)

; last-element

; Write a function which returns the last element in a sequence.
; Special Restrictions - last

(defn last-element [collection]
  (first (reverse collection)))

(last-element [1 2 3 4 5])

(= (last-element [1 2 3 4 5]) 5)
(= (last-element '(5 4 3)) 3)
(= (last-element ["b" "c" "d"]) "d")


; compress-a-sequence

; Write a function which removes consecutive duplicates from a sequence.

(defn compress-a-sequence [xs]
  (map first (partition-by #(identity %) xs)))

(= (apply str (compress-a-sequence "Leeeeeerrroyyy")) "Leroy")
(= (compress-a-sequence [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
(= (compress-a-sequence [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))


; interleave-two-sequences

; Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc.
; Special Restrictions - interleave

(defn interleave-two-sequences [xs ys]
  (flatten (map vector xs ys)))

(= (interleave-two-sequences [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
(= (interleave-two-sequences [1 2] [3 4 5 6]) '(1 3 2 4))
(= (interleave-two-sequences [1 2 3 4] [5]) [1 5])
(= (interleave-two-sequences [30 20] [25 15]) [30 25 20 15])


; factorial-fun

; Write a function which calculates factorials.

(defn factorial-fun [x]
  (->> (range x)
       (rest)
       (reduce * x)))

(= (factorial-fun 1) 1)
(= (factorial-fun 3) 6)
(= (factorial-fun 5) 120)
(= (factorial-fun 8) 40320)


; flatten-a-sequence

; Write a function which flattens a sequence.
; Special Restrictions - flatten

(defn flatten-a-sequence [xs]
  (reduce (fn flattenize [x y]
            (if (coll? y)
                (reduce flattenize x y)
                (conj x y)))
          []
          xs))

(flatten-a-sequence '((1 2) 3 [4 [5 6]]))

(= (flatten-a-sequence '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
(= (flatten-a-sequence ["a" ["b"] "c"]) '("a" "b" "c"))
(= (flatten-a-sequence '((((:a))))) '(:a))


; implement-range

; Write a function which creates a list of all integers in a given range.
; Special restrictions - range

(defn implement-range [x y]
  (take (- y x) (iterate inc x)))

(= (implement-range 1 4) '(1 2 3))
(= (implement-range -2 2) '(-2 -1 0 1))
(= (implement-range 5 8) '(5 6 7))

; intro-to-some

; The some function takes a predicate function and a collection.
; It returns the first logical true value of (predicate x) where x is an item in the collection.

(def intro-to-some 6)

(= intro-to-some (some #{2 7 6} [5 6 7 8]))
(= intro-to-some (some #(when (even? %) %) [5 6 7 8]))


; duplicate-a-sequence

; Write a function which duplicates each element of a sequence.

(defn duplicate-a-sequence [xs]
  (apply concat (map #(take 2 (repeat %)) xs)))

(= (duplicate-a-sequence [1 2 3]) '(1 1 2 2 3 3))
(= (duplicate-a-sequence [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
(= (duplicate-a-sequence [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))



; get-the-caps

; Write a function which takes a string and returns a new string containing only the capital letters.

(defn get-the-caps [x]
  (apply str (re-seq #"[A-Z]" x)))

(= (get-the-caps "HeLlO, WoRlD!") "HLOWRD")
(empty? (get-the-caps "nothing"))
(= (get-the-caps "$#A(*&987Zf") "AZ")


; maximum-value

; Write a function which takes a variable number of parameters and returns the maximum value.
; Special Restrictions - max, max-key

(defn maximum-value [& xs]
  (last (sort xs)))

(= (maximum-value 1 8 3 4) 8)
(= (maximum-value 30 20) 30)
(= (maximum-value 45 67 11) 67)


; fibonacci-sequence

; Write a function which returns the first X fibonacci numbers.

(defn fibonacci-sequence [n]
  (loop [fib [1 1]]
    (if (< (count fib) n)
        (recur (conj fib (reduce + (take-last 2 fib))))
        fib)))

(= (fibonacci-sequence 3) '(1 1 2))
(= (fibonacci-sequence 6) '(1 1 2 3 5 8))
(= (fibonacci-sequence 8) '(1 1 2 3 5 8 13 21))


; palindrome-detector

; Write a function which returns true if the given sequence is a palindrome.
; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)

(defn palindrome-detector [xs]
  (= (sequence xs) (reverse xs)))


(false? (palindrome-detector '(1 2 3 4 5)))
(true? (palindrome-detector "racecar"))
(true? (palindrome-detector [:foo :bar :foo]))
(true? (palindrome-detector '(1 1 3 3 1 1)))
(false? (palindrome-detector '(:a :b :c)))


; reverse-a-sequence

; Write a function which reverses a sequence.
; Special Restrictions - reverse, rseq

(defn reverse-a-sequence [xs]
  (loop [in xs
         out []]
    (if (not-empty in)
        (recur
          (drop-last in)
          (conj out (last in)))
          out)))

(= (reverse-a-sequence [1 2 3 4 5]) [5 4 3 2 1])
(= (reverse-a-sequence (sorted-set 5 7 2 7)) '(7 5 2))
(= (reverse-a-sequence [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])


; find-the-odd-numbers

; Write a function which returns only the odd numbers from a sequence.

(defn find-the-odd-numbers [xs]
  (filter (fn [x] (= 1(mod x 2))) xs))

(= (find-the-odd-numbers #{1 2 3 4 5}) '(1 3 5))
(= (find-the-odd-numbers [4 2 1 6]) '(1))
(= (find-the-odd-numbers [2 2 4 6]) '())
(= (find-the-odd-numbers [1 1 1 3]) '(1 1 1 3))


; sum-it-all-up

; Write a function which returns the sum of a sequence of numbers.

(defn sum-it-all-up [xs] (reduce + xs))

(= (sum-it-all-up [1 2 3]) 6)
(= (sum-it-all-up (list 0 -2 5 5)) 8)
(= (sum-it-all-up #{4 2 1}) 7)
(= (sum-it-all-up '(0 0 -1)) -1)
(= (sum-it-all-up '(1 10 3)) 14)


; count-a-sequence

; Write a function which returns the total number of elements in a sequence.
; Special Restrictions - count

(defn count-a-sequence [xs]
  (->> (map vector xs (range))
       (take-last 1)
       (flatten)
       (take-last 1)
       (first)
       (inc)))

(= (count-a-sequence '(1 2 3 3 1)) 5)
(= (count-a-sequence "Hello World") 11)
(= (count-a-sequence [[1 2] [3 4] [5 6]]) 3)
(= (count-a-sequence '(13)) 1)
(= (count-a-sequence '(:a :b :c)) 3)


; nth-element


; Write a function which returns the Nth element from a sequence.
; Special Restrictions - nth

(defn nth-element [xs x]
  (first (drop x xs)))

(nth-element '(4 5 6 7) 2)

(= (nth-element '(4 5 6 7) 2) 6)
(= (nth-element [:a :b :c] 0) :a)
(= (nth-element [1 2 3 4] 1) 2)
(= (nth-element '([1 2] [3 4] [5 6]) 2) [5 6])


; penultimate-element

; Write a function which returns the second to last element from a sequence.

(defn penultimate-element [xs]
  (first (take-last 2 xs)))

(= (penultimate-element (list 1 2 3 4 5)) 4)
(= (penultimate-element ["a" "b" "c"]) "b")
(= (penultimate-element [[1 2] [3 4]]) [1 2])


; set-intersection

; Write a function which returns the intersection of two sets.
; The intersection is the sub-set of items that each set has in common.
; Special Restrictions - intersection

(defn set-intersection [xs ys]
  (->> (filter xs ys)
       (set)))

(set-intersection #{0 1 2 3} #{2 3 4 5})

(= (set-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3})
(= (set-intersection #{0 1 2} #{3 4 5}) #{})
(= (set-intersection #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})


; reimplement-iterate

; Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
; Special Restrictions - iterate

(defn reimplement-iterate [f x]
  (cons x (lazy-seq (reimplement-iterate f (f x)))))

(= (take 5 (reimplement-iterate #(* 2 %) 1)) [1 2 4 8 16])
(= (take 100 (reimplement-iterate inc 0)) (take 100 (range)))
(= (take 9 (reimplement-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))


; simple-closures

; Lexical scope and first-class functions are two of the most basic building blocks of a
; functional language like Clojure. When you combine the two together, you get something
; very powerful called lexical closures. With these, you can exercise a great deal of control
; over the lifetime of your local bindings, saving their values for use later, long after the
; code you're running now has finished. It can be hard to follow in the abstract,
; so let's build a simple closure. Given a positive integer n, return a function (f x)
; which computes x^n. Observe that the effect of this is to preserve the value of n for use
; outside the scope in which it is defined.

(defn simple-closures [n]
  (fn [x]
    (->> (repeat x)
         (take n)
         (apply *))))

(= 256 ((simple-closures 2) 16),
       ((simple-closures 8) 2))
(= [1 8 27 64] (map (simple-closures 3) [1 2 3 4]))
(= [1 2 4 8 16] (map #((simple-closures %) 2) [0 1 2 3 4]))


; product-digits

; Write a function which multiplies two numbers and returns the result as a sequence of its digits.

(defn product-digits [x y]
  (->> (str (* x y))
       (sequence)
       (map str)
       (map read-string)))

(= (product-digits 1 1) [1])
(= (product-digits 99 9) [8 9 1])
(= (product-digits 999 99) [9 8 9 0 1])


; cartesian-product

; Write a function which calculates the Cartesian product of two sets.

(defn cartesian-product [xs ys]
  (->> (map #(map vector xs (repeat %)) ys)
       (apply concat)
       (set)))

(= (cartesian-product #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
   #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
     ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
     ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})
(= (cartesian-product #{1 2 3} #{4 5})
   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
(= 300 (count (cartesian-product (into #{} (range 10))
                  (into #{} (range 30)))))


; dot-product

; Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same length.

(defn dot-product [xs ys]
  (reduce + (map * xs ys)))

(= 0 (dot-product [0 1 0] [1 0 0]))
(= 3 (dot-product [1 1 1] [1 1 1]))
(= 32 (dot-product [1 2 3] [4 5 6]))
(= 256 (dot-product [2 5 6] [100 10 1]))


; least-common-multiple

; Write a function which calculates the least common multiple. Your function should accept
; a variable number of positive integers or ratios.

(defn least-common-multiple [x & xs]
  (->> (iterate (partial + x) x)
       (filter (fn [y] (every? #(zero? (rem y %)) xs)))
       (first)))

(least-common-multiple 5 3 7)

(== (least-common-multiple 2 3) 6)
(== (least-common-multiple 5 3 7) 105)
(== (least-common-multiple 1/3 2/5) 2)
(== (least-common-multiple 3/4 1/6) 3/2)
(== (least-common-multiple 7 5/7 2 3/5) 210)


; to-tree-or-not-to-tree

; Write a predicate which checks whether or not a given sequence represents a binary tree.
; Each node in the tree must have a value, a left child, and a right child.

(defn to-tree-or-not-to-tree [[_ & children :as node]]
  (and (= 3 (count node))
       (every? #(if (coll? %)
                    (to-tree-or-not-to-tree %)
                    (nil? %))
               children)))

(= (to-tree-or-not-to-tree '(:a (:b nil nil) nil))
   true)
(= (to-tree-or-not-to-tree '(:a (:b nil nil)))
   false)
(= (to-tree-or-not-to-tree [1 nil [2 [3 nil nil] [4 nil nil]]])
   true)
(= (to-tree-or-not-to-tree [1 [2 nil nil] [3 nil nil] [4 nil nil]])
   false)
(= (to-tree-or-not-to-tree [1 [2 [3 [4 nil nil] nil] nil] nil])
   true)
(= (to-tree-or-not-to-tree [1 [2 [3 [4 false nil] nil] nil] nil])
   false)
(= (to-tree-or-not-to-tree '(:a nil ()))
   false)


; beauty-is-symmetry

; Let us define a binary tree as "symmetric" if the left half of the tree is the
; mirror image of the right half of the tree. Write a predicate to determine whether
; or not a given binary tree is symmetric. (see To Tree, or not to Tree for a reminder
; on the tree representation we're using).

(defn beauty-is-symmetry [[_ l r]]
  (letfn [(mirror [t]
            (when-let [[v l r] t]
              [v (mirror r) (mirror l)]))]
    (= l (mirror r))))

(beauty-is-symmetry '(:a (:b [5 nil nil] nil) (:b nil [5 nil nil])))

(= (beauty-is-symmetry '(:a (:b nil nil) (:b nil nil))) true)
(= (beauty-is-symmetry '(:a (:b nil nil) nil)) false)
(= (beauty-is-symmetry '(:a (:b nil nil) (:c nil nil))) false)
(= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true)
(= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
   false)
(= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                          [2 [3 nil [4 [6 nil nil] nil]] nil]])
   false)


; group-a-sequence

; Given a function f and a sequence s, write a function which returns a map.
; The keys should be the values of f applied to each item in s.
; The value at each key should be a vector of corresponding items in the order they appear in s.
; Special Restriction - group-by

(defn group-a-sequence [f xs]
  (->> (map hash-map (map f xs) xs)
       (apply merge-with vector)))

(= (group-a-sequence #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
(= (group-a-sequence #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
(= (group-a-sequence count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})


; read-a-binary-number

; Convert a binary number, provided in the form of a string, to its numerical value.

(defn read-a-binary-number [xs]
  (->> (reverse (seq xs))
       (map #(Integer/parseInt (str %)))
       (map-indexed #(bit-shift-left %2 %1))
       (reduce +)))

(= 0     (read-a-binary-number "0"))
(= 7     (read-a-binary-number "111"))
(= 8     (read-a-binary-number "1000"))
(= 9     (read-a-binary-number "1001"))
(= 255   (read-a-binary-number "11111111"))
(= 1365  (read-a-binary-number "10101010101"))
(= 65535 (read-a-binary-number "1111111111111111"))


; intro-to-destructuring-2


; Sequential destructuring allows you to bind symbols to parts of sequential things
; (vectors, lists, seqs, etc.): (let [bindings* ] exprs*) Complete the bindings
; so all let-parts evaluate to 3.

(= 3
  (let [[f args] [+ (range 3)]] (apply f args))
  (let [[[f args] b] [[+ 1] 2]] (f args b))
  (let [[f args] [inc 2]] (f args)))


; symmetric-differences

; Write a function which returns the symmetric difference of two sets.
; The symmetric difference is the set of items belonging to one but not both of the two sets.

(defn symmetric-differences [xs ys]
  (clojure.set/union
    (clojure.set/difference xs ys)
    (clojure.set/difference ys xs)))

(symmetric-differences #{1 2 3 4 5 6} #{1 3 5 7})

(= (symmetric-differences #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
(= (symmetric-differences #{:a :b :c} #{}) #{:a :b :c})
(= (symmetric-differences #{} #{4 5 6}) #{4 5 6})
(= (symmetric-differences #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})


; pascals-triangle

; Pascal's triangle is a triangle of numbers computed using the following rules:
; - The first row is 1.
; - Each successive row is computed by adding together
; adjacent numbers in the row above, and adding a 1 to the beginning and end of the row.
; Write a function which returns the nth row of Pascal's Triangle.

(defn pascals-triangle [n]
  (if (= n 1)
      '(1)
      (let [xs (concat '(0) (pascals-triangle (dec n)) '(0))]
        (->> (interleave xs (rest xs))
             (partition 2)
             (map (partial reduce +))))))

(= (pascals-triangle 1) [1])
(= (map pascals-triangle (range 1 6))
   [     [1]
        [1 1]
       [1 2 1]
      [1 3 3 1]
     [1 4 6 4 1]])
(= (pascals-triangle 11)
   [1 10 45 120 210 252 210 120 45 10 1])


; pairwise-disjoint-sets

; Given a set of sets, create a function which returns true if no two of those sets have
; any elements in common and false otherwise. Some of the test cases are a bit tricky,
; so pay a little more attention to them.
; Such sets are usually called pairwise disjoint or mutually disjoint.

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

(= (pairwise-disjoint-sets #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
   true)
(= (pairwise-disjoint-sets #{#{:a :b :c :d :e}
         #{:a :b :c :d}
         #{:a :b :c}
         #{:a :b}
         #{:a}})
   false)
(= (pairwise-disjoint-sets #{#{[1 2 3] [4 5]}
         #{[1 2] [3 4 5]}
         #{[1] [2] 3 4 5}
         #{1 2 [3 4] [5]}})
   true)
(= (pairwise-disjoint-sets #{#{'a 'b}
         #{'c 'd 'e}
         #{'f 'g 'h 'i}
         #{''a ''c ''f}})
   true)
(= (pairwise-disjoint-sets #{#{'(:x :y :z) '(:x :y) '(:z) '()}
         #{#{:x :y :z} #{:x :y} #{:z} #{}}
         #{'[:x :y :z] [:x :y] [:z] [] {}}})
   false)
(= (pairwise-disjoint-sets #{#{(= "true") false}
         #{:yes :no}
         #{(class 1) 0}
         #{(symbol "true") 'false}
         #{(keyword "yes") ::no}
         #{(class '1) (int \0)}})
   false)
(= (pairwise-disjoint-sets #{#{distinct?}
         #{#(-> %) #(-> %)}
         #{#(-> %) #(-> %) #(-> %)}
         #{#(-> %) #(-> %) #(-> %)}})
   true)
(= (pairwise-disjoint-sets #{#{(#(-> *)) + (quote mapcat) #_ nil}
         #{'+ '* mapcat (comment mapcat)}
         #{(do) set contains? nil?}
         #{, , , #_, , empty?}})
   false)


; inflix-calculator

; Your friend Joe is always whining about Lisps using the prefix notation for math.
; Show him how you could easily write a function that does math using the infix notation.
; Is your favorite language that flexible, Joe?
; Write a function that accepts a variable length mathematical expression consisting of
; numbers and the operations +, -, *, and /. Assume a simple calculator that does not do
; precedence and instead just calculates left to right.

(defn inflix-calculator [& xs]
  (->> (partition-all 2 (rest xs))
       (reduce #((first %2) %1 (second %2)) (first xs))))

(= 7  (inflix-calculator 2 + 5))
(= 42 (inflix-calculator 38 + 48 - 2 / 2))
(= 8  (inflix-calculator 10 / 2 - 1 * 2))
(= 72 (inflix-calculator 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))


; sum-of-square-of-digits

; Write a function which takes a collection of integers as an argument.
; Return the count of how many elements are smaller than the sum of their
; squared component digits. For example: 10 is larger than 1 squared plus 0 squared;
; whereas 15 is smaller than 1 squared plus 5 squared.

(defn sum-of-square-of-digits [xs]
  (letfn [(squared-digits [x]
            (->> (seq (str x))
                 (map #(Integer/parseInt (str %)))
                 (map #(* % %))
                 (reduce +)))]
    (count (filter #(> (squared-digits %) %) xs))))


(= 8 (sum-of-square-of-digits (range 10)))
(= 19 (sum-of-square-of-digits (range 30)))
(= 50 (sum-of-square-of-digits (range 100)))
(= 50 (sum-of-square-of-digits (range 1000)))


; indexing-sequences

; Transform a sequence into a sequence of pairs containing the original elements along
; with their index.

(defn indexing-sequences [xs]
  (map vector xs (range)))

(= (indexing-sequences [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
(= (indexing-sequences [0 1 3]) '((0 0) (1 1) (3 2)))
(= (indexing-sequences [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])


; reimplement-map

; Map is one of the core elements of a functional programming language. Given a function f and
; an input sequence s, return a lazy sequence of (f x) for each element x in s.
; Special Restrictions - map, map-indexed, mapcat, for

(defn reimplement-map [f xs]
  (rest (reductions #(f %2) nil xs))
  #_(lazy-seq
    (when-let [[x & s] (seq xs)]
      (cons (f x) (reimplement-map f s)))))

(= [3 4 5 6 7]
   (reimplement-map inc [2 3 4 5 6]))
(= (repeat 10 nil)
   (reimplement-map (fn [_] nil) (range 10)))

(= [1000000 1000001]
   (->> (reimplement-map inc (range))
        (drop (dec 1000000))
        (take 2)))


; trees-into-tables

; Because Clojure's for macro allows you to "walk" over multiple sequences in a nested fashion,
; it is excellent for transforming all sorts of sequences. If you don't want a sequence as your
; final output (say you want a map), you are often still best-off using for, because you can
; produce a sequence and feed it into a map, for example.
; For this problem, your goal is to "flatten" a map of hashmaps. Each key in your output map
; should be the "path"* that you would have to take in the original map to get to a value,
; so for example {1 {2 3}} should result in {[1 2] 3}. You only need to flatten one
; level of maps: if one of the values is a map, just leave it alone.

; * That is, (get-in original [k1 k2]) should be the same as (get result [k1 k2])

(defn trees-into-tables [xs]
  (into {} (for [[k v] xs [v-k v-v] v] [[k v-k] v-v])

    #_(for [[k v] xs]
        (->> (map #(cons k %) v)
             (map #(hash-map (vec (drop-last %)) (last %)))
             (apply merge)))))

(= (trees-into-tables '{a {p 1, q 2} b {m 3, n 4}})
                      '{[a p] 1, [a q] 2 [b m] 3, [b n] 4})
(= (trees-into-tables '{[1] {a b c d}
         [2] {q r s t u v w x}})
   '{[[1] a] b, [[1] c] d,
     [[2] q] r, [[2] s] t,
     [[2] u] v, [[2] w] x})
(= (trees-into-tables '{m {1 [a b c] 3 nil}})
   '{[m 1] [a b c], [m 3] nil})


; recognize-playing-cards

; A standard American deck of playing cards has four suits - spades, hearts, diamonds, and clubs -
; and thirteen cards in each suit. Two is the lowest rank, followed by other integers up to ten;
; then the jack, queen, king, and ace.
; It's convenient for humans to represent these cards as suit/rank pairs, such as H5 or DQ:
; the heart five and diamond queen respectively. But these forms are not convenient for programmers,
; so to write a card game you need some way to parse an input string into meaningful components.
; For purposes of determining rank, we will define the cardzs to be valued from 0 (the two) to 12
; (the ace)

; Write a function which converts (for example) the string "SJ" into a map of {:suit :spade, :rank 9}.
; A ten will always be represented with the single character "T", rather than the two characters "10".

(defn recognize-playing-cards [[s r]]
  (let [suits {\H :heart, \D :diamond, \C :club, \S :spade}
        ranks (zipmap "23456789TJQKA" (range 13))]
    {:suit (get suits s), :rank (get ranks r)}))

(= {:suit :diamond :rank 10} (recognize-playing-cards "DQ"))
(= {:suit :heart :rank 3} (recognize-playing-cards "H5"))
(= {:suit :club :rank 12} (recognize-playing-cards "CA"))
(= (range 13) (map (comp :rank recognize-playing-cards str)
                   '[S2 S3 S4 S5 S6 S7
                     S8 S9 ST SJ SQ SK SA]))


; pascals-trapezoid

; Write a function that, for any given input vector of numbers, returns an infinite lazy sequence of vectors,
; where each next one is constructed from the previous following the rules used in Pascal's Triangle.
; For example, for [3 1 2], the next row is [3 4 3 2].

; Beware of arithmetic overflow! In clojure (since version 1.3 in 2011), if you use an arithmetic operator
; like + and the result is too large to fit into a 64-bit integer, an exception is thrown. You can use +'
; to indicate that you would rather overflow into Clojure's slower, arbitrary-precision bigint.

(defn pascals-trapezoid [xs]
  (iterate #(vec (map +' (concat [0] %) (concat % [0]))) xs))

(= (second (pascals-trapezoid [2 3 2])) [2 5 5 2])
(= (take 5 (pascals-trapezoid [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
(= (take 2 (pascals-trapezoid [3 1 2])) [[3 1 2] [3 4 3 2]])
(= (take 100 (pascals-trapezoid [2 4 2])) (rest (take 101 (pascals-trapezoid [2 2]))))
