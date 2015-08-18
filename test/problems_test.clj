(ns problems-test
  (:require [clojure.test :refer :all]
            [problems :refer :all]))

(deftest test-split-a-sequence
  (is (= (split-a-sequence 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
  (is (= (split-a-sequence 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
  (is (= (split-a-sequence 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))

(deftest test-advanced-destructuring
  (is (= advanced-destructuring (let [[a b & c :as d] [1 2 3 4 5]] [a b c d]))))

(deftest test-map-construction
  (is (= (map-construction [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
  (is (= (map-construction [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
  (is (= (map-construction [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

(deftest test-comparisons
  (is (= :gt (comparisons < 5 1)))
  (is (= :eq (comparisons (fn [x y] (< (count x) (count y))) "pear" "plum")))
  (is (= :lt (comparisons (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
  (is (= :gt (comparisons > 0 2))))

(deftest test-greatest-common-divisor
  (is (= (greatest-common-divisor 2 4) 2))
  (is (= (greatest-common-divisor 10 5) 5))
  (is (= (greatest-common-divisor 5 7) 1))
  (is (= (greatest-common-divisor 1023 858) 33)))

(deftest test-a-half-truth
  (is (= false (a-half-truth false false)))
  (is (= true (a-half-truth true false)))
  (is (= false (a-half-truth true)))
  (is (= true (a-half-truth false true false)))
  (is (= false (a-half-truth true true true)))
  (is (= true (a-half-truth true true true false))))

(deftest test-intro-to-destructuring
  (is (= intro-to-destructuring (let [[a b c d e f g] (range)] [c e]))))

(deftest test-drop-every-nth-item
  (is (= (drop-every-nth-item [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
  (is (= (drop-every-nth-item [:a :b :c :d :e :f] 2) [:a :c :e]))
  (is (= (drop-every-nth-item [1 2 3 4 5 6] 4) [1 2 3 5 6])))

(deftest test-pack-a-sequence
  (is (= (pack-a-sequence [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
  (is (= (pack-a-sequence [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
  (is (= (pack-a-sequence [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))))

(deftest test-interpose-a-sequence
  (is (= (interpose-a-sequence 0 [1 2 3]) [1 0 2 0 3]))
  (is (= (apply str (interpose-a-sequence ", " ["one" "two" "three"])) "one, two, three"))
  (is (= (interpose-a-sequence :z [:a :b :c :d]) [:a :z :b :z :c :z :d])))

(deftest test-intro-to-iterate
  (is (= intro-to-iterate (take 5 (iterate #(+ 3 %) 1)))))

(deftest test-contain-yourself
  (= (contains? #{4 5 6} contain-yourself))
  (= (contains? [1 1 1 1 1] contain-yourself))
  (= (contains? {4 :a 2 :b} contain-yourself)))

(deftest test-last-element
  (is (= (last-element [1 2 3 4 5]) 5))
  (is (= (last-element '(5 4 3)) 3))
  (is (= (last-element ["b" "c" "d"]) "d")))

(deftest test-compress-a-sequence
  (is (= (apply str (compress-a-sequence "Leeeeeerrroyyy")) "Leroy"))
  (is (= (compress-a-sequence [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
  (is (= (compress-a-sequence [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))))

;
; ; interleave-two-sequences
;
; ; Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc.
; ; Special Restrictions - interleave
;
; (deftest test-interleave-two-sequences
;   (flatten (map vector xs ys)))
;
; (is (= (interleave-two-sequences [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
; (is (= (interleave-two-sequences [1 2] [3 4 5 6]) '(1 3 2 4)))
; (is (= (interleave-two-sequences [1 2 3 4] [5]) [1 5]))
; (is (= (interleave-two-sequences [30 20] [25 15]) [30 25 20 15]))
;
;
; ; factorial-fun
;
; ; Write a function which calculates factorials.
;
; (deftest test-factorial-fun
;   (->> (range x)
;        (rest)
;        (reduce * x)))
;
; (is (= (factorial-fun 1) 1))
; (is (= (factorial-fun 3) 6))
; (is (= (factorial-fun 5) 120))
; (is (= (factorial-fun 8) 40320))
;
;
; ; flatten-a-sequence
;
; ; Write a function which flattens a sequence.
; ; Special Restrictions - flatten
;
; (deftest test-flatten-a-sequence
;   (reduce (fn flattenize [x y]
;             (if (coll? y)
;                 (reduce flattenize x y)
;                 (conj x y)))
;           []
;           xs))
;
; (flatten-a-sequence '((1 2) 3 [4 [5 6]]))
;
; (is (= (flatten-a-sequence '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
; (is (= (flatten-a-sequence ["a" ["b"] "c"]) '("a" "b" "c")))
; (is (= (flatten-a-sequence '((((:a))))) '(:a)))
;
;
; ; implement-range
;
; ; Write a function which creates a list of all integers in a given range.
; ; Special restrictions - range
;
; (deftest test-implement-range
;   (take (- y x) (iterate inc x)))
;
; (is (= (implement-range 1 4) '(1 2 3)))
; (is (= (implement-range -2 2) '(-2 -1 0 1)))
; (is (= (implement-range 5 8) '(5 6 7)))
;
; ; intro-to-some
;
; ; The some function takes a predicate function and a collection.
; ; It returns the first logical true value of (predicate x) where x is an item in the collection.
;
; (def intro-to-some 6)
;
; (is (= intro-to-some (some #{2 7 6} [5 6 7 8])))
; (is (= intro-to-some (some #(when (even? %) %) [5 6 7 8])))
;
;
; ; duplicate-a-sequence
;
; ; Write a function which duplicates each element of a sequence.
;
; (deftest test-duplicate-a-sequence
;   (apply concat (map #(take 2 (repeat %)) xs)))
;
; (is (= (duplicate-a-sequence [1 2 3]) '(1 1 2 2 3 3)))
; (is (= (duplicate-a-sequence [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
; (is (= (duplicate-a-sequence [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
;
;
;
; ; get-the-caps
;
; ; Write a function which takes a string and returns a new string containing only the capital letters.
;
; (deftest test-get-the-caps
;   (apply str (re-seq #"[A-Z]" x)))
;
; (is (= (get-the-caps "HeLlO, WoRlD!") "HLOWRD"))
; (is (empty? (get-the-caps "nothing")))
; (is (= (get-the-caps "$#A(*&987Zf") "AZ"))
;
;
; ; maximum-value
;
; ; Write a function which takes a variable number of parameters and returns the maximum value.
; ; Special Restrictions - max, max-key
;
; (deftest test-maximum-value
;   (last (sort xs)))
;
; (is (= (maximum-value 1 8 3 4) 8))
; (is (= (maximum-value 30 20) 30))
; (is (= (maximum-value 45 67 11) 67))
;
;
; ; fibonacci-sequence
;
; ; Write a function which returns the first X fibonacci numbers.
;
; (deftest test-fibonacci-sequence
;   (loop [fib [1 1]]
;     (if (< (count fib) n)
;         (recur (conj fib (reduce + (take-last 2 fib))))
;         fib)))
;
; (is (= (fibonacci-sequence 3) '(1 1 2)))
; (is (= (fibonacci-sequence 6) '(1 1 2 3 5 8)))
; (is (= (fibonacci-sequence 8) '(1 1 2 3 5 8 13 21)))
;
;
; ; palindrome-detector
;
; ; Write a function which returns true if the given sequence is a palindrome.
; ; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
;
; (deftest test-palindrome-detector
;   (= (sequence xs) (reverse xs)))
;
;
; (false? (palindrome-detector '(1 2 3 4 5)))
; (true? (palindrome-detector "racecar"))
; (true? (palindrome-detector [:foo :bar :foo]))
; (true? (palindrome-detector '(1 1 3 3 1 1)))
; (false? (palindrome-detector '(:a :b :c)))
;
;
; ; reverse-a-sequence
;
; ; Write a function which reverses a sequence.
; ; Special Restrictions - reverse, rseq
;
; (deftest test-reverse-a-sequence
;   (loop [in xs
;          out []]
;     (if (not-empty in)
;         (recur
;           (drop-last in)
;           (conj out (last in)))
;           out)))
;
; (is (= (reverse-a-sequence [1 2 3 4 5]) [5 4 3 2 1]))
; (is (= (reverse-a-sequence (sorted-set 5 7 2 7)) '(7 5 2)))
; (is (= (reverse-a-sequence [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]]))
;
;
; ; find-the-odd-numbers
;
; ; Write a function which returns only the odd numbers from a sequence.
;
; (deftest test-find-the-odd-numbers
;   (filter (fn [x] (is (= 1(mod x 2))) xs)))
;
; (is (= (find-the-odd-numbers #{1 2 3 4 5}) '(1 3 5)))
; (is (= (find-the-odd-numbers [4 2 1 6]) '(1)))
; (is (= (find-the-odd-numbers [2 2 4 6]) '()))
; (is (= (find-the-odd-numbers [1 1 1 3]) '(1 1 1 3)))
;
;
; ; sum-it-all-up
;
; ; Write a function which returns the sum of a sequence of numbers.
;
; (deftest test-sum-it-all-up
;
; (is (= (sum-it-all-up [1 2 3]) 6))
; (is (= (sum-it-all-up (list 0 -2 5 5)) 8))
; (is (= (sum-it-all-up #{4 2 1}) 7))
; (is (= (sum-it-all-up '(0 0 -1)) -1))
; (is (= (sum-it-all-up '(1 10 3)) 14))
;
;
; ; count-a-sequence
;
; ; Write a function which returns the total number of elements in a sequence.
; ; Special Restrictions - count
;
; (deftest test-count-a-sequence
;   (->> (map vector xs (range))
;        (take-last 1)
;        (flatten)
;        (take-last 1)
;        (first)
;        (inc)))
;
; (is (= (count-a-sequence '(1 2 3 3 1)) 5))
; (is (= (count-a-sequence "Hello World") 11))
; (is (= (count-a-sequence [[1 2] [3 4] [5 6]]) 3))
; (is (= (count-a-sequence '(13)) 1))
; (is (= (count-a-sequence '(:a :b :c)) 3))
;
;
; ; nth-element
;
;
; ; Write a function which returns the Nth element from a sequence.
; ; Special Restrictions - nth
;
; (deftest test-nth-element
;   (first (drop x xs)))
;
; (nth-element '(4 5 6 7) 2)
;
; (is (= (nth-element '(4 5 6 7) 2) 6))
; (is (= (nth-element [:a :b :c] 0) :a))
; (is (= (nth-element [1 2 3 4] 1) 2))
; (is (= (nth-element '([1 2] [3 4] [5 6]) 2) [5 6]))
;
;
; ; penultimate-element
;
; ; Write a function which returns the second to last element from a sequence.
;
; (deftest test-penultimate-element
;   (first (take-last 2 xs)))
;
; (is (= (penultimate-element (list 1 2 3 4 5)) 4))
; (is (= (penultimate-element ["a" "b" "c"]) "b"))
; (is (= (penultimate-element [[1 2] [3 4]]) [1 2]))
;
;
; ; set-intersection
;
; ; Write a function which returns the intersection of two sets.
; ; The intersection is the sub-set of items that each set has in common.
; ; Special Restrictions - intersection
;
; (deftest test-set-intersection
;   (->> (filter xs ys)
;        (set)))
;
; (set-intersection #{0 1 2 3} #{2 3 4 5})
;
; (is (= (set-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3}))
; (is (= (set-intersection #{0 1 2} #{3 4 5}) #{}))
; (is (= (set-intersection #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))
;
;
; ; reimplement-iterate
;
; ; Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
; ; Special Restrictions - iterate
;
; (deftest test-reimplement-iterate
;   (cons x (lazy-seq (reimplement-iterate f (f x)))))
;
; (is (= (take 5 (reimplement-iterate #(* 2 %) 1)) [1 2 4 8 16]))
; (is (= (take 100 (reimplement-iterate inc 0)) (take 100 (range))))
; (is (= (take 9 (reimplement-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3]))))
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
; (deftest test-simple-closures
;   (fn [x]
;     (->> (repeat x)
;          (take n)
;          (apply *))))
;
; (is (= 256 ((simple-closures 2) 16),
;        ((simple-closures 8) 2)))
; (is (= [1 8 27 64] (map (simple-closures 3) [1 2 3 4])))
; (is (= [1 2 4 8 16] (map #((simple-closures %) 2) [0 1 2 3 4])))
;
;
; ; product-digits
;
; ; Write a function which multiplies two numbers and returns the result as a sequence of its digits.
;
; (deftest test-product-digits
;   (->> (str (* x y))
;        (sequence)
;        (map str)
;        (map read-string)))
;
; (is (= (product-digits 1 1) [1]))
; (is (= (product-digits 99 9) [8 9 1]))
; (is (= (product-digits 999 99) [9 8 9 0 1]))
;
;
; ; cartesian-product
;
; ; Write a function which calculates the Cartesian product of two sets.
;
; (deftest test-cartesian-product
;   (->> (map #(map vector xs (repeat %)) ys)
;        (apply concat)
;        (set)))
;
; (is (= (cartesian-product #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
;    #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
;      ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
;      ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
; (is (= (cartesian-product #{1 2 3} #{4 5})
;    #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
; (is (= 300 (count (cartesian-product (into #{} (range 10))
;                   (into #{} (range 30))))))
;
;
; ; dot-product
;
; ; Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same length.
;
; (deftest test-dot-product
;   (reduce + (map * xs ys)))
;
; (is (= 0 (dot-product [0 1 0] [1 0 0])))
; (is (= 3 (dot-product [1 1 1] [1 1 1])))
; (is (= 32 (dot-product [1 2 3] [4 5 6])))
; (is (= 256 (dot-product [2 5 6] [100 10 1])))
;
;
; ; least-common-multiple
;
; ; Write a function which calculates the least common multiple. Your function should accept
; ; a variable number of positive integers or ratios.
;
; (deftest test-least-common-multiple
;   (->> (iterate (partial + x) x)
;        (filter (fn [y] (every? #(zero? (rem y %)) xs)))
;        (first)))
;
; (least-common-multiple 5 3 7)
;
; (is (== (least-common-multiple 2 3) 6))
; (is (== (least-common-multiple 5 3 7) 105))
; (is (== (least-common-multiple 1/3 2/5) 2))
; (is (== (least-common-multiple 3/4 1/6) 3/2))
; (is (== (least-common-multiple 7 5/7 2 3/5) 210))
;
;
; ; to-tree-or-not-to-tree
;
; ; Write a predicate which checks whether or not a given sequence represents a binary tree.
; ; Each node in the tree must have a value, a left child, and a right child.
;
; (deftest test-to-tree-or-not-to-tree
;   (and (= 3 (count node))
;        (every? #(if (coll? %)
;                     (to-tree-or-not-to-tree %)
;                     (nil? %))
;                children)))
;
; (is (= (to-tree-or-not-to-tree '(:a (:b nil nil) nil)))
;    true))
; (is (= (to-tree-or-not-to-tree '(:a (:b nil nil))))
;    false))
; (is (= (to-tree-or-not-to-tree [1 nil [2 [3 nil nil] [4 nil nil]]]))
;    true))
; (is (= (to-tree-or-not-to-tree [1 [2 nil nil] [3 nil nil] [4 nil nil]]))
;    false))
; (= (to-tree-or-not-to-tree [1 [2 [3 [4 nil nil] nil] nil] nil])
;    true))
; (is (= (to-tree-or-not-to-tree [1 [2 [3 [4 false nil] nil] nil] nil]))
;    false))
; (is (= (to-tree-or-not-to-tree '(:a nil ())))
;    false))
;
;
; ; beauty-is-symmetry
;
; ; Let us define a binary tree as "symmetric" if the left half of the tree is the
; ; mirror image of the right half of the tree. Write a predicate to determine whether
; ; or not a given binary tree is symmetric. (see To Tree, or not to Tree for a reminder
; ; on the tree representation we're using).
;
; (deftest test-beauty-is-symmetry
;   (letfn [(mirror [t]
;             (when-let [[v l r] t]
;               [v (mirror r) (mirror l)]))]
;     (= l (mirror r))))
;
; (beauty-is-symmetry '(:a (:b [5 nil nil] nil) (:b nil [5 nil nil])))
;
; (is (= (beauty-is-symmetry '(:a (:b nil nil) (:b nil nil))) true))
; (is (= (beauty-is-symmetry '(:a (:b nil nil) nil)) false))
; (is (= (beauty-is-symmetry '(:a (:b nil nil) (:c nil nil))) false))
; (is (= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;                           [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
;    true))
; (is (= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;                           [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
;    false))
; (is (= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;                           [2 [3 nil [4 [6 nil nil] nil]] nil]])
;    false))
;
;
; ; group-a-sequence
;
; ; Given a function f and a sequence s, write a function which returns a map.
; ; The keys should be the values of f applied to each item in s.
; ; The value at each key should be a vector of corresponding items in the order they appear in s.
; ; Special Restriction - group-by
;
; (deftest test-group-a-sequence
;   (->> (map hash-map (map f xs) xs)
;        (apply merge-with vector)))
;
; (is (= (group-a-sequence #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
; (is (= (group-a-sequence #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
;    {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
; (is (= (group-a-sequence count [[1] [1 2] [3] [1 2 3] [2 3]])
;    {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))
;
;
; ; read-a-binary-number
;
; ; Convert a binary number, provided in the form of a string, to its numerical value.
;
; (deftest test-read-a-binary-number
;   (->> (reverse (seq xs))
;        (map #(Integer/parseInt (str %)))
;        (map-indexed #(bit-shift-left %2 %1))
;        (reduce +)))
;
; (is (= 0     (read-a-binary-number "0")))
; (is (= 7     (read-a-binary-number "111")))
; (is (= 8     (read-a-binary-number "1000")))
; (is (= 9     (read-a-binary-number "1001")))
; (is (= 255   (read-a-binary-number "11111111")))
; (is (= 1365  (read-a-binary-number "10101010101")))
; (is (= 65535 (read-a-binary-number "1111111111111111")))
;
;
; ; intro-to-destructuring-2
;
;
; ; Sequential destructuring allows you to bind symbols to parts of sequential things
; ; (vectors, lists, seqs, etc.): (let [bindings* ] exprs*) Complete the bindings
; ; so all let-parts evaluate to 3.
;
; (is (= 3
;   (let [[f args] [+ (range 3)]] (apply f args))
;   (let [[[f args] b] [[+ 1] 2]] (f args b))
;   (let [[f args] [inc 2]] (f args))))
;
;
; ; symmetric-differences
;
; ; Write a function which returns the symmetric difference of two sets.
; ; The symmetric difference is the set of items belonging to one but not both of the two sets.
;
; (deftest test-symmetric-differences
;   (clojure.set/union
;     (clojure.set/difference xs ys)
;     (clojure.set/difference ys xs)))
;
; (symmetric-differences #{1 2 3 4 5 6} #{1 3 5 7})
;
; (is (= (symmetric-differences #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
; (is (= (symmetric-differences #{:a :b :c} #{}) #{:a :b :c}))
; (is (= (symmetric-differences #{} #{4 5 6}) #{4 5 6}))
; (is (= (symmetric-differences #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]}))
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
; (deftest test-pascals-triangle
;   (if (= n 1)
;       '(1)
;       (let [xs (concat '(0) (pascals-triangle (dec n)) '(0))]
;         (->> (interleave xs (rest xs))
;              (partition 2)
;              (map (partial reduce +))))))
;
; (is (= (pascals-triangle 1) [1]))
; (is (= (map pascals-triangle (range 1 6))
;    [     [1]
;         [1 1]
;        [1 2 1]
;       [1 3 3 1]
;      [1 4 6 4 1]]))
; (is (= (pascals-triangle 11)
;    [1 10 45 120 210 252 210 120 45 10 1]))
;
;
; ; pairwise-disjoint-sets
;
; ; Given a set of sets, create a function which returns true if no two of those sets have
; ; any elements in common and false otherwise. Some of the test cases are a bit tricky,
; ; so pay a little more attention to them.
; ; Such sets are usually called pairwise disjoint or mutually disjoint.
;
; (deftest test-pairwise-disjoint-sets
;   (->> (apply concat xs)
;        (apply distinct?))
;   #_(letfn [(disjoint? [xs ys]
;              (every? false? (map (partial contains? ys) xs)))
;           (distributor [f xs]
;              (when-not (empty? xs)
;                (concat
;                  (map #(f (first xs) %) (rest xs))
;                  (distributor f (rest xs)))))]
;     (every? true? (distributor disjoint? xs))))
;
; (is (= (pairwise-disjoint-sets #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
;    true))
; (is (= (pairwise-disjoint-sets #{#{:a :b :c :d :e}
;          #{:a :b :c :d}
;          #{:a :b :c}
;          #{:a :b}
;          #{:a}})
;    false))
; (is (= (pairwise-disjoint-sets #{#{[1 2 3] [4 5]}
;          #{[1 2] [3 4 5]}
;          #{[1] [2] 3 4 5}
;          #{1 2 [3 4] [5]}})
;    true))
; (is (= (pairwise-disjoint-sets #{#{'a 'b}
;          #{'c 'd 'e}
;          #{'f 'g 'h 'i}
;          #{''a ''c ''f}})
;    true))
; (is (= (pairwise-disjoint-sets #{#{'(:x :y :z) '(:x :y) '(:z) '()}
;          #{#{:x :y :z} #{:x :y} #{:z} #{}}
;          #{'[:x :y :z] [:x :y] [:z] [] {}}})
;    false))
; (is (= (pairwise-disjoint-sets #{#{(= "true") false}
;          #{:yes :no}
;          #{(class 1) 0}
;          #{(symbol "true") 'false}
;          #{(keyword "yes") ::no}
;          #{(class '1) (int \0)}})
;    false))
; (is (= (pairwise-disjoint-sets #{#{distinct?}
;          #{#(-> %) #(-> %)}
;          #{#(-> %) #(-> %) #(-> %)}
;          #{#(-> %) #(-> %) #(-> %)}})
;    true))
; (is (= (pairwise-disjoint-sets #{#{(#(-> *)) + (quote mapcat) #_ nil}
;          #{'+ '* mapcat (comment mapcat)}
;          #{(do) set contains? nil?}
;          #{, , , #_, , empty?}})
;    false))
;
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
; (deftest test-inflix-calculator
;   (->> (partition-all 2 (rest xs))
;        (reduce #((first %2) %1 (second %2)) (first xs))))
;
; (is (= 7  (inflix-calculator 2 + 5)))
; (is (= 42 (inflix-calculator 38 + 48 - 2 / 2)))
; (is (= 8  (inflix-calculator 10 / 2 - 1 * 2)))
; (is (= 72 (inflix-calculator 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)))
;
;
; ; sum-of-square-of-digits
;
; ; Write a function which takes a collection of integers as an argument.
; ; Return the count of how many elements are smaller than the sum of their
; ; squared component digits. For example: 10 is larger than 1 squared plus 0 squared;
; ; whereas 15 is smaller than 1 squared plus 5 squared.
;
; (deftest test-sum-of-square-of-digits
;   (letfn [(squared-digits [x]
;             (->> (seq (str x))
;                  (map #(Integer/parseInt (str %)))
;                  (map #(* % %))
;                  (reduce +)))]
;     (count (filter #(> (squared-digits %) %) xs))))
;
;
; (is (= 8 (sum-of-square-of-digits (range 10))))
; (is (= 19 (sum-of-square-of-digits (range 30))))
; (is (= 50 (sum-of-square-of-digits (range 100))))
; (is (= 50 (sum-of-square-of-digits (range 1000))))
;
;
; ; indexing-sequences
;
; ; Transform a sequence into a sequence of pairs containing the original elements along
; ; with their index.
;
; (deftest test-indexing-sequences
;   (map vector xs (range)))
;
; (is (= (indexing-sequences [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))
; (is (= (indexing-sequences [0 1 3]) '((0 0) (1 1) (3 2))))
; (is (= (indexing-sequences [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]]))
;
;
; ; reimplement-map
;
; ; Map is one of the core elements of a functional programming language. Given a function f and
; ; an input sequence s, return a lazy sequence of (f x) for each element x in s.
; ; Special Restrictions - map, map-indexed, mapcat, for
;
; (deftest test-reimplement-map
;   (rest (reductions #(f %2) nil xs))
;   #_(lazy-seq
;     (when-let [[x & s] (seq xs)]
;       (cons (f x) (reimplement-map f s)))))
;
; (is (= [3 4 5 6 7]
;    (reimplement-map inc [2 3 4 5 6])))
; (is (= (repeat 10 nil)
;    (reimplement-map (fn [_] nil) (range 10))))
; (is (= [1000000 1000001]
;    (->> (reimplement-map inc (range))
;         (drop (dec 1000000))
;         (take 2))))
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
; (deftest test-trees-into-tables
;   (into {} (for [[k v] xs [v-k v-v] v] [[k v-k] v-v])
;
;     #_(for [[k v] xs]
;         (->> (map #(cons k %) v)
;              (map #(hash-map (vec (drop-last %)) (last %)))
;              (apply merge)))))
;
; (is (= (trees-into-tables '{a {p 1, q 2} b {m 3, n 4}})
;                       '{[a p] 1, [a q] 2 [b m] 3, [b n] 4}))
; (is (= (trees-into-tables '{[1] {a b c d}
;          [2] {q r s t u v w x}})
;    '{[[1] a] b, [[1] c] d,
;      [[2] q] r, [[2] s] t,
;      [[2] u] v, [[2] w] x}))
; (is (= (trees-into-tables '{m {1 [a b c] 3 nil}})
;    '{[m 1] [a b c], [m 3] nil}))
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
; (deftest test-recognize-playing-cards
;   (let [suits {\H :heart, \D :diamond, \C :club, \S :spade}
;         ranks (zipmap "23456789TJQKA" (range 13))]
;     {:suit (get suits s), :rank (get ranks r)}))
;
; (is (= {:suit :diamond :rank 10} (recognize-playing-cards "DQ")))
; (is (= {:suit :heart :rank 3} (recognize-playing-cards "H5")))
; (is (= {:suit :club :rank 12} (recognize-playing-cards "CA")))
; (is (= (range 13) (map (comp :rank recognize-playing-cards str)
;                    '[S2 S3 S4 S5 S6 S7
;                      S8 S9 ST SJ SQ SK SA])))
;
;
; ; pascals-trapezoid
;
; ; Write a function that, for any given input vector of numbers, returns an infinite lazy sequence of vectors,
; ; where each next one is constructed from the previous following the rules used in Pascal's Triangle.
;
;
;
;
;
;
; (deftest test-pascals-trapezoid
;   (iterate #(vec (map +' (concat [0] %) (concat % [0]))) xs))
;
; (is (= (second (pascals-trapezoid [2 3 2])) [2 5 5 2]))
; (is (= (take 5 (pascals-trapezoid [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))
; (is (= (take 2 (pascals-trapezoid [3 1 2])) [[3 1 2] [3 4 3 2]]))
; (is (= (take 100 (pascals-trapezoid [2 4 2])) (rest (take 101 (pascals-trapezoid [2 2])))))
