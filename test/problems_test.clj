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

(deftest test-interleave-two-sequences
  (is (= (interleave-two-sequences [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
  (is (= (interleave-two-sequences [1 2] [3 4 5 6]) '(1 3 2 4)))
  (is (= (interleave-two-sequences [1 2 3 4] [5]) [1 5]))
  (is (= (interleave-two-sequences [30 20] [25 15]) [30 25 20 15])))

(deftest test-factorial-fun
  (is (= (factorial-fun 1) 1))
  (is (= (factorial-fun 3) 6))
  (is (= (factorial-fun 5) 120))
  (is (= (factorial-fun 8) 40320)))

(deftest test-flatten-a-sequence
  (is (= (flatten-a-sequence '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
  (is (= (flatten-a-sequence ["a" ["b"] "c"]) '("a" "b" "c")))
  (is (= (flatten-a-sequence '((((:a))))) '(:a))))

(deftest test-implement-range
  (is (= (implement-range 1 4) '(1 2 3)))
  (is (= (implement-range -2 2) '(-2 -1 0 1)))
  (is (= (implement-range 5 8) '(5 6 7))))

(deftest test-intro-to-some 6
  (is (= intro-to-some (some #{2 7 6} [5 6 7 8])))
  (is (= intro-to-some (some #(when (even? %) %) [5 6 7 8]))))

(deftest test-duplicate-a-sequence
  (is (= (duplicate-a-sequence [1 2 3]) '(1 1 2 2 3 3)))
  (is (= (duplicate-a-sequence [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
  (is (= (duplicate-a-sequence [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))))

(deftest test-get-the-caps
  (is (= (get-the-caps "HeLlO, WoRlD!") "HLOWRD"))
  (is (empty? (get-the-caps "nothing")))
  (is (= (get-the-caps "$#A(*&987Zf") "AZ")))

(deftest test-maximum-value
  (is (= (maximum-value 1 8 3 4) 8))
  (is (= (maximum-value 30 20) 30))
  (is (= (maximum-value 45 67 11) 67)))

(deftest test-fibonacci-sequence
  (is (= (fibonacci-sequence 3) '(1 1 2)))
  (is (= (fibonacci-sequence 6) '(1 1 2 3 5 8)))
  (is (= (fibonacci-sequence 8) '(1 1 2 3 5 8 13 21))))

(deftest test-palindrome-detector
  (is (false? (palindrome-detector '(1 2 3 4 5))))
  (is (true? (palindrome-detector "racecar")))
  (is (true? (palindrome-detector [:foo :bar :foo])))
  (is (true? (palindrome-detector '(1 1 3 3 1 1))))
  (is (false? (palindrome-detector '(:a :b :c)))))

(deftest test-reverse-a-sequence
  (is (= (reverse-a-sequence [1 2 3 4 5]) [5 4 3 2 1]))
  (is (= (reverse-a-sequence (sorted-set 5 7 2 7)) '(7 5 2)))
  (is (= (reverse-a-sequence [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])))

(deftest test-find-the-odd-numbers
  (is (= (find-the-odd-numbers #{1 2 3 4 5}) '(1 3 5)))
  (is (= (find-the-odd-numbers [4 2 1 6]) '(1)))
  (is (= (find-the-odd-numbers [2 2 4 6]) '()))
  (is (= (find-the-odd-numbers [1 1 1 3]) '(1 1 1 3))))

(deftest test-sum-it-all-up
  (is (= (sum-it-all-up [1 2 3]) 6))
  (is (= (sum-it-all-up (list 0 -2 5 5)) 8))
  (is (= (sum-it-all-up #{4 2 1}) 7))
  (is (= (sum-it-all-up '(0 0 -1)) -1))
  (is (= (sum-it-all-up '(1 10 3)) 14)))

(deftest test-count-a-sequence
  (is (= (count-a-sequence '(1 2 3 3 1)) 5))
  (is (= (count-a-sequence "Hello World") 11))
  (is (= (count-a-sequence [[1 2] [3 4] [5 6]]) 3))
  (is (= (count-a-sequence '(13)) 1))
  (is (= (count-a-sequence '(:a :b :c)) 3)))

(deftest test-nth-element
  (is (= (nth-element '(4 5 6 7) 2) 6))
  (is (= (nth-element [:a :b :c] 0) :a))
  (is (= (nth-element [1 2 3 4] 1) 2))
  (is (= (nth-element '([1 2] [3 4] [5 6]) 2) [5 6])))

(deftest test-penultimate-element
  (is (= (penultimate-element (list 1 2 3 4 5)) 4))
  (is (= (penultimate-element ["a" "b" "c"]) "b"))
  (is (= (penultimate-element [[1 2] [3 4]]) [1 2])))

(deftest test-set-intersection
  (is (= (set-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3}))
  (is (= (set-intersection #{0 1 2} #{3 4 5}) #{}))
  (is (= (set-intersection #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})))

(deftest test-reimplement-iterate
  (is (= (take 5 (reimplement-iterate #(* 2 %) 1)) [1 2 4 8 16]))
  (is (= (take 100 (reimplement-iterate inc 0)) (take 100 (range))))
  (is (= (take 9 (reimplement-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

(deftest test-simple-closures
  (is (= 256 ((simple-closures 2) 16),
         ((simple-closures 8) 2)))
  (is (= [1 8 27 64] (map (simple-closures 3) [1 2 3 4])))
  (is (= [1 2 4 8 16] (map #((simple-closures %) 2) [0 1 2 3 4]))))

(deftest test-product-digits
  (is (= (product-digits 1 1) [1]))
  (is (= (product-digits 99 9) [8 9 1]))
  (is (= (product-digits 999 99) [9 8 9 0 1])))

(deftest test-cartesian-product
  (is (= (cartesian-product #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
         #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
           ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
           ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
  (is (= (cartesian-product #{1 2 3} #{4 5})
         #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
  (is (= 300 (count (cartesian-product (into #{} (range 10))
                                       (into #{} (range 30)))))))

(deftest test-dot-product
  (is (= 0 (dot-product [0 1 0] [1 0 0])))
  (is (= 3 (dot-product [1 1 1] [1 1 1])))
  (is (= 32 (dot-product [1 2 3] [4 5 6])))
  (is (= 256 (dot-product [2 5 6] [100 10 1]))))

(deftest test-least-common-multiple
  (is (== (least-common-multiple 2 3) 6))
  (is (== (least-common-multiple 5 3 7) 105))
  (is (== (least-common-multiple 1/3 2/5) 2))
  (is (== (least-common-multiple 3/4 1/6) 3/2))
  (is (== (least-common-multiple 7 5/7 2 3/5) 210)))

(deftest test-to-tree-or-not-to-tree
  (is (= (to-tree-or-not-to-tree '(:a (:b nil nil) nil))
         true))
  (is (= (to-tree-or-not-to-tree '(:a (:b nil nil)))
         false))
  (is (= (to-tree-or-not-to-tree [1 nil [2 [3 nil nil] [4 nil nil]]])
         true))
  (is (= (to-tree-or-not-to-tree [1 [2 nil nil] [3 nil nil] [4 nil nil]])
         false))
  (is (= (to-tree-or-not-to-tree [1 [2 [3 [4 nil nil] nil] nil] nil])
         true))
  (is (= (to-tree-or-not-to-tree [1 [2 [3 [4 false nil] nil] nil] nil])
         false))
  (is (= (to-tree-or-not-to-tree '(:a nil ()))
         false)))

(deftest test-beauty-is-symmetry
  (is (= (beauty-is-symmetry '(:a (:b nil nil) (:b nil nil))) true))
  (is (= (beauty-is-symmetry '(:a (:b nil nil) nil)) false))
  (is (= (beauty-is-symmetry '(:a (:b nil nil) (:c nil nil))) false))
  (is (= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                              [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
         true))
  (is (= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                              [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
         false))
  (is (= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                              [2 [3 nil [4 [6 nil nil] nil]] nil]])
         false)))

(deftest test-group-a-sequence
  (is (= (group-a-sequence #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
  (is (= (group-a-sequence #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
         {1/2 [[[1 2] [2 4]] [3 6]], 2/3 [4 6]}))
  (is (= (group-a-sequence count [[1] [1 2] [3] [1 2 3] [2 3]])
         {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [1 2 3]})))

(deftest test-read-a-binary-number
  (is (= 0     (read-a-binary-number "0")))
  (is (= 7     (read-a-binary-number "111")))
  (is (= 8     (read-a-binary-number "1000")))
  (is (= 9     (read-a-binary-number "1001")))
  (is (= 255   (read-a-binary-number "11111111")))
  (is (= 1365  (read-a-binary-number "10101010101")))
  (is (= 65535 (read-a-binary-number "1111111111111111"))))

(deftest test-sequential-destructuring-2
  (is (= sequential-destructuring-2
         (let [[f args] [+ (range 3)]] (apply f args))
         (let [[[f args] b] [[+ 1] 2]] (f args b))
         (let [[f args] [inc 2]] (f args)))))

(deftest test-symmetric-differences
  (is (= (symmetric-differences #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
  (is (= (symmetric-differences #{:a :b :c} #{}) #{:a :b :c}))
  (is (= (symmetric-differences #{} #{4 5 6}) #{4 5 6}))
  (is (= (symmetric-differences #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})))

(deftest test-pascals-triangle
  (is (= (pascals-triangle 1) [1]))
  (is (= (map pascals-triangle (range 1 6))
         [     [1]
          [1 1]
          [1 2 1]
          [1 3 3 1]
          [1 4 6 4 1]]))
  (is (= (pascals-triangle 11)
         [1 10 45 120 210 252 210 120 45 10 1])))
(deftest test-pairwise-disjoint-sets
  (is (= (pairwise-disjoint-sets #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
         true))
  (is (= (pairwise-disjoint-sets #{#{:a :b :c :d :e}
                                   #{:a :b :c :d}
                                   #{:a :b :c}
                                   #{:a :b}
                                   #{:a}})
         false))
  (is (= (pairwise-disjoint-sets #{#{[1 2 3] [4 5]}
                                   #{[1 2] [3 4 5]}
                                   #{[1] [2] 3 4 5}
                                   #{1 2 [3 4] [5]}})
         true))
  (is (= (pairwise-disjoint-sets #{#{'a 'b}
                                   #{'c 'd 'e}
                                   #{'f 'g 'h 'i}
                                   #{''a ''c ''f}})
         true))
  (is (= (pairwise-disjoint-sets #{#{'(:x :y :z) '(:x :y) '(:z) '()}
                                   #{#{:x :y :z} #{:x :y} #{:z} #{}}
                                   #{'[:x :y :z] [:x :y] [:z] [] {}}})
         false))
  (is (= (pairwise-disjoint-sets #{#{(= "true") false}
                                   #{:yes :no}
                                   #{(class 1) 0}
                                   #{(symbol "true") 'false}
                                   #{(keyword "yes") ::no}
                                   #{(class '1) (int \0)}})
         false))
  (is (= (pairwise-disjoint-sets #{#{distinct?}
                                   #{#(-> %) #(-> %)}
                                   #{#(-> %) #(-> %) #(-> %)}
                                   #{#(-> %) #(-> %) #(-> %)}})
         true))
  (is (= (pairwise-disjoint-sets #{#{(#(-> *)) + (quote mapcat) #_ nil}
                                   #{'+ '* mapcat (comment mapcat)}
                                   #{(do) set contains? nil?}
                                   #{, , , #_, , empty?}})
         false)))

(deftest test-inflix-calculator
  (is (= 7  (inflix-calculator 2 + 5)))
  (is (= 42 (inflix-calculator 38 + 48 - 2 / 2)))
  (is (= 8  (inflix-calculator 10 / 2 - 1 * 2)))
  (is (= 72 (inflix-calculator 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))))

(deftest test-sum-of-square-of-digits
  (is (= 8 (sum-of-square-of-digits (range 10))))
  (is (= 19 (sum-of-square-of-digits (range 30))))
  (is (= 50 (sum-of-square-of-digits (range 100))))
  (is (= 50 (sum-of-square-of-digits (range 1000)))))

(deftest test-indexing-sequences
  (is (= (indexing-sequences [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))
  (is (= (indexing-sequences [0 1 3]) '((0 0) (1 1) (3 2))))
  (is (= (indexing-sequences [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])))

(deftest test-reimplement-map
  (is (= [3 4 5 6 7]
         (reimplement-map inc [2 3 4 5 6])))
  (is (= (repeat 10 nil)
         (reimplement-map (fn [_] nil) (range 10))))
  (is (= [1000000 1000001]
         (->> (reimplement-map inc (range))
              (drop (dec 1000000))
              (take 2)))))

(deftest test-trees-into-tables
  (is (= (trees-into-tables '{a {p 1, q 2} b {m 3, n 4}})
         '{[a p] 1, [a q] 2 [b m] 3, [b n] 4}))
  (is (= (trees-into-tables '{[1] {a b c d}
                              [2] {q r s t u v w x}})
         '{[[1] a] b, [[1] c] d,
           [[2] q] r, [[2] s] t,
           [[2] u] v, [[2] w] x}))
  (is (= (trees-into-tables '{m {1 [a b c] 3 nil}})
         '{[m 1] [a b c], [m 3] nil})))

(deftest test-recognize-playing-cards
  (is (= {:suit :diamond :rank 10} (recognize-playing-cards "DQ")))
  (is (= {:suit :heart :rank 3} (recognize-playing-cards "H5")))
  (is (= {:suit :club :rank 12} (recognize-playing-cards "CA")))
  (is (= (range 13) (map (comp :rank recognize-playing-cards str)
                         '[S2 S3 S4 S5 S6 S7
                           S8 S9 ST SJ SQ SK SA]))))

(deftest test-pascals-trapezoid
  (is (= (second (pascals-trapezoid [2 3 2])) [2 5 5 2]))
  (is (= (take 5 (pascals-trapezoid [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))
  (is (= (take 2 (pascals-trapezoid [3 1 2])) [[3 1 2] [3 4 3 2]]))
  (is (= (take 100 (pascals-trapezoid [2 4 2])) (rest (take 101 (pascals-trapezoid [2 2]))))))
