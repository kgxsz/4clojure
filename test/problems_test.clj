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
         {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
  (is (= (group-a-sequence count [[1] [1 2] [3] [1 2 3] [2 3]])
         {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

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

(deftest test-prime-numbers
  (is (= (prime-numbers 2) [2 3]))
  (is (= (prime-numbers 5) [2 3 5 7 11]))
  (is (= (last (prime-numbers 100)) 541)))

(deftest test-rotate-a-sequence
  (is (= (rotate-a-sequence 2 [1 2 3 4 5]) '(3 4 5 1 2)))
  (is (= (rotate-a-sequence -2 [1 2 3 4 5]) '(4 5 1 2 3)))
  (is (= (rotate-a-sequence 6 [1 2 3 4 5]) '(2 3 4 5 1)))
  (is (= (rotate-a-sequence 1 '(:a :b :c)) '(:b :c :a)))
  (is (= (rotate-a-sequence -4 '(:a :b :c)) '(:c :a :b))))

(deftest test-juxtaposition
  (is (= [21 6 1] ((juxtaposition + max min) 2 3 5 1 6 4)))
  (is (= ["HELLO" 5] ((juxtaposition #(.toUpperCase %) count) "hello")))
  (is (= [2 6 4] ((juxtaposition :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))))

(deftest test-filter-perfect-squares
  (is (= (filter-perfect-squares "4,5,6,7,8,9") "4,9"))
  (is (= (filter-perfect-squares "15,16,25,36,37") "16,25,36")))

(deftest test-perfect-numbers
  (is (= (perfect-numbers 6) true))
  (is (= (perfect-numbers 7) false))
  (is (= (perfect-numbers 496) true))
  (is (= (perfect-numbers 500) false))
  (is (= (perfect-numbers 8128) true)))

(deftest test-flipping-out
  (is (= 3 ((flipping-out nth) 2 [1 2 3 4 5])))
  (is (= true ((flipping-out >) 7 8)))
  (is (= 4 ((flipping-out quot) 2 8)))
  (is (= [1 2 3] ((flipping-out take) [1 2 3 4 5] 3))))

(deftest test-reverse-interleave
  (is (= (reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
  (is (= (reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
  (is (= (reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))))

(deftest test-split-by-type
  (is (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
  (is (= (set (split-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
  (is (= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})))

(deftest test-count-occurences
  (is (= (count-occurences [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
  (is (= (count-occurences [:b :a :b :a :b]) {:a 2, :b 3}))
  (is (= (count-occurences '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})))

(deftest test-find-distinct-items
  (is (= (find-distinct-items [1 2 1 3 1 2 4]) [1 2 3 4]))
  (is (= (find-distinct-items [:a :a :b :b :c :c]) [:a :b :c]))
  (is (= (find-distinct-items '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
  (is (= (find-distinct-items (range 50)) (range 50))))

(deftest test-function-composition
  (is (= [3 2 1] ((function-composition rest reverse) [1 2 3 4])))
  (is (= 5 ((function-composition (partial + 3) second) [1 2 3 4])))
  (is (= true ((function-composition zero? #(mod % 8) +) 3 5 7 9)))
  (is (= "HELLO" ((function-composition #(.toUpperCase %) #(apply str %) take) 5 "hello world"))))

(deftest test-partition-a-sequence
  (is (= (partition-a-sequence 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
  (is (= (partition-a-sequence 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
  (is (= (partition-a-sequence 3 (range 8)) '((0 1 2) (3 4 5)))))

(deftest test-word-sorting
  (is (= (word-sorting  "Have a nice day.")
         ["a" "day" "Have" "nice"]))
  (is (= (word-sorting  "Clojure is a fun language!")
         ["a" "Clojure" "fun" "is" "language"]))
  (is (= (word-sorting  "Fools fall for foolish follies.")
         ["fall" "follies" "foolish" "Fools" "for"])))

(deftest test-black-box-testing
  (is (= :map (black-box-testing {:a 1, :b 2})))
  (is (= :list (black-box-testing (range (rand-int 20)))))
  (is (= :vector (black-box-testing [1 2 3 4 5 6])))
  (is (= :set (black-box-testing #{10 (rand-int 5)})))
  (is (= [:map :set :vector :list] (map black-box-testing [{} #{} [] ()]))))

(deftest test-intro-to-trampoline
  (= [1 3 5 7 9 11] intro-to-trampoline))

(deftest test-anagram-finder
  (is (= (anagram-finder ["meat" "mat" "team" "mate" "eat"])
         #{#{"meat" "team" "mate"}}))
  (is (= (anagram-finder ["veer" "lake" "item" "kale" "mite" "ever"])
         #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})))

(deftest test-sequence-reductions
  (is (= (take 5 (sequence-reductions + (range))) [0 1 3 6 10]))
  (is (= (sequence-reductions conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
  (is (= (last (sequence-reductions * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)))

(deftest test-merge-with-a-function
  (is (= (merge-with-a-function * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
         {:a 4, :b 6, :c 20}))
  (is (= (merge-with-a-function - {1 10, 2 20} {1 3, 2 10, 3 15})
         {1 7, 2 10, 3 15}))
  (is (= (merge-with-a-function concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
         {:a [3 4 5], :b [6 7], :c [8 9]})))

(deftest test-into-camel-case
  (is (= (into-camel-case "something") "something"))
  (is (= (into-camel-case "multi-word-key") "multiWordKey"))
  (is (= (into-camel-case "leaveMeAlone") "leaveMeAlone")))

(deftest test-happy-numbers
  (is (= (happy-numbers 7) true))
  (is (= (happy-numbers 986543210) true))
  (is (= (happy-numbers 2) false))
  (is (= (happy-numbers 3) false)))

(deftest test-eulers-totient-function
  (is (= (eulers-totient-function 1) 1))
  (is (= (eulers-totient-function 10) (count '(1 3 7 9)) 4))
  (is (= (eulers-totient-function 40) 16))
  (is (= (eulers-totient-function 99) 60)))

(deftest test-power-set
  (is (= (power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
  (is (= (power-set #{}) #{#{}}))
  (is (= (power-set #{1 2 3})
         #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))
  (is (= (count (power-set (into #{} (range 10)))) 1024)))

(deftest test-the-balance-of-n
  (is (= true (the-balance-of-n 11)))
  (is (= true (the-balance-of-n 121)))
  (is (= false (the-balance-of-n 123)))
  (is (= true (the-balance-of-n 0)))
  (is (= false (the-balance-of-n 88099)))
  (is (= true (the-balance-of-n 89098)))
  (is (= true (the-balance-of-n 89089)))
  (is (= (take 20 (filter the-balance-of-n (range)))
         [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])))

(deftest test-digits-and-bases
  (is (= [1 2 3 4 5 0 1] (digits-and-bases 1234501 10)))
  (is (= [0] (digits-and-bases 0 11)))
  (is (= [1 0 0 1] (digits-and-bases 9 2)))
  (is (= [1 0] (let [n (rand-int 100000)](digits-and-bases n n))))
  (is (= [16 18 5 24 15 1] (digits-and-bases Integer/MAX_VALUE 42))))

(deftest test-identify-keys-and-values
  (is (= {} (identify-keys-and-values [])))
  (is (= {:a [1]} (identify-keys-and-values [:a 1])))
  (is (= {:a [1], :b [2]} (identify-keys-and-values [:a 1, :b 2])))
  (is (= {:a [1 2 3], :b [], :c [4]} (identify-keys-and-values [:a 1 2 3 :b :c 4]))))

(deftest test-palindromic-numbers
  (is (=  (take 26  (palindromic-numbers 0))
         [0 1 2 3 4 5 6 7 8 9
          11 22 33 44 55 66 77 88 99
          101 111 121 131 141 151 161]))
  (is (=  (take 16  (palindromic-numbers 162))
         [171 181 191 202
          212 222 232 242
          252 262 272 282
          292 303 313 323]))
  (is (=  (take 6  (palindromic-numbers 1234550000))
         [1234554321 1234664321 1234774321
          1234884321 1234994321 1235005321]))
  (is (=  (first  (palindromic-numbers  (* 111111111 111111111)))
         (* 111111111 111111111)))
  (is (=  (set  (take 199  (palindromic-numbers 0)))
         (set  (map #(first  (palindromic-numbers %))  (range 0 10000)))))
  (is (= true (apply <  (take 6666  (palindromic-numbers 9999999)))))
  (is (=  (nth  (palindromic-numbers 0) 10101) 9102019)))

(deftest test-balanced-primes
  (is (= false  (balanced-primes 4)))
  (is (= true  (balanced-primes 563)))
  (is (= 1103  (nth  (filter balanced-primes (range)) 15))))
