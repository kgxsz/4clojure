; MEDIUM


; prime-numbers

; Write a function which returns the first x number of prime numbers.

(defn prime-numbers [n]
  (->> (range)
       (drop 2)
       (filter (fn [x] (every? #(pos? (rem x %)) (range 2 x))))
       (take n))

  #_(let [upper-bound (if (< n 6) 12 (+ (* n (Math/log n)) (* n (Math/log (Math/log n)))))
        possible-primes (range 2 upper-bound)
        non-primes (apply clojure.set/union
                     (for [factor (range 2 (/ upper-bound 2))]
                       (set (rest (range factor upper-bound factor)))))]
    (take n (remove non-primes possible-primes))))


(prime-numbers 5)

(= (prime-numbers 2) [2 3])
(= (prime-numbers 5) [2 3 5 7 11])
(= (last (prime-numbers 100)) 541)


; rotate-a-sequence

(defn rotate-a-sequence [n xs]
  (let [r (mod n (count xs))]
    (concat (drop r xs) (take r xs)))
  #_(if (pos? (mod n (count xs)))
      (recur (dec n) (concat (rest xs) (take 1 xs)))
      xs))

; Write a function which can rotate a sequence in either direction.

(= (rotate-a-sequence 2 [1 2 3 4 5]) '(3 4 5 1 2))
(= (rotate-a-sequence -2 [1 2 3 4 5]) '(4 5 1 2 3))
(= (rotate-a-sequence 6 [1 2 3 4 5]) '(2 3 4 5 1))
(= (rotate-a-sequence 1 '(:a :b :c)) '(:b :c :a))
(= (rotate-a-sequence -4 '(:a :b :c)) '(:c :a :b))


; juxtaposition

; Take a set of functions and return a new function that takes a variable number of arguments and returns
; a sequence containing the result of applying each function left-to-right to the argument list.
; Special Restrictions - juxt

(defn juxtaposition [& fs]
  (fn [& xs]
    (for [f fs]
      (apply f xs))))

((juxtaposition + max min)  2 3 5 1 6 4)

(= [21 6 1] ((juxtaposition + max min) 2 3 5 1 6 4))
(= ["HELLO" 5] ((juxtaposition #(.toUpperCase %) count) "hello"))
(= [2 6 4] ((juxtaposition :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))


; filter-perfect-squares

; Given a string of comma separated integers, write a function which returns a new comma
; separated string that only contains the numbers which are perfect squares.

(defn filter-perfect-squares [s]
  (->>
   (clojure.string/split s #",")
   (map #(Integer/parseInt %))
   (filter #(= 0.0 (mod (Math/sqrt %) 1)))
   (interpose ",")
   (apply str)))


(= (filter-perfect-squares "4,5,6,7,8,9") "4,9")
(= (filter-perfect-squares "15,16,25,36,37") "16,25,36")


; perfect-numbers

; A number is "perfect" if the sum of its divisors equal the number itself. 6 is a perfect
; number because 1+2+3=6. Write a function which returns true for perfect numbers and false otherwise.

(defn perfect-numbers [x]
  (->>
    (range 1 x)
    (filter #(zero? (mod x %)))
    (reduce +)
    (= x)))

(perfect-numbers 6)

(= (perfect-numbers 6) true)
(= (perfect-numbers 7) false)
(= (perfect-numbers 496) true)
(= (perfect-numbers 500) false)
(= (perfect-numbers 8128) true)


; flipping-out

; Write a higher-order function which flips the order of the arguments of an input function.

(defn flipping-out [f]
  (fn [x y]
    (f y x)))


(= 3 ((flipping-out nth) 2 [1 2 3 4 5]))
(= true ((flipping-out >) 7 8))
(= 4 ((flipping-out quot) 2 8))
(= [1 2 3] ((flipping-out take) [1 2 3 4 5] 3))


; reverse-interleave

; Write a function which reverses the interleave process into x number of subsequences.

(defn reverse-interleave [xs n]
  (apply map vector (partition n xs)))

(= (reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(= (reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
(= (reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))


; split-by-type

; Write a function which takes a sequence consisting of items with different types and splits
; them up into a set of homogeneous sub-sequences. The internal order of each sub-sequence should
; be maintained, but the sub-sequences themselves can be returned in any order (this is why 'set'
; is used in the test cases).

(defn split-by-type [xs]
  (vals (group-by type xs)))

(= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
(= (set (split-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
(= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})


; count-occurences

; Write a function which returns a map containing the number of occurences
; of each distinct item in a sequence.
; Special Restrictions - frequencies

(defn count-occurences [xs]
  (let [m (group-by identity xs)]
    (zipmap (keym) (map count (vals m)))))

(= (count-occurences [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
(= (count-occurences [:b :a :b :a :b]) {:a 2, :b 3})
(= (count-occurences '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})


; find-distinct-items

; Write a function which removes the duplicates from a sequence. Order of the items must be maintained.
; Special Restrictions - distinct

(defn find-distinct-items [xs]
  (->>
    (group-by identity xs)
    (keys)
    (sort-by #(.indexOf xs %))))

(= (find-distinct-items [1 2 1 3 1 2 4]) [1 2 3 4])
(= (find-distinct-items [:a :a :b :b :c :c]) [:a :b :c])
(= (find-distinct-items '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
(= (find-distinct-items (range 50)) (range 50))


; function-composition

; Write a function which allows you to create function compositions. The parameter list should take a
; variable number of functions, and create a function applies them from right-to-left.
; Special Restrictions - comp

(defn function-composition [& fs]
  (fn [& xs]
     (first (reduce #(vector (apply %2 %1)) xs (reverse fs)))))

(= [3 2 1] ((function-composition rest reverse) [1 2 3 4]))
(= 5 ((function-composition (partial + 3) second) [1 2 3 4]))
(= true ((function-composition zero? #(mod % 8) +) 3 5 7 9))
(= "HELLO" ((function-composition #(.toUpperCase %) #(apply str %) take) 5 "hello world"))


; partition-a-sequence

; Write a function which returns a sequence of lists of x items each.
; Lists of less than x items should not be returned.
; Special Restrictions - partition, partition-all

(defn partition-a-sequence [x xs]
  (for [k (range (quot (count xs) x))
        :let [n (* k x)]]
    (take x (drop n xs))))

(= (partition-a-sequence 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
(= (partition-a-sequence 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
(= (partition-a-sequence 3 (range 8)) '((0 1 2) (3 4 5)))


; word-sorting

; Write a function that splits a sentence up into a sorted list of words.
; Capitalization should not affect sort order and punctuation should be ignored.

(defn word-sorting [s]
  (sort-by #(clojure.string/lower-case %) (re-seq #"\w+" s))
  #_(let [f clojure.string/lower-case]
  (->> (clojure.string/split s #"\.?\s|\.|!")
       (sort #(compare (f %1) (f %2))))))

(= (word-sorting  "Have a nice day.")
   ["a" "day" "Have" "nice"])
(= (word-sorting  "Clojure is a fun language!")
   ["a" "Clojure" "fun" "is" "language"])
(= (word-sorting  "Fools fall for foolish follies.")
   ["fall" "follies" "foolish" "Fools" "for"])


; black-box-testing

; Clojure has many sequence types, which act in subtly different ways. The core functions
; typically convert them into a uniform "sequence" type and work with them that way,
; but it can be important to understand the behavioral and performance differences so
; that you know which kind is appropriate for your application.
; Write a function which takes a collection and returns one of :map, :set, :list, or :vector - describing the type of collection it was given.
; You won't be allowed to inspect their class or use the built-in predicates like list? - the point is to poke at them and understand their behavior.
; Special Restrictions - class, type, Class, vector?, sequential?, list?, seq?, map?, set?, instance?, getClass

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

(str {:a 1, :b 2})

(= :map (black-box-testing {:a 1, :b 2}))
(= :list (black-box-testing (range (rand-int 20))))
(= :vector (black-box-testing [1 2 3 4 5 6]))
(= :set (black-box-testing #{10 (rand-int 5)}))
(= [:map :set :vector :list] (map black-box-testing [{} #{} [] ()]))


; intro-to-trampoline

; The trampoline function takes a function f and a variable number of parameters.
; Trampoline calls f with any parameters that were supplied. If f returns a function,
; trampoline calls that function with no arguments. This is repeated, until the return
; value is not a function, and then trampoline returns that non-function value.
; This is useful for implementing mutually recursive algorithms in a way that won't consume the stack.

(= [1 3 5 7 9 11]
   (letfn
     [(foo [x y] #(bar (conj x y) y))
      (bar [x y] (if (> (last x) 10)
                   x
                   #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))


; anagram-finder

; Write a function which finds all the anagrams in a vector of words. A word x is an anagram of word y if all the letters in x can be rearranged
; in a different order to form y. Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other.
; Each sub-set should have at least two words. Words without any anagrams should not be included in the result.

(defn anagram-finder [xs]
  (->> (vals (group-by sort xs))
       (filter #(> (count %) 1))
       (map set)
       (set)))

(= (anagram-finder ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})
(= (anagram-finder ["veer" "lake" "item" "kale" "mite" "ever"])
   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})


; sequence-reductions

; Write a function which behaves like reduce, but returns each intermediate value of the reduction.
; Your function must accept either two or three arguments, and the return sequence must be lazy.
; Special Restrictions - reductions

(defn sequence-reductions
  ([f xs]
    (sequence-reductions f (first xs) (rest xs)))
  ([f x xs]
    (lazy-seq
      (cons x
        (if (empty? xs) [] (sequence-reductions f (f x (first xs)) (rest xs)))))))

(= (take 5 (sequence-reductions + (range))) [0 1 3 6 10])
(= (sequence-reductions conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
(= (last (sequence-reductions * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)


; merge-with-a-function

; Write a function which takes a function f and a variable number of maps. Your function should return a map that
; consists of the rest of the maps conj-ed onto the first. If a key occurs in more than one map, the mapping(s)
; from the latter (left-to-right) should be combined with the mapping in the result by calling (f val-in-result val-in-latter)
; Special Restrictions - merge-with

(defn merge-with-a-function [f & xs]
  (let [ys (group-by first (mapcat seq xs))]
    (->> (vals ys)
         (map #(map second %))
         (map #(reduce f %))
         (zipmap (keys ys)))))

(= (merge-with-a-function * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})
(= (merge-with-a-function - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15})
(= (merge-with-a-function concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   {:a [3 4 5], :b [6 7], :c [8 9]})


; into-camel-case

; When working with java, you often need to create an object with fieldsLikeThis, but you'd rather work with a hashmap that
; has :keys-like-this until it's time to convert. Write a function which takes lower-case hyphen-separated strings
; and converts them to camel-case strings.

(defn into-camel-case [xs]
  (let [split-xs (clojure.string/split xs #"-")]
    (->> (rest split-xs)
         (map clojure.string/capitalize)
         (cons (first split-xs))
         (apply str))))

(= (into-camel-case "something") "something")
(= (into-camel-case "multi-word-key") "multiWordKey")
(= (into-camel-case "leaveMeAlone") "leaveMeAlone")


; happy-numbers

; Happy numbers are positive integers that follow a particular formula: take each individual digit, square it,
; and then sum the squares to get a new number. Repeat with the new number and eventually,
; you might get to a number whose squared sum is 1. This is a happy number. An unhappy number
; (or sad number) is one that loops endlessly. Write a function that determines if a number is happy or not.

(defn happy-numbers [x]
  (letfn [(happy [xs]
            (apply + (for [i (str xs)]
                       (#(* % %) (Integer/parseInt (str i))))))
          (happy? [s x]
            (cond
              (= x 1) true
              (contains? s x) false
              :else (recur (conj s x) (happy x))))]
    (happy? #{} x))

  #_(let [summed-square-digits (fn [y]
                               (->> (re-seq  #"\d" (str y))
                                    (map #(Integer/parseInt %))
                                    (map #(* % %))
                                    (reduce +)))
        result (nth (iterate summed-square-digits x) 10)]
    (if (= 1 result)
        true
        false)))

(= (happy-numbers 7) true)
(= (happy-numbers 986543210) true)
(= (happy-numbers 2) false)
(= (happy-numbers 3) false)


; eulers-totient-function

; Two numbers are coprime if their greatest common divisor equals 1. Euler's totient function f(x) is defined as the
; number of positive integers less than x which are coprime to x. The special case f(1) equals 1. Write a function
; which calculates Euler's totient function.

(defn eulers-totient-function [x]
  (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
    (->> (map #(gcd % x) (range 1 (inc x)))
         (filter #{1})
         (count))))

(= (eulers-totient-function 1) 1)
(= (eulers-totient-function 10) (count '(1 3 7 9)) 4)
(= (eulers-totient-function 40) 16)
(= (eulers-totient-function 99) 60)


; power-set

; Write a function which generates the power set of a given set. The power set of a set x is the set of all
; subsets of x, including the empty set and x itself.

(defn power-set [xs]
  (let [n (count xs)
        range-combinations (range (Math/pow 2 n))
        decimal->binary (partial clojure.pprint/cl-format nil "~v,'0B" n)]
    (set (for [y (map decimal->binary range-combinations)]
           (->> (map vector y xs)
                (filter #(= \1 (first %)))
                (map #(second %))
                (set))))))

(= (power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
(= (power-set #{}) #{#{}})
(= (power-set #{1 2 3})
      #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
(= (count (power-set (into #{} (range 10)))) 1024)


; the-balance-of-n

; A balanced number is one whose component digits have the same sum on the left and right halves of the number.
; Write a function which accepts an integer n, and returns true iff n is balanced.

(defn the-balance-of-n [n]
  (let [seq-n (map int (str n))
        halve #(int (/ % 2))
        len (count seq-n)
        half-len (halve len)]
    (->> [(take half-len seq-n) (take-last half-len seq-n)]
         (map #(reduce + %))
         (apply =))))

(= true (the-balance-of-n 11))
(= true (the-balance-of-n 121))
(= false (the-balance-of-n 123))
(= true (the-balance-of-n 0))
(= false (the-balance-of-n 88099))
(= true (the-balance-of-n 89098))
(= true (the-balance-of-n 89089))
(= (take 20 (filter the-balance-of-n (range)))
      [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])


; digits-and-bases

; Write a function which returns a sequence of digits of a non-negative number (first argument) in numerical
; system with an arbitrary base (second argument). Digits should be represented with their integer values, e.g. 15
; would be [1 5] in base 10, [1 1 1 1] in base 2 and [15] in base 16.

(defn digits-and-bases [n b]
  (if (< n b) [n] (conj (digits-and-bases (quot n b) b) (mod n b)))
  #_(let [powers (->> (iterate #(* b %) 1)
                    (take-while #(<= % n))
                    reverse)
        initial-payload {:remaining n
                         :result []}
        update-payload (fn [{:keys [remaining result]} power]
                         {:remaining (rem remaining power)
                          :result (conj result (quot remaining power))})]
    (if (zero? n)
        [0]
        (:result (reduce update-payload initial-payload powers)))))

(= [1 2 3 4 5 0 1] (digits-and-bases 1234501 10))
(= [0] (digits-and-bases 0 11))
(= [1 0 0 1] (digits-and-bases 9 2))
(= [1 0] (let [n (rand-int 100000)](digits-and-bases n n)))
(= [16 18 5 24 15 1] (digits-and-bases Integer/MAX_VALUE 42))


;; identify-keys-and-values

;; Given an input sequence of keywords and numbers, create a map such that each key in the map is a keyword,
;; and the value is a sequence of all the numbers (if any) between it and the next keyword in the sequence.

(defn identify-keys-and-values [[x & xs]]
  (if x
      (assoc
        (identify-keys-and-values (drop-while number? xs))
        x
        (take-while number? xs))
      {}))

(= {} (identify-keys-and-values []))
(= {:a [1]} (identify-keys-and-values [:a 1]))
(= {:a [1], :b [2]} (identify-keys-and-values [:a 1, :b 2]))
(= {:a [1 2 3], :b [], :c [4]} (identify-keys-and-values [:a 1 2 3 :b :c 4]))


;; palindromic-numbers

;; A palindromic number is a number that is the same when written forwards or backwards  (e.g., 3, 99, 14341).
;; Write a function which takes an integer n, as its only argument, and returns an increasing lazy sequence of
;; all palindromic numbers that are not less than n. The most simple solution will exceed the time limit!

(defn palindromic-numbers [n]
  (letfn [(palindrome? [xs] (= (-> xs str seq) (-> xs str seq reverse)))
          (up-range [n] (map #(+ n %) (range)))]
  (filter palindrome? (up-range n))))

;; work in progress

(=  (take 26  (palindromic-numbers 0))
      [0 1 2 3 4 5 6 7 8 9
           11 22 33 44 55 66 77 88 99
               101 111 121 131 141 151 161])

(=  (take 16  (palindromic-numbers 162))
      [171 181 191 202
           212 222 232 242
               252 262 272 282
                   292 303 313 323])

(=  (take 6  (palindromic-numbers 1234550000))
      [1234554321 1234664321 1234774321
           1234884321 1234994321 1235005321])

(=  (first  (palindromic-numbers  (* 111111111 111111111)))
      (* 111111111 111111111))

(=  (set  (take 199  (palindromic-numbers 0)))
      (set  (map #(first  (palindromic-numbers %))  (range 0 10000))))

(= true
      (apply <  (take 6666  (palindromic-numbers 9999999))))

(=  (nth  (palindromic-numbers 0) 10101)
      9102019)
