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