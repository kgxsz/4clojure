; ELEMENTARY

; problem 1

(= true true)

; problem 2

(= (- 10 (* 2 3)) 4)

; problem 3

(= "HELLO WORLD" (.toUpperCase "hello world"))

; problem 4

(= (list :a :b :c) '(:a :b :c))

; problem 5

(def problem-5 '(1, 2, 3, 4))

(= problem-5 (conj '(2 3 4) 1))
(= problem-5 (conj '(3 4) 2 1))

; problem 6

(= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))

; problem 7

(def problem-7 [1 2 3 4])

(= problem-7 (conj [1 2] 3 4))
(= problem-7 (conj [1 2 3] 4))

; problem 8

(def problem-8 #{:a :b :c :d})

(= problem-8 (set '(:a :a :b :c :c :c :c :d :d)))
(= problem-8 (clojure.set/union #{:a :b :c} #{:b :c :d}))

; problem 9

(= #{1 2 3 4} (conj #{1 4 3} 2))

; problem 10

(def problem-10 20)

(= problem-10 ((hash-map :a 10, :b 20, :c 30) :b))
(= problem-10 (:b {:a 10, :b 20, :c 30}))

; problem 11

(= {:a 1, :b 2, :c 3} (conj {:a 1} {:b 2} [:c 3]))

; problem 12

(def problem-12 3)

(= problem-12 (first '(3 2 1)))
(= problem-12 (second [2 3 4]))
(= problem-12 (last (list 1 2 3)))

; problem 13

(= [20 30 40] (rest [10 20 30 40]))

;problem 14

(def problem-14 8)

(= problem-14 ((fn add-five [x] (+ x 5)) 3))
(= problem-14 ((fn [x] (+ x 5)) 3))
(= problem-14 (#(+ % 5) 3))
(= problem-14 ((partial + 5) 3))

;problem 15

(defn problem-15 [x] (* 2 x))

(= (problem-15 2) 4)
(= (problem-15 3) 6)
(= (problem-15 11) 22)
(= (problem-15 7) 14)

; problem 16

(defn problem-16 [name] (str "Hello, " name "!"))

(= (problem-16 "Dave") "Hello, Dave!")
(= (problem-16 "Jenn") "Hello, Jenn!")
(= (problem-16 "Rhea") "Hello, Rhea!")

; problem 17

(= '(6 7 3) (map #(+ % 5) '(1 2 3)))

; problem 18

(= '(6 7) (filter #(> % 5) '(3 4 5 6 7)))

; problem 19

(def problem-19 7)

(= problem-19 (let [x 5] (+ 2 x)))
(= problem-19 (let [x 3, y 10] (- y x)))
(= problem-19 (let [x 21] (let [y 3] (/ x y))))

; problem 20

(= 10 (let [x 7 y 3 z 1] (+ x y)))
(= 4 (let [x 7 y 3 z 1] (+ y z)))
(= 1 (let [x 7 y 3 z 1] z))

; problem 21

(= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))

; problem 22

(def problem-22 +)

(= 15 (reduce problem-22 [1 2 3 4 5]))
(=  0 (reduce problem-22 []))
(=  6 (reduce problem-22 1 [2 3]))

; problem 23

(= '(5 4 3 2 1) ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))

; problem 24

(= (last (sort (rest (reverse [2 5 4 1 3 6]))))
   (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (last))
   5)

; problem 25

(= [7 6 5 4 3]
  (loop [x 5
         result []]
    (if (> x 0)
      (recur (dec x) (conj result (+ 2 x)))
      result)))

; problem 26

(= (reduce + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (reduce +))
   11)

; problem 27

(defn problem-27 [a b]
  (and
   (contains? b a)
   (= nil (a b))))

(true?  (problem-27 :a {:a nil :b 2}))
(false? (problem-27 :b {:a nil :b 2}))
(false? (problem-27 :c {:a nil :b 2}))

; problem 28

(def problem-28 '(1 5 9 13 17 21 25 29 33 37))


(= problem-28 (for [x (range 40)
                    :when (= 1 (rem x 4))]
                x))

(= problem-28 (for [x (iterate #(+ 4 %) 0)
                :let [z (inc x)]
                :while (< z 40)]
                z))

(= problem-28 (for [[x y] (partition 2 (range 20))]
        (+ x y)))

; problem 29

(def problem-29 1)

(= problem-29 (if-not false 1 0))
(= problem-29 (if-not nil 1 0))
(= problem-29 (if true 1 0))
(= problem-29 (if [] 1 0))
(= problem-29 (if [0] 1 0))
(= problem-29 (if 0 1 0))
(= problem-29 (if 1 1 0))

; problem 30

(def problem-30 #{2 1})

(clojure.set/superset? problem-30 #{2})
(clojure.set/subset? #{1} problem-30)
(clojure.set/superset? problem-30 #{1 2})
(clojure.set/subset? #{1 2} problem-30)

; problem 31

(defn problem-31 [value collection]
  (zipmap collection (repeat value)))

(= (problem-31 0 [:a :b :c]) {:a 0 :b 0 :c 0})
(= (problem-31 "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})
(= (problem-31 [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})
