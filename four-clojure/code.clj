;; I believe coding practice is all about recognizing patterns
;; in your problems and translating the solutions into the 
;; appropriate usage of a certain language.
;; when learning a new language, learning from others code 
;; is equally important coming up with your own solution

;; this is a collection of problems & solutions collected from 
;; www.4clojure.com -- in problem order

;; problem 1, 2 - ignored
;; problem 3 - ignored (clojure strings are java strings)
;; problem 4 - ignored
;; problem 5 - ignored (cons, conj - like push, insert, add in python)
;; problem 6 - ignored (vectors and lists are comparable in clj through =)
;; vector can be constructed in different ways in clj:
;; e.g. (list 1 2 3) ;; actually a list
;; (vec '(1 2 3)) ;; casting
;; (vector 1 2 3) ;; multi artity version

;; problem 7 
(assert (= (conj [1 2] 3 4) '(1 2 3 4)))

;; problem 8 - ignored (set operations are in different package
;; e.g., clojure.set/union)
;; problem 9 - ignored
;; problem 10 - ignored

;; problem 11 -- use conj / cons with map
(assert (= '([1 2] [3 4]) (cons [1 2] {3 4})))
(assert (= {1 2, 3 4} (conj {3 4} [1 2])))

;; problem 12, 13, 14, 15 - ignored

;; problem 16 - string concat
(def ?? (fn [name] (str "Hello, " name \!)))
(assert (= "Hello, Dave!" (?? "Dave")))

;; problem 17 - ignored

;; problem 18 - difference between some and filter
;; compared to filter, "some" is an shortcut operation
;; similiar to "next" in python
(assert (= [1 3] (filter odd? '(1 2 3 4))))
(assert (= 1 (some #(when odd? % %) '(1 2 3 4))))

;; problem 19 - Write a function which returns the last element in a sequence.
;; NO Use of "last" - find the same meaning in the functional world
;; solution 1 - reverse and first
(def ?? (comp first reverse))
(assert (= (?? [1 2 3 4 5]) 5))
;; solution 2 - reduce (cheapest reduce is a loop)
(def ?? (partial reduce #(-> %2))) ;; use -> to return scalar
(assert (= (?? [1 2 3 4 5]) 5))

;; problem 20- Write a function which returns the second to last element from a sequence.
;; Penultimate Element
;; the challenge here is to make it efficient to access
;; variables from the end
;; NOTICE the relation between -> and comp
;; (comp last drop-last) returns a funciton
;; #(-> % drop-last last) constructs a function
(def ?? (comp last drop-last))
(assert (= (?? (list 1 2 3 4 5)) 4))
(def ?? #(-> % drop-last last))
(assert (= (?? (list 1 2 3 4 5)) 4))

;; problem 21 
;; get the nth element without using "nth" - always starting from 0
;; a good place to review drop-series in clj
;; (drop n coll) => lazy seq of all but dropping the first n
;; (drop-last n coll) => lazy seq of all but dropping the last n
;; (drop-while pred coll) => lazy seq starting from the first elem that pred predicts false
;; SIMILIARLY, ON THE OTHER SIDE
;; (take n coll) => lazy seq of first nth or all (if n > len)
;; (take-last n coll) => return a seq of last n, may not be lazy seq
;; (take-while pred coll) => lazy seq of successive items while pred predicts true
;; *** (take-nth n coll) => a lazy seq of every nth item in coll
(def ?? #(first (drop %2 %1)))
(assert (= (?? '(4 5 6 7) 2) 6))

;; problem 22
;; total number of elements in a seq without using "count"
(def ?? #(reduce + (map (constantly 1) %)))
(assert (= (?? '(1 2 3 3 1)) 5))
(def ?? #(.size (vec %)))
(assert (= (?? '(1 2 3 3 1)) 5))

;; problem 23 - reverse a seq without using "reverse" or "rseq"
;; DIFFERENCE between "reverse" and "rseq"
;; (reverse coll) => not lazy
;; (rseq coll) => returns in contant time (lazy)
;; REVERSE - it is usually RELATED to the use of list
;; as it plays like a stack. a vector is more like a queue
(def ?? #(into '() %))
(assert (= (?? [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]]))
(def ?? (partial reduce #(conj %1 %2) '()))
(assert (= (?? [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]]))

;; problem 24, 25 - ignored

;; problem 26 - first n fibonacci sequence
;; using lazy-seq (which is usually implemented as RECURSION)
(def ?? #(take % ((fn fib
                    ([] (fib 1 1)) ;; pattern for DEFAULT params
                    ([a b] (cons a ;; INFINITE GENERATOR
                        (lazy-seq (fib b (+ a b)))))))))
(assert (= (?? 8) '(1 1 2 3 5 8 13 21)))
;; it should be simpler - like in others solution
(def ?? #(take % ((fn fib [x y] (lazy-seq (cons x (fib y (+ x y))))) 1 1)))
(assert (= (?? 8) '(1 1 2 3 5 8 13 21)))
;; or use an iterator
;; LIKE IN PYTHON - GENERATOR v.s. ITERATOR pattern
;; and how to use iterate with multiple parameters
(def ?? #(take % (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))
(assert (= (?? 8) '(1 1 2 3 5 8 13 21)))
;; some guys solution - LOL
;; that makes a point how easy an overfitting solution can be invented.
(def ?? (fn [i] (take i '(1 1 2 3 5 8 13 21))))
(assert (= (?? 8) '(1 1 2 3 5 8 13 21)))

;; problem 27 - palindrome
;; use seq as general type
(def ?? #(let [s (seq %) r (reverse %)] (= s r)))
(assert (false? (?? '(1 2 3 4 5))))
(assert (true? (?? "racecar")))

;; problem 28 - flatten nested sequence without using "flatten"
;; it is the depth-first walk of tree
;; not the difference between seq? and sequential?
;; seq? is MORE SPECIFIC ABOUT testing if it is a '(x y z...)
;; sequential? is MORE GENERAL in the sense that '(..) [..] are all sequentials
(def ?? #(filter (complement sequential?) (tree-seq sequential? identity %)))
(assert (= (?? '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))

;; problem 29 -- get the caps
;; Write a function which takes a string and returns a new string containing only the capital letters.
;; a good chance to practice regular expression
;; re-seq - findall in python
(def ?? #(apply str (re-seq #"[A-Z]" %)))
(assert (= (?? "$#A(*&987Zf") "AZ"))

;; problem 30 - Write a function which removes consecutive duplicates from a sequence.
;; the key is to realized that IT IS An adaptive Partition problem, use partition-by
(def ?? #(map first (partition-by identity %)))
(assert (= (?? [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))


(println "all tests passed ...")
