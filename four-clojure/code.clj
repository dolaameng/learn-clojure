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

;; problem 31 - pack a sequence
;; Write a function which packs consecutive duplicates into sub-lists.
;; the same partition problem as the previous one
(def ?? #(partition-by identity %))
(assert (= (?? [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))

;; problem 32 - duplicate a sequence 
;; Write a function which duplicates each element of a sequence.
;; typcial map-cat pattern - a special of case of map-reduce with 
;; reduce = concat
;; *** A VERY EASY-TO-MAKE MISTAKE FOR ME IS TO FORGET USE PARENTHESIS
;; TO ENCOMPASS A FUNCTION CALL IN THE LIST FORM, E.G.
;; IT SHOULD BE (def ?? (fn [xs] (mapcat #(repeat 2 %) xs)))
;; BUT I COME UP WITH (def ?? (fn [xs] mapcat #(repeat 2 %) xs))
;; THIS IS ACTUALLY EQUIVALENT TO CALL "mapcat" (without evaluation)
;; and then call anonymous function #(..) (without evaluation)
;; and then evaluate xs and return
(def ?? (fn [xs] (mapcat #(repeat 2 %) xs)))
(assert (= (?? [1 2 3]) '(1 1 2 2 3 3)))
;; another solution is to recognize it as an INTERLEAVING pattern
;; but as easily generalizable as the mapcat pattern - see problem 33 below
(def ?? #(interleave % %))
(assert (= (?? [1 2 3]) '(1 1 2 2 3 3)))

;; problem 33 - generalization of problem 32
;; Write a function which replicates each element of a sequence a variable number of times.
(def ?? (fn [xs n] (mapcat #(repeat n %) xs)))
(assert (= (?? [:a :b] 4) '(:a :a :a :a :b :b :b :b)))

;; problem 34 - implement range with using range
;; Pattern - range is itself a lazy-sequence - with a terminating (base) case
(def ?? (fn fromto [start end] (if (= start end) 
                                    () 
                                    (lazy-seq (cons start (fromto (inc start) end))))))
(assert (= (?? 1 4) '(1 2 3)))
;; OF COURSE, every lazy-seq (generator in python) has a corresponding iterator solution
;; use take-while (or take series) combined with iterate to get finite seq
(def ?? (fn [start end] (take-while #(< % end) (iterate inc start))))
(assert (= (?? 1 4) '(1 2 3)))
;; a smart solution from others
(def ?? (fn [start end] (map-indexed + (repeat (- end start) start))))
(assert (= (?? -2 2) '(-2 -1 0 1)))

;; problem 35, 36, 37 - ignored

;; problem 38 - find max without using "max", "max-key"
;; first - the diff between "max" and "max-key", e.g.
;; (max 1 2 3) => 3
;; (max-key {:one 1 :two 2 :three 3} :one :two :three) => :three
;; "max-key" implements the pattern of COMPARISON BY VALUES
;; MAX (or any linear comparsion pattern) can be implemented by REDUCE
(def ?? (fn [& xs] (reduce (fn [sofar x] (if (> sofar  x) sofar x)) xs)))
(assert (= (?? 1 8 3 4) 8))
;; or you can always sort it in O(nlogn) first
(def ?? (comp last sort list))
(assert (= (?? 1 8 3 4) 8))

;; problem 39 - interleave two sequences without using "interleave"
;; INTERLEAVING is an important pattern allowing function to take
;; inputs from different sources - LIKE zip in PYTHON
;; on the other hand is the JUXTAPOSITION pattern, which takes different
;; functions from different sources on the same data source, and return
;; them in a vector result
;; THE TRICK is that in clj, "map" is already able to suck from 
;; different sources in a parallel way
(def ?? (fn [& colls] (apply (partial mapcat list) colls)))
(assert (= (?? [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
(assert (= (?? [1 2] [3 4 5 6]) '(1 3 2 4)))

;; problem 40 - interpose a seq - interpolate in python
;; Write a function which separates the items of a sequence by an arbitrary value.
;; it is essentially another INTERLEAVING pattern
(def ?? (fn [s coll] (drop-last (interleave coll (repeat s)))))
(assert (= (?? :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))

;; problem 41 - drop every nth item
;; Write a function which drops every Nth item from a sequence.
;; Pattern accessing based on the index of data
;; NTH - related functions in clojure
;; (1) nth, nthnext nthrest  -- access a single nth element
;; (2) take-nth [n coll] -- return a lazy seq of every nth element in coll
;; (3) useful.seq/map-nth [f n coll] -- call f on every nth of coll
;; the most fundamental way of doing accessing pattern based on index
;; is to use index-pattern such as keep-indexed or map-indexed
;; the diff between keep-indexed and map-indexed is that though they
;; both call a function on (index, element) tuple, keep-indexed only keep
;; the elements that are NOT nil
(def ?? (fn [coll n] (keep-indexed (fn [i x] (if (pos? (rem (inc i) n)) x nil)) coll)))
(assert (= (?? [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
;; another solution is based on PARTITIONING
;; where we can see how general the PARTITIONING pattern can be
;; IT SEEMS Partitioning (split/partition) turns out to be 
;; a very useful and general pattern
(def ?? (fn [coll n] (mapcat (partial take (dec n)) (partition-all n coll))))
(assert (= (?? [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))

;; problem 42 - calculate the factorial of n
;; factorial can implemented by reduce
(def ?? #(->> % inc (range 1) (reduce *)))
(assert (= (?? 8) 40320))

;; problem 43 - Reverse Interleave
;; Write a function which reverses the interleave process into x number of subsequences.
;; the reverse of interleave is the juxtaposition pattern
;; Deeply they are more related to transpose of matrices
;; it is a matrix transpose in essence (map list ...)
(def ?? (fn [coll n] (apply map list (partition n coll))))
(assert (= (?? [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
(assert (= (?? (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))

;; problem 44 - rotate a sequence
;; Write a function which can rotate a sequence in either direction.
;; in python rotation is usually done by using slicing
;; in clojure, this is done through (split) or (juxt drop take)
;; split can be viewed as a special case of partition - only two parts
(def ?? (fn rotate [n coll] 
  (let [len (count coll)
        nn (rem n len)
        offset (if (pos? nn) nn (+ len nn))
        [lhs rhs] (split-at offset coll)]
        (concat rhs lhs))))
(assert (= (?? 2 [1 2 3 4 5]) '(3 4 5 1 2)))
(assert (= (?? -2 [1 2 3 4 5]) '(4 5 1 2 3)))

;; problem 45 - equavilance of "iterate" and "lazy seq"
(def ?? ((fn s [x0] (lazy-seq (cons x0 (s (+ 3 x0))))) 1))
(assert (= (take 5 ??) (take 5 (iterate #(+ 3 %) 1))))

;; problem 46 - flipping out
;; Write a higher-order function which flips the order of the arguments of an input function.
;; pattern - return another function as the return value
(def ?? (fn [f] (fn [& args] (apply f (reverse args)))))
(assert (= 3 ((?? nth) 2 [1 2 3 4 5])))

;; problem 47 - contain yourself
;; The contains? function checks if a KEY is present in a given collection.
;; This often leads beginner clojurians to use it incorrectly with 
;; numerically indexed collections like vectors and lists.
;; see the quenstion at http://stackoverflow.com/questions/14292324/how-to-understand-contains-applied-to-a-list-in-clojure
;; ignored

;; problem 48 - intro to some
;; The some function takes a predicate function and a collection. It returns the first logical true value of (predicate x) 
;; where x is an item in the collection.
;; the main difference between "some" and "filter":
;; "some" is a shortcut evaluation - returns the first true logical value
;; "filter" is always working in O(n) time - return a lazy seq of items where pred is true
;; One of the most typical pattern with "some" is to use a set/map as the pred
;; to test the membership, e.g.
(assert (= 6 (some #{2 7 6} [5 6 7 8])))
(assert (= [6 7] (filter #{2 7 6} [5 6 7 8])))
(assert (= 6 (some #(when (even? %) %) [5 6 7 8])))
(assert (= [6 8] (filter #(when (even? %) %) [5 6 7 8])))

;; problem 49 - Split a sequence
;; Write a function which will split a sequence into two parts
;; without using "split-at"
(def ?? (juxt take drop))
(assert (= (?? 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))

;; problem 50 - split by type
;; Write a function which takes a sequence consisting of items with different 
;; types and splits them up into a set of homogeneous sub-sequences. 
;; The internal order of each sub-sequence should be maintained, 
;; but the sub-sequences themselves can be returned in any order 
;; (this is why 'set' is used in the test cases).
;; THIS IS the CLUSTERING pattern, which is more general than
;; PARTITIONING pattern (partition & split), the main diff is that in PARTITIONING
;; pattern, the splits always happen at a boundary in the data set
;; where in CLUSTERING, arbitrary boundaries (groups) can be formed
(def ?? #(set (vals (group-by class %))))
(assert (= (set (?? [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))

;; problem 51 - Advanced Destructuring
;; Here is an example of some more sophisticated destructuring.
(assert (= [1 2 [3 4 5] [1 2 3 4 5]] 
    (let [[a b & c :as d] [1 2 3 4 5]] [a b c d])))
    
;; problem 52 - passed

;; problem 53 - Longest Increasing Sub-Seq
;; Given a vector of integers, find the longest consecutive sub-sequence of 
;; increasing numbers. If two sub-sequences have the same length, 
;; use the one that occurs first. 
;; An increasing sub-sequence must have a length of 2 or greater to qualify.
;; PATTERN - linear search with one-step look-back/look-forward
;; ONE-STEP LOOK-BACK or LOOK-FORWARD is so common in practice, 
;; they have a built-in implementation in it - useful.seq/partition-between
;; PATTERNS: "partition", "partition-by", "partition-between" 
;; THE TRICK: for one-step-look-forward pattern, construct the pairs of seq,
;; (partition n step)
;; partition the pairs of seq based on their relative order,
;; (partition-by)
;; pick (by mapping) the second of each pair 
;; (nested map)
(def ?? (fn [xs] (filter #(< 1 (count %)) 
    (partition-by (fn [[l r]] (= (inc l) r)) (partition 2 1 xs)))))
(assert (= (?? [1 0 1 2 3 0 4 5]) [0 1 2 3]))
(assert (= (?? [5 6 1 3 2 7]) [5 6]))
(assert (= (?? [2 3 3 4 5]) [3 4 5]))
(assert (= (?? [7 6 5 4]) []))




(println "all tests passed ...")
