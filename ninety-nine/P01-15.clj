(ns ninety-nine
    (:use clojure.test))
    
; the last element    
(defn p01 [coll]
    (last coll))
    
; the last but one of a list
(defn p02 [coll]
    (last (drop-last coll)))
    
; find the kth element of a list
(def p03 nth)

; find the number of elements of a list
(def p04 count)

; reverse a list
(def p05 reverse)

; find out whether a list is a palindrome
(defn p06 [col]
    (= col (reverse col)))
    
; flatten a nested list structure
; transform a list, possibly holding lists as elements into a flat list by 
; replacing each list with its elements recursively
; e.g., (p07 '(a (b (c d) e))) => (a b c d e)
(def p07 flatten)

; eliminate consecutive duplicates of list elements
; e.g. (compress '(a a a a b c c a a d e e e e e)) => (a b c a d e)
(defn p08 [coll]
    (reduce (fn [col elem] (if (= elem (last col)) col (conj col elem))) [] coll))
    
; pack consecutive duplicates of list elements into sublists
; if a list contains repeated elements they should be placed in separate sublists
; e.g. (pack '(a a a a b c c a a d e e e e))
; '((a a a a ) (b) (c c) (a a) (d) (e e e e))
(def p09 (partial partition-by identity))

; run-length encoding of a list
; consecutive duplicates of elements are encoded as tuples (N, E) where 
; N is the number of dplicates of the element E. 
; e.g. (encode (seq "aaaabccaadeeee")) => ((4 \a) (1 \b) (2 \c) (2 \a) (1 \d) (4 \e))
(defn p10 [xs]
    (let [x (partition-by identity xs)]
        (map list (map count x) (map first x))))
        
;; modify the result of p10 in such a way that if an element has no duplicates 
;; it is simply copied into the result list. Only elements with duplicates 
;; are transfereed as (N, E) terms.
;; e.g. (encode "aaaabccaadeeee") => ((4 \a) (\b) (2 \c) (2 \a) (1 \d) (4 \e))
(defn p11 [xs]
    (let [partitioned (partition-by identity xs)]
        (map #(if (= 1 (count %)) % (list (count %) (first %))) 
            partitioned)))
            
;; Given a run-length code list generated as specified in problem p10, construt
;; its uncompressed version. 
;; e.g. (decode '((4 :a) (1 :b) (2 :c) (2 :a) (1 :d) (4 :e)) ) => 
;; '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e)
(defn p12 [xs]
    (flatten (map #(let [n (first %) x (last %)] (repeat n x)) 
            xs)))
            
;; p13 ignored

;; duplicate the elements of a list
;; e.g. (duplicate '(:a :b :c :c :d)) => '(:a :a :b :b :c :c :c :c :d :d)
(defn p14 [xs]
    (flatten (map #(repeat 2 %) xs)))

;; p15 ignored

;; drop every N'th element from a list
;; e.g. (drop '(a b c d e f g h i k) 3) => '(a b d e g h k)
(defn p16 [xs, n]
    (map second (filter #(not= (dec n) (rem (first %) n)) (keep-indexed #(list %1 %2) xs))))

(deftest p1-15
    (is (= :d (p01 '(:a :b :c :d))))
    (is (= :c (p02 '(:a :b :c :d))))
    (is (= :b (p03 '(:a :b :c :d) 1)))
    (is (= 4 (p04 '(:a :b :c :d))))
    (is (= '(:d :c :b :a) (p05 '(:a :b :c :d))))
    (is (p06 (seq "xamax")))
    (is (not (p06 (seq "amax"))))
    (is (= '(:a :b :c :d :e) (p07 '(:a (:b (:c :d) :e)))))
    (is (= '(:a :b :c :a :d :e) (p08 '(:a :a :a :a :b :c :c :a :a :d :e :e))))
    (is (= '((:a :a :a :a) (:b) (:c :c) (:a :a) (:d) (:e :e :e :e))  
            (p09 '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e))))
    (is (= '((4 :a) (1 :b) (2 :c) (2 :a) (1 :d) (4 :e)) 
            (p10 '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e))))
    (is (=  '((4 :a) (:b) (2 :c) (2 :a) (:d) (4 :e)) 
            (p11 '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e))))
    (is (= '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e) 
            (p12 '((4 :a) (1 :b) (2 :c) (2 :a) (1 :d) (4 :e)))))
    (is (= '(:a :a :b :b :c :c :c :c :d :d)
            (p14 '(:a :b :c :c :d))))
    (is (= '(:a :b :d :e :g :h :k) 
            (p16 '(:a :b :c :d :e :f :g :h :i :k) 3)))
)


    
(run-tests)
    
