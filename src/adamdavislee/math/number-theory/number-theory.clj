(ns adamdavislee.math.number-theory
  "This port closely mimics the naming conventions and coding style of the racket source code.
  However it does not:
    Define inline versions of mod+, mod*, etcetera.
  TODO:
    Add pre/post conditions and assertions with type checking etc.
    Improve docstrings.
    Finish marking internal functions as private.
    Use internal memoization for some of the functions which racket uses manual bit manipulation for."
  (:require [clojure.test
             :refer [is
                     are
                     with-test
                     successful?
                     run-tests]]))

(do
  (defmacro
    ^:private
    defval
    "like def, but returns value instead of var"
    [sym value]
    `(do (def ~sym ~value) ~sym))
  (with-test
    #'adamdavislee.math.number-theory/defval
    (is
     (and (keyword? (defval a :a))
          (var? (def a :a))
          (do (defval b :b)
              (= b :b))))))

(with-test
  (defn
    assert
    [value predicate & [error-message]]
    (if (predicate value) value error-message))
  (is (nil? (assert "" int?)))
  (is (= "oops!" (assert "" int? "oops!")))
  (is (= "" (assert "" string?))))

(with-test
  (defn intvalue?
    [n]
    (== n (int n)))
  (are
      [a b]
      (= (intvalue? a) b)
      1.2
      false
      1.0
      true
      -3.0
      true
      -3.3
      false
      -2
      true))

(with-test
 (defn- abs "absolute value" [a] (if (neg? a) (- a) a))
 (is (= (abs -1) 1))
 (is (= (abs 1) 1)))

(with-test
 (defn-
  pow
  ([] 1)
  ([n] n)
  ([a b]
   (if
    (intvalue? b)
    ((if (neg? b) / *) (apply * (repeat (abs b) a)))
    (Math/pow a b)))
  ([a b & numbers]
   (let [numbers (conj numbers b a)] (reduce pow numbers))))
 (are
  [a b]
  (= (double (apply pow a)) (double b))
  []
  1
  [2]
  2
  [2 2]
  4
  [2 2 2]
  16
  [2 3 4]
  (pow (pow 2 3) 4)
  [3 -1]
  1/3
  [4 -2]
  1/16))

(with-test (defn- rt ([a b] (pow a (/ b)))) (is (= (rt 8 3) 2.0)))

(with-test
 (def
  one?
  "imo it's reasonable to introduce vocabulary for 0 1 and 2"
  (partial = 1))
 (are [a b] (= (apply one? a) b) [1] true [2] false))

(with-test (def duple (partial * 2)) (is (= 6 (duple 3))))

(with-test (def two? (partial = 2)) (is (not (two? 1))) (is (two? 2)))

(with-test (defn- half [n] (/ n 2)) (is (= (half 4) 2)))

(with-test
 (defn- sqrt [n] (Math/sqrt n))
 (is (= (sqrt 1) (double 1))))

(with-test (defn- sq [n] (pow n 2)) (is (= (sq 3) 9)))

(with-test
 (defn-
  gcd
  "uses the euclidean algorithm, see https://brilliant.org/wiki/euclidean-algorithm"
  ([] 0)
  ([a] a)
  ([a b] (if (zero? b) (abs a) (recur b (rem a b))))
  ([a b & numbers] (gcd a (apply gcd (conj numbers b)))))
 (are
  [a b]
  (= (apply gcd a) b)
  []
  0
  [3]
  3
  [2 4]
  2
  [3 4]
  1
  [-2 4]
  2
  [3 4 5]
  1))

(with-test
 (defn-
  inrange
  "range with an inclusive end and default beginning of 1"
  ([] (drop 1 (range)))
  ([end] (range 1 (inc end)))
  ([start end] (range start (inc end)))
  ([start end step] (range start (inc end) step)))
 (are
  [a b]
  (= (apply inrange a) b)
  [8]
  [1 2 3 4 5 6 7 8]
  [2 4]
  [2 3 4]))

(with-test
 (defn- andmap [& args] (every? identity (apply map args)))
 (are
  [a b]
  (= (apply andmap a) b)
  [pos? []]
  true
  [neg? [-2 -1]]
  true
  [neg? [-2 -1 0 1 2]]
  false))

(def ^:private fact-table-size 171)

(def
 fact-table
 (vec
  (reverse
   (reduce
    (fn [numbers n] (cons (* n (first numbers)) numbers))
    [1N]
    (inrange fact-table-size)))))

(def ^:private simple-cutoff 244)

(with-test
 (defn-
  factorial-simple
  [n]
  (if
   (< n fact-table-size)
   (fact-table n)
   (* n (factorial-simple (dec n)))))
 (is
  (andmap
   (fn [n] (= (factorial-simple n) (apply * (inrange n))))
   (inrange 8))))

(with-test
 (defn divides? [a b] (if (zero? a) false (= 0 (rem b a))))
 (are [a b] (divides? a b) 2 4 2 12 2 0)
 (are [a b] (not (divides? a b)) 0 1 2 3 2 13))

(with-test
 (defn coprime? [a & numbers] (one? (apply gcd (conj numbers a))))
 (are
  [a b]
  (= (apply coprime? a) b)
  [1]
  true
  [2]
  false
  [2 9]
  true
  [2 4 8]
  false
  [2 4 9]
  true)
 (is (coprime? (* 3 7) (* 5 19)))
 (is (not (coprime? (* 3 7 5) (* 5 19 2)))))

(with-test
 (defn
  pairwise-coprime?
  [a & numbers]
  (or
   (nil? numbers)
   (and
    (if
     (every? identity (map (fn [b] (coprime? a b)) numbers))
     true
     false)
    (if (apply pairwise-coprime? numbers) true false))))
 (are
  [a b]
  (= (apply pairwise-coprime? a) b)
  [1]
  true
  [1 2]
  true
  [1 2 4]
  false
  [1 2 -4]
  false)
 (are [a] (not (apply pairwise-coprime? a)) [10 7 33 14] [6 10 15])
 (is (pairwise-coprime? 10 7 33 13)))

(with-test
 (defn
  bezout
  "uses the extended euclidean algorithm, see: https://brilliant.org/wiki/extended-euclidean-algorithm"
  ([a] 1)
  ([a b]
   (let
    [loop
     (fn
      [a b ua va ub vb]
      (let
       [q (quot a b) r (rem a b)]
       (if
        (= r 0)
        (list ub vb)
        (recur b r ub vb (- ua (* q ub)) (- va (* q vb))))))
     start
     (fn [a b] (if (> a b) (loop a b 1 0 0 1) (loop b a 0 1 1 0)))]
    (cond
     (and (pos? a) (pos? b))
     (start a b)
     (and (neg? a) (neg? b))
     (mapv - (start (- a) (- b)))
     (and (neg? a) (pos? b))
     (let
      [k (+ (quot (- a) b) 1) [u v] (start (+ a (* k b)) b)]
      [u (+ (* u k) v)])
     (and (pos? a) (neg? b))
     (let
      [k (+ (quot (- b) a) 1) [u v] (start a (+ (* k a) b))]
      (list (+ u (* k v)) v)))))
  ([a b & numbers]
   (let
    [numbers
     (conj numbers b a)
     [s t]
     (bezout (apply gcd (rest numbers)) (first numbers))]
    (conj (map (partial * s) (apply bezout (rest numbers))) t))))
 (are
  [a b]
  (and (= (apply bezout a) b) (= (apply gcd a) (apply + (map * a b))))
  [4 2]
  [0 1]
  [2 4]
  [1 0]
  [2 -4]
  [1 0]
  [-2 4]
  [1 1]
  [-2 -4]
  [-1 0]
  [3 4]
  [-1 1]
  [2 4 8]
  [1 0 0]
  [2 4 7]
  [0 2 -1])
 (are
  [a]
  (= (apply gcd a) (apply + (map * (apply bezout a) a)))
  [12 20]
  [12 20]
  [20 16]
  [12 20 16]))

(with-test
 (defn modular-inverse [a n] (mod (first (bezout a n)) n))
 (are [a b] (= (apply modular-inverse a) b) [3 7] 5)
 (andmap
  (fn
   [n]
   (let
    [m (and (coprime? n 20) (modular-inverse n 20))]
    (if m (= (rem (* n m) 20) 1) true)))
  (inrange 20)))

(with-test
 (defn
  solve-chinese
  [as ns]
  (let
   [n
    (apply * ns)
    cs
    (map (fn [ni] (quot n ni)) ns)
    ds
    (map modular-inverse cs ns)]
   (mod (apply + (map * as cs ds)) n)))
 (are [a b] (= (apply solve-chinese a) b) [[2 3 2] [3 5 7]] 23))

(with-test
 (defn
  mediant
  [a b]
  (/
   (+ (numerator a) (numerator b))
   (+ (denominator a) (denominator b))))
 (are [a b] (= (apply mediant a) b) [1/2 5/6] 3/4 [-1/2 -2/3] -3/5))

(with-test
 (defn
  perfect-square?
  [a]
  (let [a-sqrt (int (sqrt a))] (if (= (pow a-sqrt 2) a) a-sqrt false)))
 (are [a b] (= (apply perfect-square? a) b) [4] 2 [5] false))

(with-test
 (defn triangle-number [n] (quot (* n (inc n)) 2))
 (are [a b] (= (apply triangle-number a) b) [1] 1 [2] 3)
 (is (= (mapv triangle-number (inrange 0 5)) [0 1 3 6 10 15])))

(with-test
 (defn square-number? [n] (and (perfect-square? n) true))
 (is (andmap square-number? [0 1 4 9 16 25]))
 (is (andmap (complement square-number?) [2 5 10 17 26])))

(with-test
 (defn pentagonal-number [n] (quot (* n (- (* 3 n) 1)) 2))
 (is (= (mapv pentagonal-number [0 1 2 3 4 5]) [0 1 5 12 22 35])))

(with-test
 (defn hexagonal-number [n] (* n (- (* 2 n) 1)))
 (is (= (mapv hexagonal-number [0 1 2 3 4 5]) [0 1 6 15 28 45])))

(with-test
 (defn heptagonal-number [n] (quot (* n (- (* 5 n) 3)) 2))
 (is (= (mapv heptagonal-number [0 1 2 3 4 5]) [0 1 7 18 34 55])))

(with-test
 (defn octagonal-number [n] (* n (- (* 3 n) 2)))
 (is (= (mapv octagonal-number [0 1 2 3 4 5]) [0 1 8 21 40 65])))

(with-test
 (defn
  quadratic-solutions
  "return list of solutions to a x^2 + b x + c = 0"
  [a b c]
  (let
   [d (- (* b b) (* 4 a c))]
   (cond
    (< d 0)
    []
    (= d 0)
    [(/ b (* -2 a))]
    :else
    (let
     [sqrt-d (sqrt d)]
     [(/ (- (- b) sqrt-d) (* 2 a)) (/ (+ (- b) sqrt-d) (* 2 a))]))))
 (are
  [a]
  a
  (= (quadratic-solutions 1 0 -4) [-2.0 2.0])
  (= (quadratic-solutions 1 0 4) [])
  (= (quadratic-solutions 1 0 0) [0])))

(with-test
 (defn exact-integer? [n] (= (double n) (double (int n))))
 (is (exact-integer? 1.0))
 (is (not (exact-integer? 1.1))))

(with-test
 (defn
  quadratic-integer-solutions
  [a b c]
  "return list of integer solutions to a x^2 + b x + c = 0"
  (filter exact-integer? (quadratic-solutions a b c)))
 (are
  [a b]
  (= (apply quadratic-integer-solutions a) b)
  [1 0 -4]
  [-2.0 2.0]
  [1 0 4]
  []
  [1 0 0]
  [0]))

(with-test
 (defn
  natural?
  [n]
  (every?
   identity
   ((juxt
     exact-integer?
     (fn [n] (some identity ((juxt pos? zero?) n))))
    n)))
 (are [a] (natural? a) 0 1 2 3 4)
 (are [a] (not (natural? a)) -1 -2 -3 -4))

(with-test
 (defn
  quadratic-natural-solutions
  "return list of nonnegative-integer solutions to a x^2 + b x + c = 0"
  [a b c]
  (filter natural? (quadratic-solutions a b c)))
 (are
  [a b]
  (= (apply quadratic-natural-solutions a) b)
  [1 0 -4]
  [2.0]
  [1 0 4]
  []
  [1 0 0]
  [0]))

(with-test
 (defn
  tangent-number
  "the nth tangent number, http://mathworld.wolfram.com/TangentNumber.html"
  [n]
  (def T (atom (assoc (vec (repeat (+ n 2) 0)) 1 1)))
  (doseq
   [k (range (inc n))]
   (do
    (doseq
     [i (range (inc k))]
     (swap!
      T
      (fn*
       [p1__21131#]
       (assoc p1__21131# i (* (inc i) (p1__21131# (inc i)))))))
    (swap! T (fn* [p1__21132#] (assoc p1__21132# k 0)))
    (doseq
     [i (range (inc k) 1 -1)]
     (swap!
      T
      (fn*
       [p1__21133#]
       (assoc p1__21133# i (+ (p1__21133# i) (p1__21133# (- i 2)))))))))
  (first (clojure.core/deref T)))
 (is
  (=
   (mapv tangent-number [1 3 5 7 9 11 13])
   [1 2 16 272 7936 353792 22368256]))
 (is (= (mapv tangent-number [0 2 4 6 8 10]) [0 0 0 0 0 0])))

(with-test
 (defn
  farey-sequence
  [n]
  (loop
   [a 1 b 1 c (dec n) d n fs []]
   (let
    [fs (cons (/ a b) fs)]
    (if
     (pos? a)
     (let
      [k (quot (+ n b) d)]
      (recur c d (- (* k c) a) (- (* k d) b) fs))
     fs))))
 (is
  (=
   (farey-sequence 5)
   (quote (0 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1)))))

(with-test
 (defn
  eulerian-number
  "uses standard recurrence: <n,k> = (k+1) <n-1,k> + (n-k) <n-1,k-1>"
  [n k]
  (if
   (zero? k)
   1
   (do
    (def E (atom (vec (repeat (max (inc n) (inc k)) 0))))
    (swap! E (fn* [p1__21134#] (assoc p1__21134# 0 1)))
    (doseq
     [i (inrange n) j (range (dec i) 0 -1)]
     (swap!
      E
      (fn*
       [p1__21135#]
       (assoc
        p1__21135#
        j
        (+
         (* (inc j) ((clojure.core/deref E) j))
         (* (- i j) ((clojure.core/deref E) (dec j))))))))
    ((clojure.core/deref E) k))))
 (is
  (= (mapv (partial eulerian-number 5) [0 1 2 3 4]) [1 26 66 26 1])))

(with-test
 (defn
  binomial
  [n k]
  (if
   (or (zero? k) (every? one? [n k]))
   1
   (loop
    [n n k k]
    (cond
     (zero? k)
     1
     (one? k)
     n
     (> k n)
     0
     (two? k)
     (half (* n (dec n)))
     (> k (half n))
     (recur n (- n k))
     :else
     (*
      (+ n (- k) 1)
      (reduce
       (fn [product i] (* product (/ (+ n (- k) i) i)))
       1
       (inrange 2 k)))))))
 (are
  [a b]
  (= (apply binomial a) b)
  [10 3]
  120
  [10 11]
  0
  [10 0]
  1
  [10 10]
  1
  [10 1]
  10
  [10 9]
  10))

(with-test
 (defn
  triangle-number?
  [n]
  (not (nil? (quadratic-natural-solutions 1/2 1/2 (- n)))))
 (is (= (andmap triangle-number? (quote (0 1 3 6 10 15))))))

(with-test
 (defn
  pentagonal-number?
  [n]
  (not (nil? (quadratic-natural-solutions 3/2 -1/2 (- n)))))
 (is (= (andmap pentagonal-number? (quote (0 1 5 12 22 35))))))

(with-test
 (defn
  hexagonal-number?
  [n]
  (not (nil? (quadratic-natural-solutions 2 -1 (- n)))))
 (is (= (andmap hexagonal-number? (quote (0 1 6 15 28 45))))))

(with-test
 (defn
  heptagonal-number?
  [n]
  (not (nil? (quadratic-natural-solutions 5/2 -3/2 (- n)))))
 (is (= (andmap heptagonal-number? (quote (0 1 7 18 34 55))))))

(with-test
 (defn
  octagonal-number?
  [n]
  (not (nil? (quadratic-natural-solutions 3 -2 (- n)))))
 (is (= (andmap octagonal-number? (quote (0 1 8 21 40 65))))))

(with-test
 (defn
  make-fibonacci
  [c d]
  (fn
   [n]
   (loop
    [a d b c p 0 q 1 count n]
    (cond
     (zero? count)
     b
     (even? count)
     (recur
      a
      b
      (+ (* p p) (* q q))
      (+ (* 2 p q) (* q q))
      (quot count 2))
     :else
     (recur
      (+ (* b q) (* a q) (* a p))
      (+ (* b p) (* a q))
      p
      q
      (dec count))))))
 (is (= (mapv (make-fibonacci 2 1) (range 8)) [2 1 3 4 7 11 18 29])))

(with-test
 (def fibonacci (make-fibonacci 0 1))
 (= (mapv fibonacci (range 8)) [0 1 1 2 3 5 8 13]))

(with-test
 (defn
  make-modular-fibonacci
  [c d]
  (fn
   [n modulus]
   (loop
    [a d b c p 0 q 1 count n]
    (cond
     (zero? count)
     (mod b modulus)
     (even? count)
     (recur
      a
      b
      (mod (+ (* p p) (* q q)) modulus)
      (mod (+ (* 2 p q) (* q q)) modulus)
      (quot count 2))
     :else
     (recur
      (mod (+ (* b q) (* a q) (* a p)) modulus)
      (mod (+ (* b p) (* a q)) modulus)
      p
      q
      (dec count))))))
 (is
  (every?
   identity
   (for
    [a (range -5 6) b (range -5 6) modulus (range 1 8)]
    (=
     (mapv
      (fn [n] ((make-modular-fibonacci a b) n modulus))
      (range 20))
     (mapv (fn [n] (mod ((make-fibonacci a b) n) modulus)) (range 20)))))))

(with-test
 (def modular-fibonacci (make-modular-fibonacci 0 1))
 (is
  (=
   (mapv (fn [n] (modular-fibonacci n 3)) (range 8))
   [0 1 1 2 0 2 2 1])))

(with-test
 (defn
  factorial
  [n]
  (cond
   (or (zero? n) (one? n))
   1
   (< n simple-cutoff)
   (factorial-simple n)
   :else
   ((fn
     loop
     [n a]
     (if
      (<= (- n a) 0)
      n
      (* (loop n (duple a)) (loop (- n a) (duple a)))))
    n
    1N)))
 (defn
  test-factorial
  [n]
  (if (zero? n) 1 (* (bigint n) (test-factorial (dec n)))))
 (are
  [a b]
  (= a b)
  (map factorial [0 1 2 3 4 5])
  [1 1 2 6 24 120]
  (factorial (inc fact-table-size))
  (test-factorial (inc fact-table-size))
  (factorial (inc simple-cutoff))
  (test-factorial (inc simple-cutoff))))

(with-test
 (defn
  permutations
  [n k]
  (cond
   (zero? k)
   1
   (< n k)
   0
   :else
   (/ (factorial n) (factorial (- n k)))))
 10
 3
 (are
  [a b]
  (= (apply permutations a) b)
  [10 3]
  720
  [10 0]
  1
  [10 10]
  3628800
  [0 0]
  1))

(with-test
 (defn
  multinomial
  [n ks]
  (if
   (not= n (apply + ks))
   0
   (apply / (factorial n) (map factorial ks))))
 (are
  [a b]
  (= (apply multinomial a) b)
  [20 [3 4 5 8]]
  3491888400
  [0 []]
  1
  [4 [1 1]]
  0))

(with-test
 (defn
  modular-expt
  [a b n]
  ((fn
    loop
    [a b]
    (cond
     (<= b 1)
     (if (zero? b) (mod 1 n) (mod a n))
     (even? b)
     (mod (sq (loop a (quot b 2))) n)
     :else
     (mod (* a (loop a (dec b))) n)))
   a
   b))
 (is (= (mod (pow -6N 523N) 19) (modular-expt -6N 523N 19))))

(with-test
 (defn-
  modular-const
  [n a]
  (if
   (integer? a)
   (mod a n)
   (mod (* (numerator a) (modular-inverse n (denominator a))) n)))
 (are
  [a b]
  (= (apply modular-const a) b)
  [1 2]
  0
  [3 1]
  1
  [2 1]
  1
  [1 2]
  0
  [3 2/3]
  2
  [3 1/3]
  1))

(def ^:private ^:dynamic current-modulus 1)

(do
  (defmacro
    with-modulus
    "binds the current-modulus atom to n while executing body"
    [n & body]
    `(binding
         [current-modulus ~n]
         (do ~@body)))
  (with-test
    #'adamdavislee.math.number-theory/with-modulus
    (let [prev-modulus current-modulus
          n (rand-int 1024)]
      (is
       (= n
          (with-modulus
            n
            current-modulus)))
      (is
       (= prev-modulus
          current-modulus)))))

(with-test
  (defn
    mod+
    ([] 0)
    ([a] (mod a current-modulus))
    ([a b] (mod (+ a b) current-modulus))
    ([a b & cs]
     (reduce
      (fn [c d] (mod (+ c d) current-modulus))
      (mod (+ a b) current-modulus)
      cs)))
  (with-modulus
    3
    (are [a b] (= (mod+ a) b) 92 2 79 1 109 1 106 1)
    (are
        [a b]
        (= (apply mod+ a) b)
        [117 11 4]
        0
        [109 74 94]
        1
        [49 66 65]
        0
        [62 112 77]
        2)))

(with-test
 (defn
  mod-
  ([a] (mod (- a) current-modulus))
  ([a b] (mod (- a b) current-modulus))
  ([a b & cs]
   (reduce
    (fn [c d] (mod (- c d) current-modulus))
    (mod (- a b) current-modulus)
    cs)))
 (with-modulus
  3
  (are
   [a b]
   (= (apply mod- a) b)
   [46 109 15]
   0
   [62 102 113]
   0
   [58 92 49]
   1
   [118 54 98]
   2)))

(with-test
 (defn
  mod*
  ([] (mod 1 current-modulus))
  ([a] (mod a current-modulus))
  ([a b] (mod (* a b) current-modulus))
  ([a b & cs]
   (reduce
    (fn [c d] (mod (* c d) current-modulus))
    (mod (* a b) current-modulus)
    cs)))
 (with-modulus
  3
  (let
   [ins
    (vec
     (repeatedly
      4
      (fn [] (vec (repeatedly 3 (fn [] (rand-int 128)))))))
    outs
    (mapv (fn [a] (apply mod* a)) ins)]
   [ins outs])
  (are
   [a b]
   (= (apply mod* a) b)
   [122 7 68]
   1
   [75 63 98]
   0
   [77 94 16]
   2
   [49 18 9]
   0)))

(with-test
 (defn
  mod-div
  ([a] (modular-inverse a current-modulus))
  ([a b]
   (mod (* a (modular-inverse b current-modulus)) current-modulus))
  ([a b & cs] (mod-div a (apply mod* b cs))))
 (with-modulus
  3
  (are
   [a b]
   (= (apply mod-div a) b)
   [24971 28175 57626]
   2
   [47928 26377 64096]
   0
   [13786 27692 6754]
   2
   [63868 50846 51536]
   1)))

(with-test
 (defn modsqr [a] (mod (sq a) current-modulus))
 (with-modulus
  3
  (are
   [a b]
   (= (modsqr a) b)
   (modsqr 3)
   0
   (modsqr 4)
   1
   (modsqr 5)
   1
   (modsqr 6)
   0)))

(with-test
 (defn
  modexpt
  [a b]
  (if
   (< b 0)
   (modular-expt
    (modular-inverse a current-modulus)
    (- b)
    current-modulus)
   (modular-expt a b current-modulus)))
 (with-modulus 3 (are [a b] (= (apply modexpt a) b) [2 3] 2 [4 7] 1)))

(with-test
 (defn
  mod2
  [a]
  (if
   (integer? a)
   (mod a current-modulus)
   (mod-div (numerator a) (denominator a))))
 (is (zero? (with-modulus 7 (mod2 (* 218 7)))))
 (are [a b] (= (with-modulus 7 (mod2 a)) b) (* 218 7) 0 3/2 5))

(with-test
 (defn-
  def-mod-op
  [op]
  (fn [& numbers] (apply op (map mod2 numbers))))
 (is (with-modulus 3 ((def-mod-op =) 0 3 6 9))))

(with-test
 (def mod= (def-mod-op =))
 (with-modulus 3 (is (mod= 3 6 9 12)) (is (not (mod= 3 6 9 13)))))

(with-test
 (def mod< (def-mod-op <))
 (with-modulus 3 (is (mod< 3 4 5)) (is (not (mod< 3 4 5 6)))))

(with-test
 (def mod> (def-mod-op >))
 (with-modulus 3 (is (mod> 5 4 3)) (is (not (mod> 6 5 4 3)))))

(with-test
 (def mod<= (def-mod-op <=))
 (with-modulus 3 (is (mod<= 3 3 4 4)) (is (not (mod<= 4 4 3 3)))))

(with-test
 (def mod>= (def-mod-op >=))
 (with-modulus 3 (is (mod>= 4 4 3 3)) (is (not (mod>= 3 3 4 4)))))

(with-test
 (defn
  bernoulli
  "based on this recursive definition: https://en.wikipedia.org/wiki/Bernoulli_number#Recursive_definition"
  [n]
  (-
   (if (zero? n) 1 0)
   (apply
    +
    (mapv
     (fn [k] (* (binomial n k) (/ (bernoulli k) (+ n (- k) 1))))
     (inrange 0 (dec n))))))
 (is
  (=
   (mapv bernoulli (inrange 0 16))
   [1
    -1/2
    1/6
    0
    -1/30
    0
    1/42
    0
    -1/30
    0
    5/66
    0
    -691/2730
    0
    7/6
    0
    -3617/510])))

(with-test
 (defn
  partitions
  [n]
  (if
   (zero? n)
   1
   (+
    (loop
     [k 1 m (dec n) s 0]
     (if
      (neg? m)
      s
      (recur
       (inc k)
       (- m (+ (* 3 k) 1))
       (if (odd? k) (+ s (partitions m)) (- s (partitions m))))))
    (loop
     [k -1 m (- n 2) s 0]
     (if
      (neg? m)
      s
      (recur
       (dec k)
       (+ m (* 3 k) -2)
       (if (odd? k) (+ s (partitions m)) (- s (partitions m)))))))))
 (is
  (=
   (map partitions [0 1 2 3 4 5 6 7 8 9 10])
   [1 1 2 3 5 7 11 15 22 30 42])))

(with-test
 (defn-
  nth-prime
  [n]
  (loop
   [numbers (drop 2 (inrange)) primes [2] prime 2]
   (if
    (= n (count primes))
    (last primes)
    (let
     [numbers
      (filter
       (fn [a] (or (= prime a) (not (divides? prime a))))
       numbers)
      primes
      (conj primes (first numbers))
      prime
      (first numbers)
      numbers
      (rest numbers)]
     (recur numbers primes prime)))))
 (is (= (map nth-prime (inrange 1 8)) [2 3 5 7 11 13 17 19])))

(with-test
 (defn
  prime?
  [n]
  (cond
   (one? n)
   false
   (<= n 3)
   true
   (or (divides? 2 n) (divides? 3 n))
   false
   :else
   (loop
    [i 5]
    (if
     (<= (sq i) n)
     (if
      (or (divides? i n) (divides? (+ 2 i) n))
      false
      (recur (+ i 6)))
     true))))
 (is (prime? 1020379))
 (is (not (prime? 1020378)))
 (is (not (prime? 1))))

(with-test
 (defn
  max-dividing-power
  [p n]
  (defn-
   max-dividing-power-naive
   [p n]
   {:post [(natural? %)]}
   (loop
    [p-to-e 1 e 0]
    (if (divides? p-to-e n) (recur (* p p-to-e) (inc e)) (dec e))))
  (defn-
   find-power
   [p-to-e e]
   (+ e (max-dividing-power-naive p (quot n p-to-e))))
  (defn-
   find-start
   [p-to-e e]
   (let
    [p-to-e2 (sq p-to-e)]
    (cond
     (= p-to-e2 n)
     (duple e)
     (> p-to-e2 n)
     (find-power p-to-e e)
     (divides? p-to-e2 n)
     (if
      (divides? p (quot n p-to-e2))
      (find-start p-to-e2 (duple e))
      (duple e))
     :else
     (find-power p-to-e e))))
  (cond
   (one? p)
   1
   (not (divides? p n))
   0
   :else
   (assert (find-start p 1) natural?)))
 (is
  (= (max-dividing-power 3 27) 3)
  (= (max-dividing-power 3 (duple 27)) 3)))

(with-test
 (defn odd-prime? [n] (and (odd? n) (prime? n)))
 (is (odd-prime? 3))
 (is (not (odd-prime? 2))))

(with-test
 (defn randint [a b] (+ (rand-int (+ b (- a) 1)) a))
 (is
  (=
   (vec
    (sort
     (keys
      (frequencies
       (take (pow 2 8) (repeatedly (fn* [] (randint 2 8))))))))
   (inrange 2 8))))

(with-test
 (defn-
  prime-strong-pseudo-single?
  "this thing maybe possibly works"
  [n]
  {:pre [(natural? n)]}
  (cond
   (<= 4 n)
   (let
    [a (randint 2 (- n 2)) g (gcd a n)]
    (if
     (< 1 g)
     g
     (loop
      [ν 0 m (dec n)]
      (cond
       (even? m)
       (recur (inc ν) (quot m 2))
       :else
       (let
        [b (modular-expt a m n)]
        (cond
         (one? b)
         :probably-prime
         :else
         (loop
          [i 0 b b b-old b]
          (if
           (and (< i ν) (not (one? b)))
           (recur (inc i) (mod (sq b) n) b)
           (if
            (one? b)
            (let
             [g (gcd (inc b-old) n)]
             (if (or (one? g) (= g n)) :probably-prime g))
            :composite)))))))))
   (one? n 1)
   :composite
   :else
   :probably-prime))
 (is (= (prime-strong-pseudo-single? 4) 2))
 (is (#{3 2} (prime-strong-pseudo-single? 6)))
 (is (#{:composite 4 2} (prime-strong-pseudo-single? 8)))
 (is (= (prime-strong-pseudo-single? 5) :probably-prime))
 (is (= (prime-strong-pseudo-single? 7) :probably-prime))
 (is (= (prime-strong-pseudo-single? 11) :probably-prime)))

(quote
 (defn-
  prime-strong-pseudo-explanation
  [n]
  (loop
   [trials 1.0E7 result (prime-strong-pseudo-single? n)]
   (cond
    (zero? trials)
    :very-probably-prime
    (= result (quote probably-prime))
    (recur (dec trials) (prime-strong-pseudo-single? n))
    :else
    result))))

(quote
 (defn
  prime-strong-pseudo?
  [n]
  (let
   [explanation (prime-strong-pseudo-explanation n)]
   (or (= explanation :very-probably-prime) (= explanation true)))))

(quote
 (defn
  small-prime-factors-over
  [n p]
  {:pre [(natural? p)]}
  (cond
   (neg? p)
   (raise-argument-error (quote small-prime-factors-over) "Natural" p)
   (< n p)
   (list)
   (= n p)
   (list (list p 1))
   (prime? n)
   (list (list n 1))
   (divides? p n)
   (let
    [m (max-dividing-power p n)]
    (cons
     (list p m)
     (small-prime-factors-over (quot n (expt p m)) (next-prime p))))
   :else
   (recur n (next-prime p)))))

(quote
 (with-test
  (defn as-power [a])
  (are
   [[a b e]]
   (let [[b2 e2] (as-power a)] (and (= b b2) (= e e2)))
   (apply (as-power a))
   [27 3 3]
   [28 28 1]
   [(* 5 5 7 7 7) (* 5 5 7 7 7) 1]
   [(* 5 5 7 7 7 7) 245 2])))

(quote
 (with-test
  (defn
   perfect-power?
   [a]
   (and
    (not (zero? a))
    (let [(base n) (as-power a)] (and (> n 1) (> a 1)))))
  (are
   [a b]
   (= (perfect-power a) b)
   3
   false
   9
   true
   (pow 12 7)
   true
   (dec (pow 12 7))
   false)))

(quote
 (defn
  powers-of
  [a n]
  (loop
   [i 0 a-to-i 1]
   (if (<= i n) (cons a-to-i (loop (+ i 1) (* a-to-i a))) (quote ())))))

(quote
 (with-test
  (defn as-power [a])
  (are
   [[a b e]]
   (let [[b2 e2] (as-power a)] (and (= b b2) (= e e2)))
   (apply (as-power a))
   [27 3 3]
   [28 28 1]
   [(* 5 5 7 7 7) (* 5 5 7 7 7) 1]
   [(* 5 5 7 7 7 7) 245 2])))

(quote
 (with-test
  (defn
   perfect-power?
   [a]
   (and
    (not (zero? a))
    (let [(base n) (as-power a)] (and (> n 1) (> a 1)))))
  (are
   [a b]
   (= (perfect-power a) b)
   3
   false
   9
   true
   (pow 12 7)
   true
   (dec (pow 12 7))
   false)))

(quote
 (defn
  powers-of
  [a n]
  (loop
   [i 0 a-to-i 1]
   (if (<= i n) (cons a (loop (+ i 1) (* a-to-i a))) (quote ())))))

(successful? (run-tests))
