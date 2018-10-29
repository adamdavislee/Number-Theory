(ns adamdavislee.math.number-theory
  "This translation closely mimics the names and general style of the racket source code.
  However:
    It doesn't define inline versions of mod+, mod*, etcetera.
    `current-modulus` is a dynamic var (similar conceptually to the racket's 'parameters')
    `factorize` returns a map
    It renames:
     `mod/` to `mod-div`
     `mod` to `mod2`
     `integer-root/remainder` to `integer-root-remainder`
     `perfect-square` to `perfect-square?`
     `prime-divisors` to `factorization-divisors`
     `prime-exponents` to `factorization-exponents` (not all exponents of the return value are prime)
  TODO:
    Use internal memoization for some of the functions which racket uses manual bit manipulation for.
    Refactor the prime functions as a lazy-seq
    Refactor quadratic solutions to return a lazy-seq
    Refactor the fibonacci functions as a lazy-seq"
  (:require [clojure.test
             :refer [is
                     are
                     with-test
                     successful?
                     run-tests], ]
            [clojure.spec.alpha :as s]))
(s/check-asserts true)
(with-test
  (defn ^:private pos-integer?
    "like `pos-int` but works for bigint[eger]s"
    [n]
    {:pre [(number? n)]}
    (and (integer? n) (pos? n)))
  (are
      [a]
      a
      (not (pos-int? 1N))
      (pos-integer? 1N)))
(with-test
  (defn ^:private nat-integer?
    "like `nat-int` but works for bigint[eger]s"
    [n]
    {:pre [(number? n)]}
    (and (integer? n)
         (or (zero? n) (pos? n))))
  (are
      [a]
      a
      (not (pos-int? 1N))
      (pos-integer? 1N)))
(with-test
  (defn-
    intvalue?
    "tests if a number is an integer (could be a decimal)"
    [n]
    {:pre [(number? n)]}
    (== n (biginteger n)))
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
  (defn-
    natvalue?
    "tests if a number is a natural (could be a decimal)"
    [n]
    {:pre [(number? n)]}
    (and (intvalue? n) (< -1 n)))
  (are
      [a] (natvalue? a),
      0
      1
      2
      3
      4)
  (are
      [a] (not (natvalue? a)),
      -1
      -2
      -0.5))
(with-test
  (defn-
    abs
    "absolute value"
    [a]
    {:pre (number? a)}
    (if (neg? a) (- a) a))
  (is (= (abs -1) 1))
  (is (= (abs 1) 1)))
(with-test
  (defn-
    pow
    "exponent operator with identity value and multiple arity similar to * or +"
    ([] 1)
    ([n] {:pre [(number? n)]} n)
    ([a b]
     {:pre [(every? number? [a b])]}
     (if (intvalue? b)
       ((if (neg? b) / *)
        (apply * (repeat (abs b) a)))
       (Math/pow a b)))
    ([a b & numbers]
     {:pre [(every?
             number?
             (conj numbers a b))]}
     (let [numbers (conj numbers b a)]
       (reduce pow numbers))))
  (are
      [a b] (= (double (apply pow a))
               (double b)),
      [] 1,
      [2] 2,
      [2 2] 4,
      [2 2 2] 16,
      [2 3 4] (pow (pow 2 3) 4),
      [3 -1] 1/3,
      [4 -2] 1/16, ))
(with-test
  (defn-
    rt
    "root operator"
    [a b]
    {:pre [(every? number? [a b])]}
    (pow a (/ b)))
  (is (= (rt 8 3) 2.0)))
(with-test
  (def ^:private
    one?
    "imo it isn't unreasonable to introduce vocabulary for 0 1 and 2"
    (partial = 1))
  (are
      [a b] (= (apply one? a) b),
      [1] true,
      [2] false, ))
(with-test
  (def ^:private
    two?
    "imo it isn't unreasonable to introduce vocabulary for 0 1 and 2"
    (partial = 2))
  (is (not (two? 1)))
  (is (two? 2)))
(with-test
  (def ^:private
    duple
    "closest word i could find analogous to increment or square for multiplication"
    (partial * 2))
  (is (= 6 (duple 3))))
(with-test
  (defn-
    half
    "just divides by two"
    [n]
    {:pre [(number? n)]}
    (/ n 2))
  (is (= (half 4) 2)))
(with-test
  (defn-
    sqrt
    "just an alias"
    [n]
    {:pre [(number? n)]}
    (Math/sqrt n))
  (is (= (sqrt 1) (double 1))))
(with-test
  (defn-
    sq
    "shorthand for squaring a number"
    [n]
    {:pre [(number? n)]}
    (pow n 2))
  (is (= (sq 3) 9)))
(with-test
  (defn-
    inrange
    "range with an inclusive end and default beginning of 1"
    ([] (drop 1 (range)))
    ([end]
     {:pre [(number? end)]}
     (range 1 (inc end)))
    ([start end]
     {:pre [(every? number? [start end])]}
     (range start (inc end)))
    ([start end step]
     {:pre [(every?
             number?
             [start end step])]}
     (range start (inc end) step)))
  (are
      [a b] (= (apply inrange a) b),
      [8] [1 2 3 4 5 6 7 8],
      [2 4] [2 3 4], ))
(with-test
  (defn-
    truthy?
    "converts any value to a boolean"
    [a]
    (or (and a true) false))
  (true? (truthy? 8))
  (false? (truthy? nil)))
(with-test
  (defn-
    detruthy
    "converts any truthy function to a boolean function"
    [f]
    {:pre [(fn? f)]}
    (fn [& args]
      (truthy? (apply f args))))
  (false? ((detruthy *) nil))
  (true? ((detruthy *) 2)))
(def ^:private
  fact-table-size
  171)
(def ^:private
  fact-table
  (vec
   (reverse
    (reduce
     (fn [numbers n]
       (cons
        (* n (first numbers))
        numbers))
     [1N]
     (inrange fact-table-size)))))
(def ^:private
  simple-cutoff
  244)
(with-test
  (defn-
    gcd
    "uses the euclidean algorithm, see https://brilliant.org/wiki/euclidean-algorithm"
    ([] 0)
    ([a] {:pre [(integer? a)]} a)
    ([a b]
     {:pre [(every? integer? [a b])]}
     (if (zero? b)
       (abs a)
       (recur b (rem a b))))
    ([a b & numbers]
     {:pre [(every?
             integer?
             (conj numbers a b))]}
     (gcd
      a
      (apply gcd (conj numbers b)))))
  (are
      [a b] (= (apply gcd a) b),
      [] 0,
      [3] 3,
      [2 4] 2,
      [3 4] 1,
      [-2 4] 2,
      [3 4 5] 1, ))
(with-test
  (defn divides?
    "just shorthand for testing divisibility"
    [a b]
    {:pre [(every? number? [a b])]}
    (if (zero? a)
      false
      (== 0 (rem b a))))
  (are
      [a b] (divides? a b),
      2 4,
      2 12,
      2 0, )
  (are
      [a b] (not (divides? a b)),
      0 1,
      2 3,
      2 13, ))
(with-test
  (defn coprime?
    "generalization of primality, tests that a set of numbers has no common divisor"
    [a & numbers]
    {:pre [(every?
            integer?
            (conj numbers a))]}
    (one?
     (apply gcd (conj numbers a))))
  (are
      [a b] (= (apply coprime? a) b),
      [1] true,
      [2] false,
      [2 9] true,
      [2 4 8] false,
      [2 4 9] true, )
  (is
   (coprime? (* 3 7) (* 5 19)))
  (is
   (not (coprime? (* 3 7 5) (* 5 19 2)))))
(with-test
  (defn pairwise-coprime?
    "similar to `coprime?`, tests that a set of numbers has no two numbers with a common divisor"
    [a & numbers]
    {:pre [(every?
            integer?
            (conj numbers a))]}
    (or (nil? numbers)
        (and (if (every?
                  identity
                  (map
                   (fn [b] (coprime? a b))
                   numbers))
               true
               false)
             (if (apply
                  pairwise-coprime?
                  numbers)
               true
               false))))
  (are
      [a b] (= (apply pairwise-coprime? a)
               b),
      [1] true,
      [1 2] true,
      [1 2 4] false,
      [1 2 -4] false, )
  (are
      [a] (not (apply pairwise-coprime? a)),
      [10 7 33 14]
      [6 10 15])
  (is
   (pairwise-coprime? 10 7 33 13)))
(with-test
  (defn bezout
    "function from a list of numbers to a list of numbers that when paired with the first, multiplied, and summed is the gcd of the first set: https://brilliant.org/wiki/bezouts-identity
  Uses the extended euclidean algorithm: https://brilliant.org/wiki/extended-euclidean-algorithm"
    ([a] {:pre [(integer? a)]} 1)
    ([a b]
     {:pre [(every? integer? [a b])]}
     (let [loop (fn [a b ua va ub vb]
                  (let [q (quot a b)
                        , r
                        (rem a b)]
                    (if (= r 0)
                      (list ub vb)
                      (recur b
                             r
                             ub
                             vb
                             (- ua (* q ub))
                             (- va (* q vb))))))
           , start
           (fn [a b]
             (if (> a b)
               (loop a b 1 0 0 1)
               (loop b a 0 1 1 0)))]
       (cond
         (and (pos? a) (pos? b)) (start a b),
         (and (neg? a) (neg? b)) (mapv - (start (- a) (- b))),
         (and (neg? a) (pos? b))
         (let [k (+ (quot (- a) b) 1)
               , [u v]
               (start (+ a (* k b)) b)]
           [u (+ (* u k) v)])
         (and (pos? a) (neg? b))
         (let [k (+ (quot (- b) a) 1)
               [u v] (start a (+ (* k a) b))]
           (list (+ u (* k v)) v)))))
    ([a b & numbers]
     {:pre [(every?
             integer?
             (conj numbers a b))]}
     (let [numbers (conj numbers b a)
           [s t] (bezout
                  (apply gcd (rest numbers))
                  (first numbers))]
       (conj
        (map
         (partial * s)
         (apply bezout (rest numbers)))
        t))))
  (are
      [a b] (and (= (apply bezout a) b),
                 (= (apply gcd a)
                    (apply + (map * a b)))),
      [4 2] [0 1],
      [2 4] [1 0],
      [2 -4] [1 0],
      [-2 4] [1 1],
      [-2 -4] [-1 0],
      [3 4] [-1 1],
      [2 4 8] [1 0 0],
      [2 4 7] [0 2 -1], )
  (are
      [a] (= (apply gcd a)
             (apply
              + (map * (apply bezout a) a))),
      [12 20]
      [12 20]
      [20 16]
      [12 20 16]))
(with-test
  (defn modular-inverse
    "returns the number less than n that when multiplied with a is 1 (mod n)"
    [a n]
    {:pre [(every? integer? [a n])
           (coprime? a n)]}
    (mod (first (bezout a n)) n))
  (are
      [a b] (= (apply modular-inverse a) b),
      [3 7] 5, )
  (is
   (every?
    (fn [n]
      (let [m (and (coprime? n 20)
                   (modular-inverse n 20))]
        (if m
          (= (rem (* n m) 20) 1)
          true)))
    (inrange 20))))
(with-test
  (defn solve-chinese
    "returns the least natural equal to every nth term of `as` mod the corresponding term of `ns`"
    [as ns]
    {:pre [(every?
            integer?
            (concat as ns))
           (every? pos? ns)]}
    (let [n (apply * ns)
          cs (map (fn [ni] (quot n ni)) ns)
          ds (map modular-inverse cs ns)]
      (mod
       (apply + (map * as cs ds))
       n)))
  (are
      [a b] (= (apply solve-chinese a) b),
      [[2 3 2] [3 5 7]] 23, ))
(with-test
  (defn mediant
    "the mediant of (/ a b) and (/ c d) is (/ (+ a c) (+ b d))"
    [a b]
    {:pre [(every? number? [a b])]}
    (/
     (+ (numerator a) (numerator b))
     (+ (denominator a)
        (denominator b))))
  (are
      [a b]
      (= (apply mediant a) b)
      [1/2 5/6] 3/4,
      [-1/2 -2/3]
      -3/5))
(with-test
  (defn quadratic-solutions
    "returns list of solutions to ax^2+bx+c=0"
    [a b c]
    {:pre [(every? number? [a b c])]}
    (let [d (- (* b b) (* 4 a c))]
      (cond
        (< d 0) [],
        (= d 0) [(/ b (* -2 a))],
        :else (let [sqrt-d (sqrt d)]
                [(/ (- (- b) sqrt-d) (* 2 a))
                 (/ (+ (- b) sqrt-d) (* 2 a))]))))
  (are
      [a] a,
      (= (quadratic-solutions 1 0 -4)
         [-2.0 2.0])
      (= (quadratic-solutions 1 0 4)
         [])
      (= (quadratic-solutions 1 0 0)
         [0])))
(with-test
  (defn quadratic-integer-solutions
    "returns list of integer solutions to ax^2+bx+c=0"
    [a b c]
    {:pre [(every? number? [a b c])]}
    (mapv
     int
     (filter
      intvalue?
      (quadratic-solutions a b c))))
  (are
      [a b] (= (apply
                quadratic-integer-solutions
                a)
               b),
      [1 0 -4] [-2 2],
      [1 0 4] [],
      [1 0 0] [0], ))
(with-test
  (defn quadratic-natural-solutions
    "returns list of nonnegative-integer solutions to a x^2 + b x + c = 0"
    [a b c]
    (filter
     natvalue?
     (quadratic-solutions a b c)))
  (are
      [a b] (= (apply
                quadratic-natural-solutions
                a)
               b),
      [1 0 -4] [2.0],
      [1 0 4] [],
      [1 0 0] [0], ))
(with-test
  (defn perfect-square?
    "predicate returning the (sqrt a) if a is the square of an integer"
    [a]
    {:pre [(nat-int? a)]}
    (let [a-sqrt (int (sqrt a))]
      (if (= (pow a-sqrt 2) a)
        a-sqrt
        false)))
  (are
      [a b] (= (apply perfect-square? a) b),
      [4] 2,
      [5] false, ))
(with-test
  (defn triangle-number?
    "tests if input is a triangle number (about polygonal numbers: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Figurate/figurate.html)"
    [n]
    [:pre (nat-int? n)]
    (not (nil?
          (quadratic-natural-solutions
           1/2
           1/2
           (- n)))))
  (is
   (= (every?
       triangle-number?
       '(0 1 3 6 10 15)))))
(with-test
  (defn square-number?
    "tests if input is a square number (about polygonal numbers: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Figurate/figurate.html)"
    [n]
    {:pre [(nat-int? n)]}
    (and (perfect-square? n) true))
  (is
   (every?
    square-number?
    [0 1 4 9 16 25]))
  (is
   (every?
    (complement square-number?)
    [2 5 10 17 26])))
(with-test
  (defn pentagonal-number?
    "tests if input is a pentagonal number (about polygonal numbers: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Figurate/figurate.html)"
    [n]
    {:pre (nat-int? n)}
    (not (nil?
          (quadratic-natural-solutions
           3/2
           -1/2
           (- n)))))
  (is
   (= (every?
       pentagonal-number?
       '(0 1 5 12 22 35)))))
(with-test
  (defn hexagonal-number?
    "tests if input is a hexagonal number (about polygonal numbers: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Figurate/figurate.html)"
    [n]
    {:pre (nat-int? n)}
    (not (nil?
          (quadratic-natural-solutions
           2
           -1
           (- n)))))
  (is
   (= (every?
       hexagonal-number?
       '(0 1 6 15 28 45)))))
(with-test
  (defn heptagonal-number?
    "tests if input is a heptagonal number (about polygonal numbers: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Figurate/figurate.html)"
    [n]
    {:pre (nat-int? n)}
    (not (nil?
          (quadratic-natural-solutions
           5/2
           -3/2
           (- n)))))
  (is
   (= (every?
       heptagonal-number?
       '(0 1 7 18 34 55)))))
(with-test
  (defn octagonal-number?
    "tests if input is a octagonal number (about polygonal numbers: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Figurate/figurate.html)"
    [n]
    {:pre (nat-int? n)}
    (not (nil?
          (quadratic-natural-solutions
           3
           -2
           (- n)))))
  (is
   (= (every?
       octagonal-number?
       '(0 1 8 21 40 65)))))
(with-test
  (defn triangle-number
    "returns the nth triangle number (about polygonal numbers: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Figurate/figurate.html)"
    [n]
    {:pre (nat-int? n)}
    (quot (* n (inc n)) 2))
  (are
      [a b] (= (apply triangle-number a) b),
      [1] 1,
      [2] 3, )
  (is
   (= (mapv
       triangle-number
       (inrange 0 5))
      [0 1 3 6 10 15])))
(with-test
  (def sqr
    "just an alias to parody the racket source code; also equivalent to the nth square-number (about polygonal numbers: bla bla bla (you can find the url :-D ))"
    sq)
  (is (= (sqr 3) 9)))
(with-test
  (defn pentagonal-number
    "returns the nth pentagonal number (about polygonal numbers: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Figurate/figurate.html)"
    [n]
    {:pre [(nat-int? n)]}
    (quot (* n (- (* 3 n) 1)) 2))
  (is
   (= (mapv
       pentagonal-number
       [0 1 2 3 4 5])
      [0 1 5 12 22 35])))
(with-test
  (defn hexagonal-number
    "returns the nth hexagonal number (about polygonal numbers: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Figurate/figurate.html)"
    [n]
    {:pre [(nat-int? n)]}
    (* n (- (* 2 n) 1)))
  (is
   (= (mapv
       hexagonal-number
       [0 1 2 3 4 5])
      [0 1 6 15 28 45])))
(with-test
  (defn heptagonal-number
    "returns the nth heptagonal number (about polygonal numbers: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Figurate/figurate.html)"
    [n]
    {:pre [(nat-int? n)]}
    (quot (* n (- (* 5 n) 3)) 2))
  (is
   (= (mapv
       heptagonal-number
       [0 1 2 3 4 5])
      [0 1 7 18 34 55])))
(with-test
  (defn octagonal-number
    "returns the nth octagonal number (about polygonal numbers: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Figurate/figurate.html)"
    [n]
    {:pre [(nat-int? n)]}
    (* n (- (* 3 n) 2)))
  (is
   (= (mapv
       octagonal-number
       [0 1 2 3 4 5])
      [0 1 8 21 40 65])))
(with-test
  (defn tangent-number
    "returns the nth tangent number (about tangent numbers: http://mathworld.wolfram.com/TangentNumber.html)"
    [n]
    (def ^:private
      T
      (atom
       (assoc (vec (repeat (+ n 2) 0))
              1
              1)))
    (doseq
        [k (range (inc n))]
        (do
          (doseq
              [i (range (inc k))]
              (swap!
               T
               #(assoc
                 %
                 i
                 (* (inc i) (% (inc i))))))
          (swap! T #(assoc % k 0))
          (doseq
              [i (range (inc k) 1 -1)]
              (swap!
               T
               #(assoc
                 %
                 i
                 (+ (% i) (% (- i 2))))))))
    (first @T))
  (is
   (= (mapv
       tangent-number
       [1 3 5 7 9 11 13])
      [1
       2
       16
       272
       7936
       353792
       22368256]))
  (is
   (= (mapv
       tangent-number
       [0 2 4 6 8 10])
      [0 0 0 0 0 0])))
(with-test
  (defn farey-sequence
    "returns the nth farey-sequence; a farey sequence is just the list of all rationals with integer denominators less than or equal to n"
    [n]
    {:pre (nat-int? n)}
    (loop [a 1,
           b 1,
           c (dec n),
           d n,
           fs [], ]
      (let [fs (cons (/ a b) fs)]
        (if (pos? a)
          (let [k (quot (+ n b) d)]
            (recur c
                   d
                   (- (* k c) a)
                   (- (* k d) b)
                   fs))
          fs))))
  (is
   (= (farey-sequence 5)
      '(0
        1/5
        1/4
        1/3
        2/5
        1/2
        3/5
        2/3
        3/4
        4/5
        1))))
(with-test
  (defn eulerian-number
    "the eulerian number is the number of the permutations of n which have exactly k pairs of numbers in ascending order (referred to as 'ascensions'); for example if n=3 k=2 there is only one permutation of 3 (i.e. [1 2 3]) in which there are two ascensions (i.e. [1 2] and [2 3]); algorithm uses the standard recurrence: <n,k> = (k+1) <n-1,k> + (n-k) <n-1,k-1>"
    [n k]
    {:pre [(every? nat-int? [n k])]}
    (if (zero? k)
      1
      (do
        (def ^:private
          E
          (atom
           (vec
            (repeat
             (max (inc n) (inc k))
             0))))
        (swap! E #(assoc % 0 1))
        (doseq
            [i (inrange n),
             j
             (range (dec i) 0 -1)]
            (swap!
             E
             #(assoc
               %
               j
               (+ (* (inc j) (@E j))
                  (* (- i j) (@E (dec j)))))))
        (@E k))))
  (is
   (= (mapv
       (partial eulerian-number 5)
       [0 1 2 3 4])
      [1 26 66 26 1])))
(with-test
  (defn make-fibonacci
    "returns the fibonacci sequence beginning with c and d"
    [c d]
    {:pre (every? nat-int? [c d])}
    (fn [n]
      (loop [a d, b c, p 0, q 1, count n, ]
        (cond
          (zero? count) b,
          (even? count) (recur a
                               b
                               (+ (* p p) (* q q))
                               (+ (* 2 p q) (* q q))
                               (quot count 2)),
          :else (recur (+ (* b q) (* a q) (* a p))
                       (+ (* b p) (* a q))
                       p
                       q
                       (dec count)), ))))
  (is
   (= (mapv
       (make-fibonacci 2 1)
       (range 8))
      [2 1 3 4 7 11 18 29])))
(with-test
  (def fibonacci
    "the canonical fibonacci sequence beginning with 0 and 1"
    (make-fibonacci 0 1))
  (= (mapv fibonacci (range 8))
     [0 1 1 2 3 5 8 13]))
(with-test
  (defn make-modular-fibonacci
    "returns a modular fibonacci sequence beginning with c and d, the sequence returned takes two arguments: the nth term, and a modulus"
    [c d]
    {:pre (every? nat-int? [c d])}
    (fn [n modulus]
      (loop [a d, b c, p 0, q 1, count n, ]
        (cond
          (zero? count) (mod b modulus),
          (even? count) (recur a
                               b
                               (mod
                                (+ (* p p) (* q q))
                                modulus)
                               (mod
                                (+ (* 2 p q) (* q q))
                                modulus)
                               (quot count 2)),
          :else (recur (mod
                        (+ (* b q) (* a q) (* a p))
                        modulus)
                       (mod
                        (+ (* b p) (* a q))
                        modulus)
                       p
                       q
                       (dec count)), ))))
  (is
   (every?
    identity
    (for [a (range -5 6),
          b (range -5 6),
          modulus (range 1 8), ]
      (= (mapv
          (fn [n]
            ((make-modular-fibonacci a b)
             n
             modulus))
          (range 20))
         (mapv
          (fn [n]
            (mod
             ((make-fibonacci a b) n)
             modulus))
          (range 20)))))))
(with-test
  (def modular-fibonacci
    "the modular counterpart to the canonical fibonacci sequence beginning with 0 and 1"
    (make-modular-fibonacci 0 1))
  (is
   (= (mapv
       (fn [n]
         (modular-fibonacci n 3))
       (range 8))
      [0 1 1 2 0 2 2 1])))
(with-test
  (defn-
    factorial-simple
    "the canonical recursive algorithm"
    [n]
    {:pre (integer? n)}
    (if (< n fact-table-size)
      (fact-table n)
      (* n
         (factorial-simple (dec n)))))
  (is
   (every?
    (fn [n]
      (= (factorial-simple n)
         (apply * (inrange n))))
    (inrange 8))))
(with-test
  (defn factorial
    "returns the factorial of n (i.e. the product of the numbers 1 to n)"
    [n]
    {:pre [(nat-int? n)]}
    (cond
      (or (zero? n) (one? n)) 1,
      (< n simple-cutoff) (factorial-simple n),
      :else ((fn loop [n a]
               (if (<= (- n a) 0)
                 n
                 #_"trampoline?"
                 (* (loop n (duple a))
                    (loop (- n a) (duple a)))))
             n
             1N)))
  (defn-
    test-factorial
    [n]
    (if (zero? n)
      1
      (* (biginteger n)
         (test-factorial (dec n)))))
  (are
      [a b] (= a b),
      (map factorial [0 1 2 3 4 5])
      [1 1 2 6 24 120]
      (factorial
       (inc fact-table-size))
      (test-factorial
       (inc fact-table-size))
      (factorial (inc simple-cutoff))
      (test-factorial
       (inc simple-cutoff))))
(with-test
  (defn permutations
    "returns the 'number of ways to choose k things from n things (order matters)"
    [n k]
    {:pre [(every? nat-int? [n k])]}
    (cond
      (zero? k) 1,
      (< n k)
      0
      :else (/
             (factorial n)
             (factorial (- n k)))))
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
  (defn binomial
    "returns the number of ways to choose k things from n things (order doesn't matter); the 'n choose k' that we learned in high school"
    [n k]
    {:pre [(every? nat-int? [n k])]}
    (if (or (zero? k)
            (every? one? [n k]))
      1
      (loop [n n, k k]
        (cond
          (zero? k) 1,
          (one? k) n,
          (> k n) 0,
          (two? k) (half (* n (dec n))),
          (> k (half n)) (recur n (- n k)),
          :else (* (+ n (- k) 1)
                   (reduce
                    (fn [product i]
                      (* product (/ (+ n (- k) i) i)))
                    1
                    (inrange 2 k))), ))))
  (are
      [a b] (= (apply binomial a) b),
      [10 3] 120,
      [10 11] 0,
      [10 0] 1,
      [10 10] 1,
      [10 1] 10,
      [10 9] 10, ))
(with-test
  (defn multinomial
    "a generalization of binomial, returns the number of ways to choose every kth number of things from n items (https://docs.racket-lang.org/math/number-theory.html#%28def._%28%28lib._math%2Fnumber-theory..rkt%29._multinomial%29%29)"
    [n ks]
    {:pre [every? nat-int? (conj ks n)]}
    (if (not= n (apply + ks))
      0
      (apply
       /
       (factorial n)
       (map factorial ks))))
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
  (defn modular-expt
    "an efficient algorithm for computing (mod (pow a b) n)"
    [a b n]
    {:pre [(or (integer? a)
               (nat-integer? b))
           (pos-integer? n)]}
    ((fn loop [a b]
       (cond
         (<= b 1) (if (zero? b)
                    (mod 1 n)
                    (mod a n)),
         (even? b)
         (mod
          (sq (loop a (quot b 2)))
          n)
         :else (mod (* a (loop a (dec b))) n)))
     a
     b))
  (is
   (= (mod (pow -6N 523N) 19)
      (modular-expt -6N 523N 19))))
(with-test
  (defn-
    modular-const
    "an internal racket function (https://github.com/racket/math/blob/master/math-lib/math/private/number-theory/modular-arithmetic-base.rkt#L59)"
    [n a]
    {:pre [(pos-int? n) (number? a)]}
    (if (integer? a)
      (mod a n)
      (mod
       (* (numerator a)
          (modular-inverse
           n
           (denominator a)))
       n)))
  (are
      [a b] (= (apply modular-const a) b),
      [1 2] 0,
      [3 1] 1,
      [2 1] 1,
      [1 2] 0, ))
(def ^:dynamic
  current-modulus
  1)
(do
  (defmacro
    with-modulus
    "binds the `current-modulus` var to n while executing body"
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
  (defn mod+
    "equivalent to `(mod (+ a b ...) current-modulus)` but generates smaller intermediate values"
    ([] 0)
    ([a]
     {:pre [(nat-int? a)]}
     (mod a current-modulus))
    ([a b]
     {:pre [(every? nat-int? [a b])]}
     (mod (+ a b) current-modulus))
    ([a b & cs]
     {:pre [(every? nat-int? (conj cs a b))]}
     (reduce
      (fn [c d]
        (mod (+ c d) current-modulus))
      (mod (+ a b) current-modulus)
      cs)))
  (with-modulus
    3
    (are
        [a b] (= (mod+ a) b),
        92 2,
        79 1,
        109 1,
        106 1, )
    (are
        [a b] (= (apply mod+ a) b),
        [117 11 4] 0,
        [109 74 94] 1,
        [49 66 65] 0,
        [62 112 77] 2, )))
(with-test
  (defn mod-
    "equivalent to `(mod (- a b ...) current-modulus)` but generates smaller intermediate values"
    ([a]
     {:pre [(nat-int? a)]}
     (mod (- a) current-modulus))
    ([a b]
     {:pre [(every? nat-int? [a b])]}
     (mod (- a b) current-modulus))
    ([a b & cs]
     {:pre [(every? nat-int? (conj cs a b))]}
     (reduce
      (fn [c d]
        (mod (- c d) current-modulus))
      (mod (- a b) current-modulus)
      cs)))
  (with-modulus
    3
    (are
        [a b] (= (apply mod- a) b),
        [46 109 15] 0,
        [62 102 113] 0,
        [58 92 49] 1,
        [118 54 98] 2, )))
(with-test
  (defn mod*
    "equivalent to `(mod (* a b ...) current-modulus)` but generates smaller intermediate values"
    ([] (mod 1 current-modulus))
    ([a]
     {:pre [(nat-int? a)]}
     (mod a current-modulus))
    ([a b]
     {:pre [(every? nat-int? [a b])]}
     (mod (* a b) current-modulus))
    ([a b & cs]
     {:pre [(every? nat-int? (conj cs a b))]}
     (reduce
      (fn [c d]
        (mod (* c d) current-modulus))
      (mod (* a b) current-modulus)
      cs)))
  (with-modulus
    3
    (let [ins (vec
               (repeatedly
                4
                (fn []
                  (vec
                   (repeatedly
                    3
                    (fn [] (rand-int 128)))))))
          outs (mapv
                (fn [a] (apply mod* a))
                ins)]
      [ins outs])
    (are
        [a b] (= (apply mod* a) b),
        [122 7 68] 1,
        [75 63 98] 0,
        [77 94 16] 2,
        [49 18 9] 0, )))
(with-test
  (defn mod-div
    "unlike mod+, mod-, etc., given one argument, returns the modular-inverse; otherwise is the same as `/` but calculates by multiplying the first arg by the multiplicative inverse of (apply * rest-of-args) (don't ask me why :-D)"
    ([a]
     {:pre [(nat-integer? a)]}
     (modular-inverse
      a
      current-modulus))
    ([a b]
     {:pre [(every? nat-integer? [a b])]}
     (mod
      (* a
         (modular-inverse
          b
          current-modulus))
      current-modulus))
    ([a b & cs]
     {:pre [(every?
             nat-integer?
             (conj cs a b))]}
     (mod-div a (apply mod* b cs))))
  (with-modulus
    3
    (are
        [a b] (= (apply mod-div a) b),
        [24971 28175 57626] 2,
        [47928 26377 64096] 0,
        [13786 27692 6754] 2,
        [63868 50846 51536] 1, )))
(with-test
  (defn modsqr
    "equivalent to `(mod (sq a))`"
    [a]
    {:pre [(integer? a)]}
    (mod (sq a) current-modulus))
  (with-modulus
    3
    (are
        [a b] (= (modsqr a) b),
        (modsqr 3) 0,
        (modsqr 4) 1,
        (modsqr 5) 1,
        (modsqr 6) 0, )))
(with-test
  (defn modexpt
    "equivalent to `(mod-expt a b current-modulus)`"
    [a b]
    {:pre [(every? integer? [a b])]}
    (if (< b 0)
      (modular-expt
       (modular-inverse
        a
        current-modulus)
       (- b)
       current-modulus)
      (modular-expt
       a
       b
       current-modulus)))
  (with-modulus
    3
    (are
        [a b] (= (apply modexpt a) b),
        [2 3] 2,
        [4 7] 1, )))
(with-test
  (defn mod2
    "racket calls this mod which clojure already defines, it's a different function though"
    [a]
    {:pre [(some
            identity
            ((juxt integer? rational?) a))]}
    (if (integer? a)
      (mod a current-modulus)
      (mod-div
       (numerator a)
       (denominator a))))
  (is
   (zero?
    (with-modulus
      7
      (mod2 (* 218 7)))))
  (are
      [a b] (= (with-modulus 7 (mod2 a)) b),
      (* 218 7) 0,
      3/2 5, ))
(with-test
  (defn-
    def-mod-op
    "helper function for defining the modular comparison functions"
    [op]
    (fn [& numbers]
      (apply op (map mod2 numbers))))
  (is
   (with-modulus
     3
     ((def-mod-op =) 0 3 6 9))))
(with-test
  (def mod=
    "with one argument returns `true`, otherwise equivalent to `(= (mod2 n1) (mod2 n2) ...)`"
    (def-mod-op =))
  (with-modulus
    3
    (is (mod= 3 6 9 12))
    (is (not (mod= 3 6 9 13)))))
(with-test
  (def mod<
    "with one argument returns `true`, otherwise equivalent to `(< (mod2 n1) (mod2 n2) ...)`"
    (def-mod-op <))
  (with-modulus
    3
    (is (mod< 3 4 5))
    (is (not (mod< 3 4 5 6)))))
(with-test
  (def mod>
    "with one argument returns `true`, otherwise equivalent to `(> (mod2 n1) (mod2 n2) ...)`"
    (def-mod-op >))
  (with-modulus
    3
    (is (mod> 5 4 3))
    (is (not (mod> 6 5 4 3)))))
(with-test
  (def mod<=
    "with one argument returns `true`, otherwise equivalent to `(<= (mod2 n1) (mod2 n2) ...)`"
    (def-mod-op <=))
  (with-modulus
    3
    (is (mod<= 3 3 4 4))
    (is (not (mod<= 4 4 3 3)))))
(with-test
  (def mod>=
    "with one argument returns `true`, otherwise equivalent to `(>= (mod2 n1) (mod2 n2) ...)`"
    (def-mod-op >=))
  (with-modulus
    3
    (is (mod>= 4 4 3 3))
    (is (not (mod>= 3 3 4 4)))))
(with-test
  (defn bernoulli-number
    "returns the nth bernoulli number; the algorithm is based on this recursive definition: https://en.wikipedia.org/wiki/Bernoulli_number#Recursive_definition"
    [n]
    {:pre [(integer? n)]}
    (- (if (zero? n) 1 0)
       (apply
        + (mapv
           (fn [k]
             (* (binomial n k)
                (/
                 (bernoulli-number k)
                 (+ n (- k) 1))))
           (inrange 0 (dec n))))))
  (is
   (= (mapv
       bernoulli-number
       (inrange 0 16))
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
  (defn partitions
    "returns the partitions of `n`; an integer's partitions are the ways a number can by represented by a sum of integers"
    [n]
    {:pre [(integer? n)]}
    (if (zero? n)
      1
      (+ (loop [k 1, m (dec n), s 0, ]
           (if (neg? m)
             s
             (recur (inc k)
                    (- m (+ (* 3 k) 1))
                    (if (odd? k)
                      (+ s (partitions m))
                      (- s (partitions m))))))
         (loop [k -1, m (- n 2), s 0, ]
           (if (neg? m)
             s
             (recur (dec k)
                    (+ m (* 3 k) -2)
                    (if (odd? k)
                      (+ s (partitions m))
                      (- s (partitions m)))))))))
  (is
   (= (map
       partitions
       [0 1 2 3 4 5 6 7 8 9 10])
      [1 1 2 3 5 7 11 15 22 30 42])))
(with-test
  (defn-
    nth-prime
    "returns the nth positive prime number"
    [n]
    {:pre [nat-int? n]}
    (loop [numbers (drop 2 (inrange)),
           primes [2],
           prime 2, ]
      (if (= n (count primes))
        (last primes)
        (let [numbers (filter
                       (fn [a]
                         (or (= prime a)
                             (not (divides? prime a))))
                       numbers)
              primes (conj primes (first numbers))
              prime (first numbers)
              numbers (rest numbers)]
          (recur numbers primes prime)))))
  (is
   (= (map nth-prime (inrange 1 8))
      [2 3 5 7 11 13 17 19])))
(with-test
  (defn max-dividing-power
    "returns the largest exponent of base `p` where `(pow p exponent)` divides `n`"
    [p n]
    {:pre [(every? integer? [p n])]}
    (defn-
      max-dividing-power-naive
      [p n]
      {:post [(nat-integer? %)]}
      (loop [p-to-e 1, e 0, ]
        (if (divides? p-to-e n)
          (recur (* p p-to-e) (inc e))
          (dec e))))
    (defn-
      find-power
      [p-to-e e]
      (+ e
         (max-dividing-power-naive
          p
          (quot n p-to-e))))
    (defn-
      find-start
      [p-to-e e]
      (let [p-to-e2 (sq p-to-e)]
        (cond
          (= p-to-e2 n) (duple e),
          (> p-to-e2 n) (find-power p-to-e e),
          (divides? p-to-e2 n) (if (divides? p (quot n p-to-e2))
                                 (find-start p-to-e2 (duple e))
                                 (duple e)),
          :else (find-power p-to-e e), )))
    (cond
      (one? p) 1,
      (not (divides? p n)) 0,
      :else (s/assert
             nat-integer?
             (find-start p 1))))
  (is
   (= (max-dividing-power 3 27) 3)
   (= (max-dividing-power
       3
       (duple 27))
      3)))
(with-test
  (defn-
    randint
    "helper for generating random integers for the pseudo-certain prime tester function"
    [a b]
    {:pre [(every? integer? [a b])]}
    (+ (biginteger
        (rand (+ b (- a) 1)))
       a))
  (is
   (= (vec
       (sort
        (keys
         (frequencies
          (take
           (pow 2 8)
           (repeatedly #(randint 2 8)))))))
      (inrange 2 8))))
(with-test
  (defn-
    sieve-prime?
    "basic sieve of Eratosthenes for small primes"
    [n]
    {:pre [(nat-integer? n)]}
    (cond
      (one? n) false,
      (<= n 3) true,
      (or (divides? 2 n)
          (divides? 3 n)) false,
      :else (loop [i 5]
              (if (<= (sq i) n)
                (if (or (divides? i n)
                        (divides? (+ 2 i) n))
                  false
                  (recur (+ i 6)))
                true))))
  (is (sieve-prime? 1020379))
  (is
   (not (sieve-prime? 1020378)))
  (is (not (sieve-prime? 1))))
(with-test
  (defn-
    prime-strong-pseudo-single?
    "this algorithm uses random integers to incrementally increase the certainty that a given number is prime"
    [n]
    {:pre [(nat-integer? n)]}
    (cond
      (<= 4 n) (let [a (randint 2 (- n 2))
                     g (gcd a n)]
                 (if (< 1 g)
                   g
                   (loop [ν 0, m (dec n), ]
                     (cond
                       (even? m) (recur (inc ν) (quot m 2)),
                       :else (let [b (modular-expt a m n)]
                               (cond
                                 (one? b) :probably-prime,
                                 :else (loop [i 0, b b, b-old b, ]
                                         (if (and (< i ν) (not (one? b)))
                                           (recur (inc i)
                                                  (mod (sq b) n)
                                                  b)
                                           (if (one? b)
                                             (let [g (gcd (inc b-old) n)]
                                               (if (or (one? g) (= g n))
                                                 :probably-prime g))
                                             :composite))), )))))),
      (one? n 1) :composite,
      :else :probably-prime, ))
  (is
   (= (prime-strong-pseudo-single? 4)
      2))
  (is
   (#{3 2}
    (prime-strong-pseudo-single? 6)))
  (is
   (#{:composite 4 2}
    (prime-strong-pseudo-single? 8)))
  (is
   (= (prime-strong-pseudo-single? 5)
      :probably-prime))
  (is
   (= (prime-strong-pseudo-single? 7)
      :probably-prime))
  (is
   (= (prime-strong-pseudo-single?
       11)
      :probably-prime)))
(defn-
  prime-strong-pseudo-explanation
  "runs the psuedo-prime function over 24 trials and returns the probability that the number is prime"
  [n]
  {:pre [(nat-integer? n)]}
  (loop [trials 24,
         result (prime-strong-pseudo-single? n), ]
    (cond
      (zero? trials) :very-probably-prime,
      (= result :probably-prime) (recur (dec trials)
                                        (prime-strong-pseudo-single? n)),
      :else result, )))
(defn-
  prime-strong-pseudo?
  "takes the output of `prime-strong-pseudo-explanation and returns a boolean"
  [n]
  {:pre [(nat-integer? n)]}
  (let [explanation (prime-strong-pseudo-explanation
                     n)]
    (or (= explanation
           :very-probably-prime)
        (true? explanation))))
(defn prime?
  "returns the nth prime number; uses 3 tiers of performance optimizations"
  [n]
  {:pre [(nat-integer? n)]}
  (let [n (abs n)]
    ((cond
       (< n 1000) (memoize sieve-prime?),
       (< n 1019999) sieve-prime?,
       :else prime-strong-pseudo?, )
     n)))
(declare prev-prime)
(with-test
  (defn next-prime
    "returns the first prime number greater than n"
    [n]
    {:pre [(integer? n)]}
    (cond
      (neg? n) (- (prev-prime (abs n))),
      (= n 0) 2,
      (= n 1) 2,
      (= n 2) 3,
      (even? n) (let [n+1 (inc n)]
                  (if (prime? n+1)
                    n+1
                    (next-prime n+1))),
      :else (let [n+2 (+ n 2)]
              (if (#{true
                     :probably-prime :very-probably-prime}
                   (prime? n+2))
                n+2
                (next-prime n+2))), ))
  (are
      [a b]
      (== (next-prime a) b)
      0 2,
      1 2,
      (pow 10 7) 10000019,
      (pow 10 8) 100000007, ))
(with-test
  (defn prev-prime
    "returns the first prime number less than n"
    [n]
    {:pre [(integer? n)]}
    (cond
      (neg? n) (- (next-prime (abs n))),
      (= n 3) 2,
      (< n 3) -2,
      (even? n) (let [n-1 (dec n)]
                  (if (prime? n-1)
                    n-1
                    (prev-prime n-1))),
      :else (let [n-2 (- n 2)]
              (if (#{true
                     :probably-prime :very-probably-prime}
                   (prime? n-2))
                n-2
                (prev-prime n-2))), ))
  (are
      [a b]
      (= (prev-prime a) b)
      10 7,
      8 7,
      17 13,
      10 7,
      8 7,
      17 13, ))
(with-test
  (defn odd-prime?
    "tests if `n` is odd and prime"
    [n]
    {:pre [(integer? n)]}
    (and (odd? n) (prime? n)))
  (is (odd-prime? 3))
  (is (not (odd-prime? 2))))
(with-test
  (defn next-primes
    "returns the next `primes-wanted` primes greater than `m`"
    [m primes-wanted]
    {:pre [(nat-integer? primes-wanted)
           (integer? m)]}
    ((fn loop [n primes-wanted]
       (if (zero? primes-wanted)
         []
         (let [next (next-prime n)]
           (if next
             (cons
              next
              (loop next (dec primes-wanted)))
             []))))
     m
     primes-wanted))
  (is
   (= (next-primes -5 10)
      [-3 -2 2 3 5 7 11 13 17 19])))
(with-test
  (defn prev-primes
    "returns the next `primes-wanted` primes less than `m`"
    [m primes-wanted]
    {:pre [(nat-integer? primes-wanted)
           (integer? m)]}
    ((fn loop [n primes-wanted]
       (if (zero? primes-wanted)
         []
         (let [prev (prev-prime n)]
           (if prev
             (cons
              prev
              (loop prev (dec primes-wanted)))
             []))))
     m
     primes-wanted))
  (= (prev-primes 5 10)
     [3
      2
      -2
      -3
      -5
      -7
      -11
      -13
      -17
      -19]))
(with-test
  (defn nth-prime
    "returns the nth positive prime number"
    [n]
    {:pre [(nat-integer? n)]}
    (loop [n n, p 2, ]
      (if (zero? n)
        p
        (recur (dec n) (next-prime p)))))
  (are
      [a b] (= (nth-prime a) b),
      0 2,
      1 3,
      2 5, ))
(with-test
  (defn random-prime
    [n]
    {:pre [(< 2 n)]}
    (let [r (rand-int n)]
      (if (prime? r)
        r
        (random-prime n))))
  (is
   (nil?
    (some
     #{4}
     (take
      64
      (repeatedly #(random-prime 8))))))
  (is
   (some
    #{7}
    (take
     64
     (repeatedly #(random-prime 8))))))
(defn-
  factorize-small
  "factorize uses this for `n` less than 1000"
  [n]
  {:pre [(integer? n)]}
  (into
   {}
   ((fn f
      [n p]
      (cond
        (< n p) [],
        (= n p) [[p 1]],
        (prime? n) [[n 1]],
        (divides? p n) (vec
                        (let [m (max-dividing-power p n)]
                          (cons
                           [p m]
                           (f
                            (quot n (pow p m))
                            (next-prime p))))),
        :else (recur n (next-prime p)), ))
    n
    2)))
(defn-
  integer-sqrt
  "helper for `integer-root`"
  [n]
  {:pre [(integer? n)]}
  (int (Math/floor (sqrt n))))
(with-test
  (defn-
    bit-shift
    "multiplies a by `(pow 2 b)` by using the binary operation on a java biginteger: https://stackoverflow.com/questions/141525/what-are-bitwise-shift-bit-shift-operators-and-how-do-they-work#141873"
    [a b]
    {:pre [(every? integer? [a b])]}
    (.shiftLeft (biginteger a) b))
  (are
      [a b]
      (= (apply bit-shift a) b)
      [1 10] 1024,
      [255 -3] 31, ))
(with-test
  (defn-
    bit-length
    "returns the number of bits in the two's complement representation of n (like a photographic negative of it's binary representation ) excluding the sign bit: https://www.tutorialspoint.com/java/math/biginteger_bitlength.htm"
    [n]
    {:pre [(integer? n)]}
    (.bitLength (biginteger n)))
  (are
      [a b] (= (bit-length a) b),
      8 4,
      -8 3, ))
(defn integer-root
  "returns the largest base where `(<= (pow base y) x)"
  [x y]
  {:pre [(every? nat-integer? [x y])]
   :post [(nat-integer? %)]}
  (biginteger
   (let [x (long x)]
     (cond
       (or (one? y) (#{0 1} x)) x,
       (= y 2) (integer-sqrt x),
       :else (let [length (bit-length x)]
               (cond
                 (<= length y) 1,
                 (<= length (duple y)) (if (< x (Math/pow 3 y)) 2 3),
                 (even? y) (integer-root
                            (integer-sqrt x)
                            (quot y 2)),
                 :else (s/assert
                        nat-integer?
                        (let [divided-length (long
                                              (quot (quot (dec length) y) 2))
                              top-bits (bit-shift
                                        x
                                        (- (duple divided-length)))
                              nth-root-top-bits (integer-root top-bits y)]
                          (loop [g (bit-shift
                                    (inc nth-root-top-bits)
                                    divided-length), ]
                            (let [a (pow
                                     g
                                     (s/assert nat-integer? (dec y)))
                                  b (* a y)
                                  c (* a (dec y))
                                  d (quot (+ x (* g c)) b)
                                  diff (- d g)]
                              (cond
                                (not (neg? diff)) g,
                                (< diff -1) (recur d),
                                :else (loop [g d]
                                        (if (not (< x (pow g y)))
                                          g
                                          (recur (dec g)))))))))))))))
(defn integer-root-remainder
  "returns a pair of the integer root and the difference between `a` and `(pow integer-root n)`"
  [a n]
  {:pre [(every? nat-integer? [a n])]}
  (let [i (integer-root a n)]
    [i
     (biginteger
      (s/assert
       nat-integer?
       (- a (pow i n))))]))
(defn-
  simple-as-power
  "returns a base and the largest exponent possible, where `(= a (pow base exponent))`"
  [a]
  {:pre [(pos-integer? a)]}
  (loop [n (bit-length a)]
    (let [[root remainder] (integer-root-remainder
                            a
                            (inc n))]
      (if (zero? remainder)
        [root
         (biginteger
          (s/assert nat-integer? (inc n)))]
        (if (pos? n)
          (recur (dec n))
          (throw
           (Exception.
            "internal error in adamdavislee.number-theory/simple-as-power")))))))
(defn-
  combine-bases
  "`pollard-factorize` doesn't combine all of the bases that have the same exponents"
  [messy-factorization]
  (reduce
   (fn [result [b e]]
     (assoc result
            b
            (+ (or (result b) 0) e)))
   {}
   messy-factorization))
(defn-
  simple-perfect-power
  "like `simple-as-power` but returns `nil` if `a` can't be expressed as a power of two integers with an exponent greater than 1"
  [a]
  {:pre [(pos-integer? a)]}
  (and (not (zero? a))
       (let [[base n] (simple-as-power a)]
         (if (and (< 1 n) (< 1 a))
           [base n]))))
(defn-
  pollard
  "more efficient algorithm for factoring large integers: https://www.cs.colorado.edu/~srirams/courses/csci2824-spr14/pollardsRho.html)"
  [n]
  {:pre [(pos-integer? n)]}
  (let [x0 (biginteger (rand n))]
    (loop [xi x0, yi x0, i 0, g 1, ]
      (if (or (< 1 g n) (> i (sqrt n)))
        (if (< 1 g n) g)
        (recur (rem (inc (sq xi)) n)
               (rem
                (inc (sq (inc (sq yi))))
                n)
               (inc i)
               (gcd (- xi yi) n))))))
(defn-
  pollard-factorize
  "builds on `pollard` to factorize large integers: https://www.cs.colorado.edu/~srirams/courses/csci2824-spr14/pollardsRho.html"
  [n]
  {:pre [(pos-integer? n)]}
  (let [n (biginteger n)]
    (cond
      (one? n) [],
      (prime? n) [[n 1]],
      (even? n) (concat [[2 1]]
                        (pollard-factorize (quot n 2))),
      (divides? 3 n) (concat [[3 1]]
                             (pollard-factorize (quot n 3))),
      (simple-perfect-power n) (let [base-and-exp (simple-perfect-power n)]
                                 (if (prime? (first base-and-exp))
                                   [base-and-exp]
                                   (map
                                    (fn [b-and-e]
                                      [(first b-and-e)
                                       (* (second base-and-exp)
                                          (second b-and-e))])
                                    (pollard-factorize
                                     (first base-and-exp))))),
      :else (loop [divisor (pollard n)]
              (if divisor
                (concat (pollard-factorize divisor)
                        (pollard-factorize
                         (quot n divisor)))
                (recur (pollard n)))))))
(with-test
  (defn factorize
    "returns a map with keys corresponding to bases and values corresponding to exponents to represent `n` as a product of primes"
    [n]
    {:pre [(pos-int? n)]}
    ((if (< n 1000)
       factorize-small
       (comp
        combine-bases
        pollard-factorize))
     n))
  (are
      [a b] (= (factorize a) b),
      12 {2 2, 3 1, },
      (* 10000019 100000007) {10000019 1, 100000007 1, }, ))
(with-test
  (defn defactorize
    "takes the output of `factorize` and returns the integer represented"
    [coll]
    (apply
     * (mapv #(apply pow %) coll)))
  (is
   (let [n (+ (rand-int 1024) 512)]
     (= n
        (defactorize (factorize n)))))
  (is
   (= 1000001970000133
      (defactorize
        (factorize 1000001970000133)))))
(with-test
  (defn as-power
    "returns a base and the largest exponent possible, where `(= a (pow base exponent))`"
    [a]
    {:pre [(pos-int? a)]}
    (let [r (apply
             gcd
             (map second (factorize a)))]
      [(integer-root a r) r]))
  (are
      [a r n] (let [[b e] (as-power a)]
                (and (= b r) (= e n))),
      27 3 3,
      28 28 1,
      (* 5 5 7 7 7) (* 5 5 7 7 7) 1,
      (* 5 5 7 7 7 7) 245 2, ))
(with-test
  (defn prime-power
    "if n is a power of a single prime number then the prime and exponent are returns otherwise `nil`"
    [n]
    {:pre [(nat-integer? n)]}
    (let [factorization (factorize n)]
      (if (one? (count factorization))
        (first factorization))))
  (is (prime-power (pow 3 7)))
  (is
   (not (prime-power (pow 12 7)))))
(with-test
  (def prime-power?
    "like `prime-power` but returns a boolean"
    (detruthy prime-power))
  (is
   (true?
    (prime-power? (pow 3 7))))
  (is
   (not (true?
         (prime-power? (pow 12 7))))))
(defn odd-prime-power?
  "like `prime-power?` but adds condition that the prime base must be odd"
  [n]
  {:pre [(nat-integer? n)]}
  (let [p-e (prime-power n)]
    (and p-e (odd? (first p-e)))))
(with-test
  (defn perfect-power
    "like `as-power` but returns `nil` if `a` can't be expressed as a power of two integers with an exponent greater than 1"
    [a]
    {:pre [(pos-int? a)]}
    (and (not (zero? a))
         (let [[base n] (as-power a)]
           (and (> n 1) (> a 1) [base n]))))
  (are
      [a b] (= (perfect-power a) b),
      3 false,
      9 [3 2],
      (pow 12 7) [12 7],
      (dec (pow 12 7)) false, ))
(with-test
  (def perfect-power?
    "like `perfect-power` but returns a boolean"
    (detruthy perfect-power))
  (are
      [a b] (= (perfect-power? a) b),
      3 false,
      9 true,
      (pow 12 7) true,
      (dec (pow 12 7)) false, ))
(with-test
  (def factorization-divisors
    "returns the input's prime divisors (the keys in the output of `factorize`"
    (comp
     (partial map first)
     factorize))
  (is
   (= (factorization-divisors 360)
      [2 3 5])))
(with-test
  (def factorization-exponents
    "returns the input's prime "
    (comp
     (partial map second)
     factorize))
  (is
   (= (factorization-exponents 360)
      [3 2 1])))
(with-test
  (def prime-omega
    "returns how many prime factors a number has"
    (comp
     (partial apply +)
     factorization-exponents))
  (is (= (prime-omega 360) 6)))
(with-test
  (defn-
    powers-of
    ""
    [base n]
    (take
     (inc n)
     (iterate (partial * base) 1)))
  (is
   (= (powers-of 2 4)
      [1 2 4 8 16])))
(defn-
  factorization->divisors
  [factorization]
  (if (empty? factorization)
    [1]
    (let [[[base exponent] & pairs] factorization
          other-divisors (factorization->divisors pairs)]
      (apply
       concat (map
               (fn [p-to-i]
                 (map
                  (fn [d] (* p-to-i d))
                  other-divisors))
               (powers-of base exponent))))))
(with-test
  (defn divisors
    "returns a list of all a number's positive divisors (not only prime divisors)"
    [n]
    {:pre [(integer? n)]}
    (if (zero? n)
      []
      (sort
       (factorization->divisors
        (factorize (abs n))))))
  (are
      [a b]
      (= (divisors a) b)
      12 [1 2 3 4 6 12],
      -12 [1 2 3 4 6 12],
      0 [], ))
(with-test
  (defn totient
    "returns the number of integers from 1 to n that are coprime with n; also called the phi function"
    [n]
    {:pre [(nat-integer? n)]}
    (let [ps (factorization-divisors n)]
      (s/assert
       nat-integer?
       (* (quot n (apply * ps))
          (apply * (map dec ps))))))
  (are
      [a b] (= (totient a) b),
      9 6,
      128 64, ))
(with-test
  (defn moebius-mu
    "mu(n) =  1  if n is a product of an even number of primes
                    = -1  if n is a product of an odd number of primes
                    =  0  if n has a multiple prime factor"
    [n]
    {:pre (nat-integer? n)}
    (if (every?
         one?
         (factorization-exponents n))
      (if (even?
           (count
            (factorization-divisors n)))
        1
        -1)
      0))
  (are
      [a b] (= (moebius-mu a) b),
      (* 2 3 5) -1,
      (* 2 3 5 7) 1,
      (* 2 2 3 5 7) 0, ))
(with-test
  (defn divisor-sum
    "returns sum of the kth powers of all divisors of n"
    ([n]
     {:pre (nat-integer? n)}
     (divisor-sum n 1))
    ([n k]
     {:pre (every? nat-integer? [n k])}
     (let [f (factorize n)
           ps (map first f)
           es (map second f)
           divisor-sum0 (fn [p e] (inc e))
           divisor-sum1 (fn [p e]
                          (loop [sum 1, n 0, p-to-n 1, ]
                            (if (= n e)
                              sum
                              (let [t (* p p-to-n)]
                                (recur (+ t sum) (inc n) t)))))
           divisor-sumk (fn [p e]
                          (let [p-to-k (pow p k)]
                            (loop [sum 1, n 0, p-to-kn 1, ]
                              (if (= n e)
                                sum
                                (let [t (* p-to-k p-to-kn)]
                                  (recur (+ t sum) (inc n) t))))))]
       (s/assert
        nat-integer?
        (apply
         * (map
            (case k,
              0 divisor-sum0,
              1 divisor-sum1, divisor-sumk, )
            ps
            es))))))
  (is (= (divisor-sum 12 2) 210)))
(with-test
  (defn mangoldt-lambda
    "if n is equal to `(pow prime-base pos-int-exponent)` then returns the natural log of n, otherwise 0; known as the 'von Mangoldt' function"
    [n]
    {:pre [(nat-integer? n)]}
    (let [am (prime-power n)]
      (if (coll? am)
        (Math/log (first am))
        0)))
  (are
      [a b] (== (mangoldt-lambda a) b),
      (* 3 3) (Math/log 3),
      12 0, ))
(with-test
  (defn unit-group
    "returns the unit group modulo n (unit groups have to do with the existence of a number that when mulitplied with the first returns 1 or an analogous identity value: https://en.wikipedia.org/wiki/Multiplicative_group_of_integers_modulo_n"
    [n]
    {:pre [(pos-int? n)]}
    (filter
     (fn [m] (coprime? m n))
     (inrange (dec n))))
  (is
   (= (unit-group 20)
      [1 3 7 9 11 13 17 19])))
(with-test
  (defn unit-group-order
    "every element from `unit-group` has an 'order', returns the order of `g` in the unit group of `n` (about modular unit groups: https://en.wikipedia.org/wiki/Multiplicative_group_of_integers_modulo_n)"
    [g n]
    {:pre [(every? pos-int? [g n])
           (coprime? g n)]}
    (with-modulus
      n
      (loop [k 1, a g]
        (if (mod= a 1)
          k
          (recur (inc k) (mod* a g))))))
  (are
      [a b]
      (= (apply unit-group-order a)
         b)
      [19 20] 2,
      [3 20] 4, ))
(with-test
  (defn unit-group-orders
    "returns every order in `n`'s unit group"
    [n]
    {:pre [(pos-int? n)]}
    (map
     (fn [m] (unit-group-order m n))
     (unit-group n)))
  (is
   (= (unit-group-orders 20)
      [1 4 4 2 2 4 4 2])))
(with-test
  (defn exists-primitive-root?
    "returns `true` if the `n`'s unit group has a primitive root (i.e. it's cyclic), otherwise returns `false`; in other words, tests if n is either 1, 2, 4, `(pow odd-prime exponent)`, or `(* 2 (pow odd-prime exponent))`"
    [n]
    {:pre [(pos-int? n)]}
    (cond
      (some #{2} [1 2 4]) true,
      (odd? n) (odd-prime-power? n),
      :else (odd-prime-power? (quot n 2), )))
  (every?
   exists-primitive-root?
   [1 2 4 3 9 6 18])
  (some
   exists-primitive-root?
   [8 16 12]))
(with-test
  (defn primitive-root
    "returns a primitive-root of `n`'s unit group or `false`"
    [n]
    {:pre [(pos-int? n)]}
    (cond
      (not (exists-primitive-root? n)) false,
      (and (odd-prime-power? n)
           (not (prime? n))) (let [pp (prime-power n)
                                   p (if pp
                                       (first pp)
                                       (throw
                                        (Exception. "internal error")))
                                   gg (primitive-root p)
                                   g (or gg
                                         (throw
                                          (Exception. "internal error")))]
           (if (= (unit-group-order g (sq p))
                  (totient (sq p)))
             g
             (mod (+ g p) n))),
      (and (even? n)
           (odd-prime? (quot n 2))) (let [gg (primitive-root (quot n 2))
                                          g (or gg
                                                (throw
                                                 (Exception. "internal error")))]
           (if (odd? g)
             g
             (mod (+ g (quot n 2)) n))),
      :else (let [phi-n (totient n)
                  , qs
                  (factorization-divisors phi-n),
                  primitive-root? (fn [g]
                                    (with-modulus
                                      n
                                      (every?
                                       identity
                                       (map
                                        (fn [q]
                                          (not (mod=
                                                (modexpt g (quot phi-n q))
                                                1)))
                                        qs))))
                  , ]
              (loop [g 1]
                (cond
                  (= g n) false,
                  (not (coprime? g n)) (recur (inc g)),
                  (primitive-root? g) g,
                  :else (recur (inc g)), ))), ))
  (is
   (false? (primitive-root 20)))
  (is (= (primitive-root 10) 7)))
(with-test
  (defn primitive-root?
    "tests if `g` in `n`'s unit group is a primitive root modulo `n`"
    [g n]
    {:pre [(every? pos-int? [g n])
           (coprime? g n)]}
    (let [phi-n (totient n)]
      (with-modulus
        n
        (every?
         identity
         (map
          (fn [q]
            (not (mod=
                  (modexpt g (quot phi-n q))
                  1)))
          (factorization-divisors phi-n))))))
  (is (primitive-root? 7 10))
  (is
   (false? (primitive-root? 7 20))))
(with-test
  (defn primitive-roots
    "returns all the primitive roots in `n`'s unit group"
    [n]
    {:pre [(pos-int? n)]}
    (if (not (exists-primitive-root? n))
      []
      (let [phi-n (totient n)
            qs (factorization-divisors phi-n)
            primitive-root? (fn [g]
                              (with-modulus
                                n
                                (every?
                                 identity
                                 (map
                                  (fn [q]
                                    (not (mod=
                                          (modexpt g (quot phi-n q))
                                          1)))
                                  qs))))]
        (loop [g 1, roots [], ]
          (cond
            (= g n) (reverse roots),
            (not (coprime? g n)) (recur (inc g) roots),
            (primitive-root? g) (recur (inc g) (cons g roots)),
            :else (recur (inc g) roots), )))))
  (is
   (= (primitive-roots 10) [3 7])))
(with-test
  (defn quadratic-character
    "modulo the prime number `p`, if `a` is a quadratic residue returns 1, returns -1 otherwise (if `a` is 0 returns 0) (if p isn't a prime number, the function is indeterminate) (this function is known as the Legendre Symbol: https://brilliant.org/wiki/legendre-symbol)"
    [a p]
    {:pre [(nat-int? a) (pos-int? p)]}
    (let [l (modular-expt
             a
             (quot (dec p) 2)
             p)]
      (if (or (== l 0) (== l 1))
        l
        -1)))
  (are
      [a b] (= (apply quadratic-character a)
               b),
      [2 5] -1,
      [3 5] -1,
      [5 5] 0,
      [7 5] -1,
      [11 5] 1, ))
(with-test
  (defn quadratic-residue?
    "tests if `a` is a quadratic residue modulo `n`; a quadratic residue is an integer `a` that modulo `n` is congruent to some squared integer: https://docs.racket-lang.org/math/number-theory.html#%28def._%28%28lib._math%2Fnumber-theory..rkt%29._quadratic-character%29%29"
    [a n]
    {:pre [(nat-integer? a)
           (nat-integer? n)]}
    (let [ps (factorization-divisors n)
          odd-ps (if (= (first ps) 2)
                   (rest ps)
                   ps)]
      (and (every?
            #(one?
              (quadratic-character a %))
            odd-ps)
           (cond
             (divides? 8 n) (one? (mod a 8)),
             (divides? 4 n) (one? (mod a 4)),
             :else true))))
  (are
      [a] (apply quadratic-residue? a),
      [1 17]
      [2 17]
      [4 17]
      [8 17]
      [9 17]
      [13 17]
      [15 17]
      [16 17])
  (are
      [a] (not (apply quadratic-residue? a)),
      [3 17]
      [5 17]
      [6 17]
      [7 17]
      [10 17]
      [11 17]
      [12 17]
      [14 17]))
(with-test
  (defn jacobi-symbol
    "a generalization of quadratic-character: https://brilliant.org/wiki/jacobi-symbol/"
    [a n]
    {:pre [(nat-integer? a)
           (odd? n)
           (pos-int? n)]
     :post [((set (inrange -1 1)) %)]}
    (if (one? n)
      1
      (let [prime-factors (factorize n)]
        ((fn next
           [[factor & remaining-factors]]
           (let [qcap (quadratic-character
                       a
                       (first factor))]
             (if-not
                 remaining-factors
                 (pow qcap (second factor))
                 (* (pow qcap (second factor))
                    (next remaining-factors)))))
         prime-factors))))
  (are
      [a b]
      (= (apply jacobi-symbol a) b)
      [0 23] 0,
      [1 1] 1,
      [2 1] 1,
      [4 1] 1,
      [2 3] -1,
      [4 5] 1,
      [7 5] -1,
      [5 3] -1,
      [25 53] 1,
      [21 1] 1,
      [21 21] 0,
      [12 3] 0,
      [30 59] -1,
      [7 51] -1,
      [22 55] 0, ))
(successful? (run-tests))
