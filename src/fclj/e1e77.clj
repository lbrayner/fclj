(ns fclj.e1e77
  (:gen-class))

(defn count-seq [x] (if (= x []) 0 (+ 1 (count-seq (rest x)))))

(defn reverse-seq [x] 
  (if (= (count x) 1) x 
    (concat (reverse-seq (rest x)) [(first x)])))

(defn sum-it-all [x] 
  (if (= x []) 0 
    (+ (first x) (sum-it-all (rest x)))))

(defn odd-numbers [x] 
  (filter #(not= (rem % 2) 0) x))

(defn n-fibonacci [x] 
  (if (<= x 2) [1 1] 
    (let [fib (n-fibonacci (- x 1))
          rfib (reverse fib)]
      (concat fib [(+ (first rfib) (first (rest rfib)))]))))

(defn palindrome? [x] 
  (let [rev (reverse x)
        sq (seq x)]
    (= sq rev)))

(defn do-flatten [x] 
  (if (= x []) []
   (let [i (first x)]
     (if (not (sequential? i))
       (concat [i] (do-flatten (rest x)))
       (let [j (first i)
             k (rest i)]
         (do-flatten (concat [j] k (rest x))))))))

(defn capials [x] 
  (if (= x []) ""
   (let [i (first x)]
     (if (and (>= (compare i \A) 0) (<= (compare i \Z) 0))
       (str i (capials (rest x)))
       (capials (rest x))))))

(defn rem-dup [x] 
  (if (= x []) []
   (let [i (first x)
         r (rest x)
         j (first r)]
     (if (= i j)
       (rem-dup r)
       (concat [i] (rem-dup r))))))

(defn pack-a-seq [x] 
  (let [f (fn anon [b x] ; boolean collection
    (if (= (count x) 1) x 
      (let [h (first x)
            t (rest x)]
        (if (and b (sequential? h))
          (let [i (first h)
                j (first t)]
            (if (= i j)
              (anon true (concat (list (conj h j)) (rest t)))
              (concat (list h) (anon true (concat (list (list j)) (rest t))))))
          (anon true (concat (list (list h)) t))))))]
    (f false x)))


; (defn t2 [coll] 
;   (reduce #(let [i (peek %1)]
;              (cond
;               (= (first i) %2)
;               (conj (pop %1) (conj i %2))
              
;               :else
;               (conj %1 (list %2))))
;           [] coll))

(defn duplicate [x] 
  (if (= x []) '() 
    (let [h (first x)]
      (concat (list h h) (duplicate (rest x))))))

(defn repl [y i]
 (if (<= i 0) '() 
   (concat (list y) (repl y (- i 1)))))

(defn rplct [x n] 
  (let [f (fn anon [y i]
           (if (<= i 0) '() 
             (concat (list y) (anon y (- i 1)))))]
    (if (= x []) '() 
     (concat (f (first x) n) (rplct (rest x) n)))))

(defn reihe [a b] 
  (if (< a b) (concat (list a) (reihe (+ a 1) b)) '()))

(defn maximun [& args] 
  (let [f (fn anon [m y] ; integer collection
           (if (= y []) m
             (let [h (first y)
                   t (rest y)]
               (if (> h m) (anon h t) (anon m t)))))]
    (f 0 args)))

(defn interlv [x y] 
  (if (or (= x []) (= y [])) '()
    (concat (list (first x) (first y)) (interlv (rest x) (rest y)))))

(defn interps [x y] 
  (if (<= (count y) 1) y
    (concat (list (first y) x) (interps x (rest y)))))

(defn drop-every-nth [x i] 
  (let [f (fn anon [m n y] ; integer integer collection
           (if (= y []) '() 
             (if (<= m 1)
               (anon n n (rest y))
               (concat (list (first y)) (anon (- m 1) n (rest y))))))]
    (f i i x)))

(defn fact [x] 
  (if (= x 1) 1 (* x (fact (- x 1)))))



(defn every-nth [s n]
  (let [f (fn anon [m i l]
            (if (= l []) '()
              (if (<= m 1)
                (concat (list (first l)) (anon i i (rest l)))
                (anon (- m 1) i (rest l)))))]
    (f n n s)))

(defn shift [s n]
  (if (<= n 0) s
    (shift (concat (rest s) (list (first s))) (- n 1))))

(defn rleave [x y]
  (let [lshift (fn anon [s n]
                (if (<= n 0) s
                  (anon (concat (rest s) (list (first s))) (- n 1))))
        every-nth (fn [s n]
                   (let [f (fn anon [m i l]
                             (if (= l []) '()
                               (if (<= m 1)
                                 (concat 
                                   (list (first l)) 
                                   (anon i i (rest l)))
                                 (anon (- m 1) i (rest l)))))]
                     (f n n s)))
        f (fn anon [s n c]
            (if (<= c 0) '()
              (concat 
                (list (conj (every-nth (rest s) n) (first s))) 
                (anon (lshift s 1) n (- c 1)))))]
    (f x y y)))

(defn absolute [n] (max n (-' n)))

(defn rotate [y x]
  (let [lshift (fn anon [s n]
                (if (<= n 0) s
                  (anon (concat (rest s) (list (first s))) (- n 1))))
        reverse-seq (fn anon [s]
                (if (= (count s) 1) s 
                 (concat (anon (rest s)) [(first s)])))
        abs (fn [n] (max n (-' n)))]
    (if (< y 0)
      (reverse-seq (lshift (reverse-seq x) (abs y)))
      (lshift x y))))

; (defn flip [x]
;  (let [f (comp reverse vector)
;        g (partial apply x)]
;    (comp g f)))

(defn flip [x]
 (comp (partial apply x) reverse vector))

(defn split-seq [n x]
 (let [f (fn anon [i y]
           (let [l (first y)
                 r (first (rest y))
                 c (count l)]
             (if (< c i)
               (anon i (list (conj l (first r)) (rest r))) y)))]
   (f n (list [] x))))


(defn discriminate [x i v]
  (if (= (count v) i) x
    (let [n (nth v i)]
      (if (empty? x) 
       (discriminate [n] (inc i) v)
       (let [k (type (first x))]
         (if (= k (type n))
           (discriminate (conj x n) (inc i) v)
           (discriminate x (inc i) v))))))) 

(defn drop-nth [x n]
  (let [v (vec x)
        c (count v)]
    (concat 
      (subvec v 0 n) 
      (subvec v (inc n) c))))

(defn split-by-type [x]
  (let [d (fn anon [x i v]
            (let [c (count v)]
              (if (>= i c) (list x v) 
                (let [n (nth v i)]
                  (if (empty? x) 
                    (anon [n] i (vec (concat 
                                       (subvec v 0 i) 
                                       (subvec v (inc i) c))))
                    (let [k (type (first x))]
                      (if (= k (type n))
                        (anon (conj x n) i
                              (vec (concat 
                                     (subvec v 0 i) 
                                     (subvec v (inc i) c))))
                        (anon x (inc i) v))))))))
        e (fn anon [x]
            (let [p (d [] 0 x)
                  f (first p)
                  s (second p)]
              (if (empty? s) [f]
                (concat [f] (anon s)))))]
    (set (e x))))

(defn find-num-seq [v]
  (let [d (fn [x n] ; drop-nth
            (let [v (vec x)
                  c (count v)]
              (concat 
                (subvec v 0 n) 
                (subvec v (inc n) c))))
        f (fn anon [s i v]
            (let [c (count v)
                  z (list s v)]
              (if (>= i c) z
                (let [n (nth v i)]
                  (if (empty? s) 
                    (anon [n] i (vec (d v i)))
                    (let [l (peek s)]
                      (if (= (inc l) n)
                        (anon (conj s n) i
                              (vec (d v i)))
                        z)))))))
        g (fn anon [p]
            (let [[i j] p]
              (if (empty? j) [i]
                (let [a (anon (f [] 0 j))]
                  (if (empty? i) a
                    (concat [i] a))))))
        k (g (list [] v))
        l (sort #(compare (count %2) (count %1)) k)
        m (first l)]
   (if (> (count m) 1) m [])))
; Mit compare den ersten gegen den zweiten Parameter getauscht

(defn part [n x]
  (if (< (count x) n) '()
    (concat (list (take n x)) (part n (drop n x)))))

(defn freq [x]
  (let [f (fn anon [m s]
            (if (empty? s) m
              (let [[n & r] s
                    c (m n)]
                (if (nil? c)
                  (anon (conj m [n 1]) r)
                  (anon (conj m [n (inc c)]) r)))))]
   (f {} (into '() x))))

(defn dist [x]
  (let [d (fn [x n] ; drop-nth
            (let [v (vec x)
                  c (count v)]
              (concat 
                (subvec v 0 n) 
                (subvec v (inc n) c))))
        f (fn anon [s i v]
            (if (>= i (count v)) v
              (let [n (nth v i)]
                (if (contains? s n)
                  (anon s i (vec (d v i)))
                  (anon (conj s n) (inc i) v)))))]
   (seq (f #{} 0 (vec x)))))

(defn foo [x] (when (> x 0) (conj (foo (dec x)) x)))

(defn bar [& xs] (if (= (count xs) 1) [(first xs)]
                   (concat [(first xs)] (apply bar (rest xs)))))
; geschachtelte Sammlungen statt concat Anweisungen

(defn compose [& fs]
  (if (<= (count fs) 1) (first fs)
   (let [v (vec fs)
         f (peek v)]
     (fn [& args] ((apply compose (pop v)) (apply f args))))))
; pop für rest verwechselt

(defn juxtapose [& fs]
  (let [j (fn anon [v fs & args]
            (if (empty? fs) v
              (let [f (first fs)]
                (apply anon (conj v (apply f args)) (rest fs) args))))]
    (fn [& args] (apply j [] fs args))))
; Mit apply und einer seq als vorletzterer Parameter den letzeren vergaß

(defn reduce-steps
  ([f xs] (reduce-steps f (first xs) (rest xs)))
  ([f v xs] (let [r (fn anon [f v xs]
                      (if (empty? xs) '()
                      ; (if (= (count xs) 0) '()
                       (let [c (f v (first xs))]
                         (lazy-seq (cons c (anon f c (rest xs)))))))]
              (cons v (r f v xs)))))
; count auf eine lazy-seq anwenden

(defn build-map [vk vv]
  (let [b (fn anon [m ks vs]
            (if (or (empty? ks) (empty? vs)) m
              (let [k (first ks)
                    v (first vs)]
                (anon (conj m [k v]) (rest ks) (rest vs)))))]
    (b {} vk vv)))

(defn iter [f v]
  (let [i (fn anon [f v]
            (let [k (f v)]
             (lazy-seq (cons k (anon f k)))))]
    (cons v (i f v))))

(defn grp-by [f xs]
  (let [g (fn anon [m f xs]
            (if (empty? xs) m
              (let [x (first xs)
                    fx (f x)
                    v (m fx)]
                (anon 
                  (conj m [fx (vec (conj v x))])
                  f 
                  (rest xs)))))]
    (g {} f xs)))

(defn typify [xs]
  (let [k 0
        cm (conj xs xs)
        qm (count cm)
        qxs (count xs)]
    (if (= qm qxs) :map
      (let [ck (conj xs k)
            ckk (conj ck k)
            qk (count ckk)]
        (if (< qk (+ qxs 2)) :set
         (let [cmk (conj cm k)
               h (first cmk)]
           (if (= h k) :list :vector)))))))

(defn ggt [x y] ; größter gemeinsamer Teiler
  (let [a (max x y)
        i (min x y)
        f (fn [c x y]
            (if (= c 1) 1
              (if (and (zero? (rem x c)) (zero? (rem y c))) c 
                (recur (dec c) x y))))]
    (f i a i)))

(defn prime? [x]
 (if (< x 2) false
   (if (= x 2) true
    (let [p?' (fn [d x]
                (if (= d 1) true
                  (if (zero? (rem x d)) false
                    (recur (dec d) x))))]
      (p?' (dec x) x)))))

(defn next-prime [x]
  (if (prime? x) x
    (recur (inc x))))

(defn n-primes [x]
  (let [p? (fn [x]
             (if (< x 2) false
               (if (= x 2) true
                (let [p?' (fn [d x]
                            (if (= d 1) true
                              (if (zero? (rem x d)) false
                                (recur (dec d) x))))]
                  (p?' (dec x) x)))))
        n (fn [x]
            (if (p? x) x
              (recur (inc x))))
        f (fn anon [c s]
            (if (<= c 0) []
              (let [np (n (inc s))]
                (concat [np] (anon (dec c) np)))))] 
    (f x 1)))
; inc statt einfaches Wertes

(defn mergew [f & ms]
  (let [m (fn [f m1 m2]
            (let [m' (fn [f ks m1 m2]
                       (if (empty? ks) m1
                         (let [k (first ks)
                               rk (rest ks)
                               v1 (m1 k)
                               v2 (m2 k)]
                           (if (nil? v1)
                             (recur f rk (conj m1 [k v2]) m2)
                             (recur f rk (conj m1 [k (f v1 v2)]) m2)))))]
              (m' f (keys m2) m1 m2)))
        mgw (fn [f ms]
              (let [[t d & rm] ms]
                (if (nil? d) t
                 (recur f (cons (m f t d) rm)))))]
    (mgw f ms)))

(defn sort-words [ws]
  (let [c (fn [w1 w2]
            (compare (.toUpperCase w1) (.toUpperCase w2)))
        s (fn [s]
            (seq (.split s "[^a-zA-Z]+")))]
    (sort c (s ws))))

(defn diag [rs]
  (let [gd' (fn anon [n rs]
              (if (empty? rs) []
                (concat
                  [(nth (first rs) n)]
                  (anon (inc n) (rest rs)))))]
    (gd' 0 rs)))
; einfacher Wert statt inc
; nth statt first

(defn tttoe [b]
  (let [x #{:x}
        o #{:o}
        u (fn [v]
            (let [s (reduce conj #{} v)]
              (if (= s x) :x
                (if (= s o) :o nil))))
        gd (fn [rs]
            (let [gd' (fn [n v rs]
                        (if (empty? rs) v
                          (recur
                            (inc n)
                            (conj v (nth (first rs) n))
                            (rest rs))))]
              (gd' 0 [] rs)))
        [r1 r2 r3] b
        r b
        c (map vector r1 r2 r3)
        d [(gd r) (gd (map reverse r))]
        f (fn [vs]
            (if (empty? vs) nil
              (let [o (u (first vs))]
                (if (nil? o)
                  (recur (rest vs)) o))))
        g (fn [bvs]
            (if (empty? bvs) nil
              (let [o (f (first bvs))]
                (if (nil? o)
                  (recur (rest bvs)) o))))]
    (g [r c d])))

(defn perf-sqrs [s]
  (let [prs (fn [s]
              (map read-string (seq (.split s ","))))
        cvr (fn [xs]
              (apply str (rest (interleave (repeat ",") xs))))
        perfs? (fn [n]
                (let [sqr (. Math round (. Math sqrt n))]
                  (= n (* sqr sqr))))]
    (cvr (filter perfs? (prs s)))))

(defn tot [x]
  (let [g (fn [x y] ; größter gemeinsamer Teiler
			(let [a (max x y)
				  i (min x y)
				  f (fn [c x y]
					  (if (= c 1) 1
						(if (and 
                             (zero? (rem x c))
                             (zero? (rem y c))) c 
						  (recur (dec c) x y))))]
			  (f i a i)))
        t (fn [c i x]
            (if (>= i x) c
              (let [ggt (g i x)]
                (if (= ggt 1)
                  (recur (inc c) (inc i) x)
                  (recur c (inc i) x)))))]
    (if (= x 1) 1 (t 0 1 x))))

(defn anagrs [ws]
  (let [p (fn [m ws]
            (if (empty? ws) m
             (let [[w & rws] ws
                   k (set (seq w))
                   v (m k)]
               (if (nil? v)
                 (recur (conj m [k #{w}]) rws)
                 (recur (conj m [k (conj v w)]) rws)))))]
    (set (filter #(> (count %) 1) (vals (p {} ws))))))
; set|vec auf Nichtsammlungen anwenden
