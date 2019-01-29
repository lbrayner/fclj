(ns fclj.core
  (:gen-class)
  ; (:use fclj.e1e77)
  )

(defn comb [f x y]
  (let [c (fn [f s x y]
            (if (empty? x) s 
              (recur f
                (into s (map (partial f (first x)) y))
                (rest x) y)))]
    (if (not (coll? x)) (recur f (list x) y)
      (if (not (coll? y)) (recur f x (list y))
       (c f #{} x y)))))

(defn comb-seqs [f x y]
  (let [c (fn [r i f x y]
            (if (>= i (count x)) r
              (let [xn (nth x i)
                    yn (nth y i)
                    yn1 (nth y (inc i))]
               (if (empty? r)
                 (recur
                   (conj (conj r (comb f xn yn)) (comb f xn yn1))
                   (inc i) f x y)
                 (let [rn (nth r i)]
                   (recur
                     (conj
                       (conj (pop r) (into rn (comb f xn yn)))
                       (comb f xn yn1))
                     (inc i) f x y))))))]
    (c [] 0 f x y)))

(defn tbp [t]
  (let [c (fn [f x y]
            (let [c (fn [f s x y]
                      (if (empty? x) s 
                        (recur f
                          (into s (map (partial f (first x)) y))
                          (rest x) y)))]
                  (if (not (coll? x)) (recur f (list x) y)
                    (if (not (coll? y)) (recur f x (list y))
                     (c f #{} x y)))))
        cs (fn [f x y]
             (let [c (fn [r i f x y]
                      (if (>= i (count x)) r
                        (let [xn (nth x i)
                              yn (nth y i)
                              yn1 (nth y (inc i))]
                         (if (empty? r)
                           (recur
                             (conj (conj r (c f xn yn)) (c f xn yn1))
                             (inc i) f x y)
                           (let [rn (nth r i)]
                             (recur
                               (conj
                                 (conj (pop r) (into rn (c f xn yn)))
                                 (c f xn yn1))
                               (inc i) f x y))))))]
                  (c [] 0 f x y)))
        tf (fn [r t]
             (if (empty? t) r
              (if (empty? r)
                (let [[ft st & rt] t]
                  (recur (cs + ft st) rt))
                (let [[ft & rt] t]
                  (recur (cs + r ft) rt)))))
        ts (tf [] t)]
    (apply min (flatten (map seq ts)))))
; Element statt der ganzen Sammlung

(defn divisors [x]
  (let [d (fn [r i x]
            (if (>= i x) r
              (if (= (rem x i) 0)
                (recur (conj r i) (inc i) x)
                (recur r (inc i) x))))]
    (d [] 1 x)))

(defn perf? [x]
  (let [divs (fn [x]
              (let [d (fn [r i x]
                        (if (>= i x) r
                          (if (= (rem x i) 0)
                            (recur (conj r i) (inc i) x)
                            (recur r (inc i) x))))]
                (d [] 1 x)))
        ds (divs x)]
    (= x (apply + ds))))

(defn intersec [x y]
  (let [i (fn [r x y]
            (if (empty? x) r
              (let [fx (first x)
                    rx (rest x)]
                (if (contains? y fx)
                  (recur (conj r fx) rx y)
                  (recur r rx y)))))]
    (i #{} x y)))

(defn chain? [x y] ; zwei Wörter
  (let [cx (count x)
        cy (count y)
        f (fn [n i x y]
            (if (empty? x) (do (println x y i) (= n (+ i (count y))))
              (if (> i n) false
               (let [[xh & xt] x
                     [yh & yt] y
                     b (= xh yh)
                     j (if b i (inc i))]
                 (do (println xh yh)
                  (if (or b (= cx cy))
                    (recur n j xt yt)
                    (recur n j xt y)))))))
        as (sort #(compare (count %2) (count %1)) [x y])]
    (apply f 1 0 as)))

(defn seq-chain? [x]
  (let [[f s] x
        r (rest x)]
    (if (nil? s) true
      (if (not (chain? f s)) false
        (recur r)))))

(defn swap [x i j]
  (if (= i j) x
   (let [v1 (subvec x 0 i)
         v2 (subvec x (inc i) j)
         v3 (subvec x (inc j))
         ith (nth x i)
         jth (nth x j)]
     (-> [] (into v1) (conj jth) (into v2) (conj ith) (into v3)))))


(defn fact [x]
  (let [f (fn [r x]
            (if (= x 1) r
              (recur (* r x) (dec x))))]
    (f 1 x)))

(defn perm-path [n x]
  (let [mp (fn anon [l r n i]
             (if (zero? n) [nil {}]
               (if (< n r)
                 (let [d (- n l)
                       qdi (quot d i)]
                   [qdi {i [qdi (rem d i)]}])
                 (let [di (dec i)
                       v (anon r (* r i) n di)
                       [q m] v
                       e (m di)]
                   (if (zero? q) v 
                     (let [d (- q l)]
                       (if (neg? d) v 
                         (let [qdi (quot d i)]
                           [qdi (conj m [i [qdi (rem d i)]])]))))))))]
    (second (mp 1 x n (dec x)))))

(defn gen-perm [m v]
  (let [ks (reverse (sort (keys m)))
        c (count v)
        f (fn [ks v]
            (if (empty? ks) v
             (let [[k & rks] ks
                   p (second (m k))
                   i (dec (- c k))
                   j (+ (inc i) p)]
               (recur rks (swap v i j)))))]
    (f ks v)))


; {1 [11 0], 2 [3 1], 3 [0 2]}

(defn permute [x]
  (let [v (vec (range 0 x))
        s (fn [x i j]
            (if (= i j) x
             (let [v1 (subvec x 0 i)
                   v2 (subvec x (inc i) j)
                   v3 (subvec x (inc j))
                   ith (nth x i)
                   jth (nth x j)]
               (-> [] (into v1) (conj jth) (into v2) (conj ith) (into v3)))))
		pp (fn [n x]
			(let [mp (fn anon [l r n i]
					   (if (zero? n) [nil {}]
						 (if (< n r)
						   (let [d (- n l)
                                 qdi (quot d i)]
							 [qdi {i [qdi (rem d i)]}])
						   (let [di (dec i)
								 v (anon r (* r i) n di)
								 [q m] v
								 e (m di)]
							 (if (zero? q) v 
							   (let [d (- q l)]
								 (if (neg? d) v 
								   (let [qdi (quot d i)]
									 [qdi (conj m [i [qdi (rem d i)]])]))))))))]
			  (second (mp 1 x n (dec x)))))
        gp (fn [m v]
            (let [ks (reverse (sort (keys m)))
                  c (count v)
                  f (fn [ks v]
                      (if (empty? ks) v
                       (let [[k & rks] ks
                             p (second (m k))
                             i (dec (- c k))
                             j (+ (inc i) p)]
                         (recur rks (s v i j)))))]
              (f ks v)))
        f (fact x)
        p (fn anon [n x]
            (if (>= n f) '()
             (let [m (pp n x)]
               (lazy-seq (cons (gp m v) (anon (inc n) x))))))]
    (p 0 x)))

(defn chainable? [x]
  (let [fact (fn [x]
              (let [f (fn [r x]
                        (if (= x 1) r
                          (recur (* r x) (dec x))))]
                (f 1 x)))
        perm (fn [x]
			  (let [v (vec (range 0 x))
					s (fn [x i j]
						(if (= i j) x
						 (let [v1 (subvec x 0 i)
							   v2 (subvec x (inc i) j)
							   v3 (subvec x (inc j))
							   ith (nth x i)
							   jth (nth x j)]
						   (-> [] (into v1) (conj jth) (into v2) (conj ith) (into v3)))))
					pp (fn [n x]
						(let [mp (fn anon [l r n i]
								   (if (zero? n) [nil {}]
									 (if (< n r)
									   (let [d (- n l)
											 qdi (quot d i)]
										 [qdi {i [qdi (rem d i)]}])
									   (let [di (dec i)
											 v (anon r (* r i) n di)
											 [q m] v
											 e (m di)]
										 (if (zero? q) v 
										   (let [d (- q l)]
											 (if (neg? d) v 
											   (let [qdi (quot d i)]
												 [qdi (conj m [i [qdi (rem d i)]])]))))))))]
						  (second (mp 1 x n (dec x)))))
					gp (fn [m v]
						(let [ks (reverse (sort (keys m)))
							  c (count v)
							  f (fn [ks v]
								  (if (empty? ks) v
								   (let [[k & rks] ks
										 p (second (m k))
										 i (dec (- c k))
										 j (+ (inc i) p)]
									 (recur rks (s v i j)))))]
						  (f ks v)))
					f (fact x)
					p (fn anon [n x]
						(if (>= n f) '()
						 (let [m (pp n x)]
						   (lazy-seq (cons (gp m v) (anon (inc n) x))))))]
				(p 0 x)))
        c? (fn [x y] ; zwei Wörter
            (let [cx (count x)
                  cy (count y)
                  f (fn [n i x y]
                      (if (empty? x) (= n (+ i (count y)))
                        (if (> i n) false
                         (let [[xh & xt] x
                               [yh & yt] y
                               b (= xh yh)
                               j (if b i (inc i))]
                           (if (or b (= cx cy))
                             (recur n j xt yt)
                             (recur n j xt y))))))
                  as (sort #(compare (count %2) (count %1)) [x y])]
              (apply f 1 0 as)))
        wp (fn anon [p x]
            (if (empty? p) '()
             (let [[h & t] p
                   wp (map (partial nth x) h)]
               (lazy-seq (cons wp (anon t x))))))
        sc? (fn [x]
              (let [[f s] x
                    r (rest x)]
                (if (nil? s) true
                  (if (not (c? f s)) false
                    (recur r)))))
        f (fn [wps]
            (if (empty? wps) false
             (let [[h & t] wps]
               (if (sc? h) (do (println h) true)
                 (recur t)))))
        y (seq x)
        p (perm (count x))
        wps (wp p y)]
    (f wps)))
; die Abbruchbedingung vergessen

(defn ht [& x]
  (let [f #(and %1 %2)
        g #(or %1 %2)
        a (reduce f x)
        o (reduce g x)]
    (and (not a) o)))

(defn multivalue-map [l]
  (let [f (fn [m l]
            (if (empty? l) m
             (let [[h & t] l
                   k (first h)]
               (if (contains? m k)
                 (let [vs (m k)
                       nm (dissoc m k)]
                   (recur (conj nm [k (conj vs h)]) t))
                 (recur (conj m [k [h]]) t)))))]
    (f {} l)))


(defn transclose [x]
  (let [f (fn [i y] ; find candidates
            (let [fc (fn [v i y]
                       (if (empty? y) v
                        (let [[a b] i
                              [j & r] y
                              [c d] j]
                          (if (= b c)
                            (recur (conj v j) i r)
                            (recur v i r)))))]
              (fc [] i y)))
        h (fn [ts v]  ; find head of relation
            (let [ms (set v)
                  s (apply disj ms ts)
                  m (into {} s)
                  ks (set (keys m))
                  vs (set (vals m))
                  ket (apply disj ks ts)
                  fh (fn [ks vs]
                       (if (empty? ks) nil
                        (let [kh (first ks)
                              kt (rest ks)]
                          (if (not (contains? vs kh)) [kh (m kh)]
                            (recur kt vs)))))]
              (fh ket vs)))
        c (fn [th cs] ; close over transitive relation $1:transitive_head $2:candidates
            (let [cl (fn [v th cs]
                       (if (empty? cs) v
                        (let [[a b] th
                              [[c d] & rcs] cs]
                          (recur (conj v [a d]) th rcs))))]
              (cl [] th cs)))
        r #(and % %2)
        t (fn [ts x]
            (let [m (into {} x)
                  th (h ts m)
				  l (seq x)
				  s (set l)
				  cs (f th l)
				  cl (c th cs)]
              (if (nil? th) x
                (let [l (seq x)
                      s (set l)
                      cs (f th l)
                      cl (c th cs)
                      k (first th)
                      b (or (empty? cl)
                            (reduce r (map (partial contains? s) cl)))]
                  (if b
                    (recur (conj ts k) x)
                    (recur ts (into x cl)))))))]
    (t #{} x)))


; (ns-unmap 'fclj.core 'mmc)
; (require 'eastwood.lint)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [m (range 1 6) 
        n [1 2 :a 3 :b]
        o {:a 1 :b 2}
        p #{0}
        m1 {:a 2, :b 3, :c 4}
        m2 {:a 3, :b 6, :c 7}
        m3 {:a 4, :b 3, :c 5}
        ws "Have a nice day."
        ttt [[:x :e :e]
             [:o :x :e]
             [:o :e :x]]
        tri '([3]
             [2 4]
            [1 9 3]
           [9 9 2 4]
          [4 6 6 7 8]
         [5 7 3 5 1 4])
        [a b c d] tri
        cs (comb-seqs + b c)
        ws1 #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}
        ws2 #{"cot" "hot" "bat" "fat"}
        ws3 '("cat" "oat" "cot" "coat" "hot" "hat" "hog" "dog")
        ws4 #{"to" "top" "stop" "tops" "toss"}
        ws5 #{"share" "hares" "shares" "hare" "are"}
        rel1 #{[8 4] [9 3] [4 2] [27 9]}
        ]
    ; (take 10 (reduce-steps + (range)))
    ; (take 5 (iter (partial * 2) 5))
    ; (tt 0 vs)
    ; (diag ttt)
    ; (println "Hello, world!")
    ; (comb-seqs + cs d)
    ; (tbp tri)
    ; (seq ws)
    ; (fact (count ws))
    ; (chainable? ws1)
    ; (chainable? ws2)
    ; (chainable? ws1)
    ; (seq-chain? ws2)
    ; (seq-chain? ws3)
    (transclose rel1)
    )
  )
