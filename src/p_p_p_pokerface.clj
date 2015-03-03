(ns p-p-p-pokerface)

(defn rank [card]
  (let [highs {\T 10, \J 11, \Q 12, \K 13, \A 14}
        value (get card 0)]
    (or (get highs value)
        (Integer/valueOf (str value)))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (or (some (fn [x] (= x 2))
            (vals (frequencies (map rank hand))))
      false))

(defn three-of-a-kind? [hand]
  (or (some (fn [x] (= x 3))
            (vals (frequencies (map rank hand))))
      false))

(defn four-of-a-kind? [hand]
  (or (some (fn [x] (= x 4))
            (vals (frequencies (map rank hand))))
      false))

(defn flush? [hand]
  (= 1 (count (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (and (= 3 (count counts))
         (= 2 (apply max counts)))))

(defn straight? [hand]
  (let [[a b c d e] (sort (map rank hand))]
    (or (= [a b c d e]
           (range a (+ a 5)))
        (and (= e 14)
             (= [1 a b c d]
                [1 2 3 4 5])))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [[check val]] (if (check hand) val 0))
                    checkers))))
