(ns p-p-p-pokerface)

(defn face-card-value [face-rank]
  "Auxilliary function to map single-digit face card codes to integer rank values"
  (get { \T 10, \J 11, \Q 12, \K 13, \A 14} face-rank)
  )

(defn rank [card]
  "Destructure hand into a first character, the rank, and an ignored second character, the suit"
  (let [ [fst _] card ]
    (cond (Character/isDigit fst) (Integer/valueOf (str fst))
          :else (face-card-value fst))))

(defn suit [card]
  "Destructure hand into an ignored first character and a second character, the suit"
  (let [ [_ snd] card ]
  (str snd)))


(defn contains-n-ofakind? [hand n]
  "Extrapolated from the initial work on pair? below"
  (= n (apply max (vals (frequencies (map rank hand))))))


(defn pair? [hand]
  "From interactive verification:
  (map rank pair-hand) => (2 2 4 5 7)
  (frequencies (map rank pair-hand)) => {2 2, 4 1, 5 1, 7 1}
  (vals (frequencies (map rank pair-hand))) => (2 1 1 1)
  (max (vals (frequencies (map rank pair-hand)))) => (2 1 1 1)
  (apply max (vals (frequencies (map rank pair-hand)))) => 2"
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (contains-n-ofakind? hand 3))

(defn four-of-a-kind? [hand]
  (contains-n-ofakind? hand 4))

(defn flush? [hand]
  "Set of suits for the hand should have one member if all are the same"
  (= 1 (count (set (map suit hand)))))

(defn sorted-hand-rank-frequencies [hand]
  "Create a standardized frequency signature for each hand, E.g.
   (map rank full-house-hand)
    => (2 5 2 2 5)
   (frequencies (map rank full-house-hand))
    => {2 3, 5 2}
   (second (frequencies (map rank full-house-hand)))
    => [5 2]
   (vals (frequencies (map rank full-house-hand)))
    => (3 2)
   (sort (vals (frequencies (map rank full-house-hand))))
    => (2 3)"
  (sort (vals (frequencies (map rank hand)))))

(defn full-house? [hand]
  (= [2 3] (sorted-hand-rank-frequencies hand)))

(defn two-pairs? [hand]
  (= [1 2 2] (sorted-hand-rank-frequencies hand)))

(defn sorted-hand-ranks [hand]
  (sort (map rank hand)))

(defn ranks-sequential-run? [ranks]
  "Expected input of vector of ranks, sorted, e.g. from sorted-hand-ranks"
  (= ranks
     (range (apply min ranks) (+ (apply max ranks) 1) )))

(defn ranks-aces-high [hand]
  (sorted-hand-ranks hand))

(defn ranks-aces-low [hand]
  (sort (replace {14 1} (sorted-hand-ranks hand))))

(defn straight? [hand]
  (or (ranks-sequential-run? (ranks-aces-high hand))
      (ranks-sequential-run? (ranks-aces-low hand))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        checker-applied-to-hand (fn [p] ((first p) hand))
        possible-checker-value-pairs (filter checker-applied-to-hand checkers)
        possible-values (map second possible-checker-value-pairs)]

    (apply max possible-values)
    ))
