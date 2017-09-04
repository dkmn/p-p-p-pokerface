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

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
