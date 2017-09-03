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

(defn pair? [hand]
  nil)

(defn three-of-a-kind? [hand]
  nil)

(defn four-of-a-kind? [hand]
  nil)

(defn flush? [hand]
  nil)

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
