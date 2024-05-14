(ns remi)

(defrecord Card [rank suit type])
(defrecord Rank [value number])

(def ranks
    (range 2 15))

(def suits #{:clubs :diamonds :hearts :spades})

(def joker (->Card nil nil :joker))

(defn create-deck
  [number-of-players]
  (if (> number-of-players 3)
    (let [standard-cards (for [r ranks, s suits]
                           (->Card r s :standard))
          jokers [joker joker]]
      (let  [deck (concat standard-cards jokers) ] (concat deck deck)  ) )
    (let [standard-cards (for [r ranks, s suits]
                           (->Card r s :standard))
          jokers [joker joker]]
      (concat standard-cards jokers))
    )
  )

(def deck (atom (shuffle (create-deck 4))) )
(def middle-pile (atom [] ))

(defn deal-hand [num-cards]
  (let [hand (take num-cards @deck)]
    (swap! deck #(drop num-cards %))
    hand))

(defrecord Player [name hand])

(defn make-players
  [number-of-players]
  (let [players (vec (for [n (range number-of-players)]
                       (let [player-name (str "player" (inc n))
                             player-hand (atom (deal-hand 14))] ; deal-hand presumably returns a collection of cards
                         (->Player player-name player-hand))))]
    players))

(def players (make-players 4))

(defn get-player
  [players number]
  (nth players (dec number)))

(defn get-player-hand
  [players number]
  (let [player (nth players (dec number))
        player-hand (:hand player)]
    @player-hand))

(defn draw-from-middle
  [player]
  (swap! (:hand player) concat (deal-hand 1)))

(draw-from-middle (get-player players 4))

(get-player players 4)
(:hand (get-player players 4))

(defn drop-card
  [player card-position]
  (let [player-hand @(:hand player)
        card (nth player-hand card-position)]
    (swap! (:hand player) #(concat (take card-position %) (drop (inc card-position) %)))
    (swap! middle-pile conj card)))

(draw-from-middle (get-player players 4))
(drop-card (get-player players 4) 12)

(count @middle-pile)


(defn all-same-suit?
  [cards]
  (let [sorted-cards (sort-by :type cards)]
    (let [last-suit (:suit (last sorted-cards))]
      (every? #(or (= last-suit (:suit %)) (= :joker (:type %))) sorted-cards))))

(defn same-cards
  [cards]
  (let [sorted-cards (sort-by :type cards)]
    (let [last-value  (:rank (last sorted-cards))]
      (if
        (every? #(or (= (:rank %) last-value) (= :joker (:type %)))  sorted-cards)
        (* (count sorted-cards) last-value)
        0))))

(defn all-different-suits?
  [cards]
  (let [suits (map :suit cards) ; Extract the suit from each card
        unique-suits (set suits)] ; Convert list of suits to a set to remove duplicates
    (= (count suits) (count unique-suits)))) ; Compare counts of original suits and unique suits

(defn same-cards-different-suits
  [cards]
  (let [value (same-cards cards)]
    (if (and (all-different-suits? cards) (> value 0))
      value
      0
      )))

(defn update-first-card-if-ace
  [group]
  (if (= (:rank (first group)) 14)
    (let [updated-card (assoc-in (first group) [:rank] 1)
          updated-group (cons updated-card (rest group))]
      updated-group)
    group))

(defn find-first-standard
  [cards]
  (some #(if (= (:type %) :standard) % nil) cards))

(defn index-of
  [coll element]
  (first (keep-indexed (fn [idx el]
                         (when (= el element)
                           idx))
                       coll)))

(defn sum-of-sequential-cards
  [group]
  (loop [sum 0 first-card (first group) remaining (rest group)]
    (if (empty? remaining)
      (+ sum (:rank first-card))
      (if
        (= (:type first-card) :joker)
        (recur
          (let [first-standard (find-first-standard remaining)]
            (+ sum (- (:rank first-standard) (+ 1 (index-of remaining first-standard)))))
          (first remaining)
          (rest remaining))

        (if (= (+ (:rank first-card)  1) (:rank (first remaining)))
          (recur (+ sum (:rank first-card))  (first remaining) (rest remaining))
          (if (= :joker (:type (first remaining)))
            (recur (+ sum (:rank first-card)) {:rank (+ (:rank first-card)  1), :suit (:suit first-card), :type :standard} (rest remaining))
            0))))))

(defn in-a-row-same-suit
  [group]
  (if (all-same-suit? group)
    (sum-of-sequential-cards (update-first-card-if-ace group) )
    0))

(defn check-pack
  [hand group-numbers]
  (let [group (map #(nth hand %) group-numbers) value-in-a-row (in-a-row-same-suit group)]
    (if
      (> value-in-a-row 0)
      value-in-a-row
      (same-cards-different-suits group))))

(defn sort-hand [hand]
  (let [suit-order {:clubs 0, :diamonds 1, :hearts 2, :spades 3}]
    (sort-by (fn [card]
               [(suit-order (:suit card)) (:rank card)])
             hand)))



(defn count-same-rank-different-suits
  [hand reference-card]
  (let [reference-rank (:rank reference-card)
        reference-suit (:suit reference-card)]
    (->> hand
         (filter #(and (= (:rank %) reference-rank)
                       (not= (:suit %) reference-suit)))
         count)))

(defn find-same-rank-different-suits
  [hand reference-card]
  (let [reference-rank-number (:rank reference-card)
        reference-suit (:suit reference-card)]
    (->> hand
         (filter #(and (= reference-rank-number (:rank %))
                       (not= reference-suit (:suit %))))
         (into #{})))) ; Converting the set to a sequence

(defn find-higher-cards
  [sorted-cards reference-card]
  (let [ref-number (:rank reference-card)]
    (drop-while #(<= (:rank %) ref-number) sorted-cards)))

(defn find-lower-cards
  [sorted-cards reference-card]
  (let [ref-number (:rank reference-card)
        sorted-cards-with-ace (sort-hand (map #(if (= 14 (:rank %))
                                                 (assoc % :rank 1)
                                                 %)
                                              sorted-cards))]
    (drop-while #(>= (:rank %) ref-number) (reverse sorted-cards-with-ace))))

(defn build-asc-sequence
  [higher-cards reference-card]
  (loop [reference-list [reference-card] reference reference-card remaining-cards higher-cards]
    (if
      (= (+ 1 (:rank reference)) (:rank (first remaining-cards)))
      (recur (conj reference-list (first remaining-cards)) (first remaining-cards) (rest remaining-cards))
      reference-list)))

(defn build-desc-sequence
  [lower-cards reference-card]
  (loop [reference-list [reference-card]  ; Initialize with the reference card
         reference reference-card        ; Start with the reference card
         remaining-cards lower-cards]    ; Consider only the cards that are potentially lower
    (if (and (seq remaining-cards)  ; Check if there are any remaining cards
             (= (- (:rank reference) 1)
                (:rank (first remaining-cards))))  ; Compare the rank number to find a descending match
      (recur (conj reference-list (first remaining-cards))  ; Add matching card to the list
             (first remaining-cards)  ; Update the reference card to the new match
             (rest remaining-cards))  ; Remove the used card from the remaining list
      reference-list)))




(defn find-sequence-cards
  [hand reference-card]
  (let [ref-suit (:suit reference-card)
        ref-number (:rank reference-card)
        all-same-suit (filter #(= (:suit %) ref-suit) hand)
        sorted-cards (sort-by #(:rank %) all-same-suit)
        higher-cards (find-higher-cards sorted-cards reference-card)
        lower-cards (find-lower-cards sorted-cards reference-card)]
    (if (= 14 ref-number)
      (let [asc-seq (build-asc-sequence sorted-cards (assoc reference-card :rank 1))
            desc-seq (build-desc-sequence (reverse sorted-cards) reference-card)]
        (if (> (count desc-seq) (count asc-seq)) desc-seq asc-seq)
        )
      (concat
        (reverse (rest (build-desc-sequence lower-cards reference-card)))
        (build-asc-sequence higher-cards reference-card)))))


(defn find-best-pack
  [hand card]
  (let [sorted-hand (sort-hand hand)
        same-rank-pack (cons card (find-same-rank-different-suits hand card))
        sequence-pack (find-sequence-cards hand card)]
    (if (> (count sequence-pack) (count same-rank-pack)) sequence-pack same-rank-pack)))

(defn check-bottom-of-sequence-for-potential-card
  [sequence]
  (let [potential-number (- (:rank (first sequence)) 1)]
    (if
      (not= 0 potential-number)
      (->Card potential-number (:suit (first sequence)) :standard))))

(defn check-top-of-sequence-for-potential-card
  [sequence]
  (let [potential-number (+ (:rank (last sequence)) 1)]
    (if
      (not= 14 potential-number)
      (->Card potential-number (:suit (first sequence)) :standard))))

(def all-suits #{:clubs :diamonds :hearts :spades})

(defn check-same-rank-pack-for-potential-card
  [same-rank-pack]
  (let [present-suits (->> same-rank-pack
                           (map :suit)
                           set)]
    (map #(->Card (:rank (first same-rank-pack)) % :standard) (clojure.set/difference all-suits present-suits))
    ))

(defn find-potential-useful-cards
  [hand card]
  (let [length-best-pack (count (find-best-pack hand card))
        longest-sequence (find-sequence-cards hand card)
        result []]
    (if (= length-best-pack 4)
      (conj
        (conj
          result
          (check-bottom-of-sequence-for-potential-card longest-sequence))
        (check-top-of-sequence-for-potential-card longest-sequence))
      (let
        [same-rank-pack (cons card (find-same-rank-different-suits hand card))]
        (vec (concat
               (conj
                 (conj
                   result
                   (check-bottom-of-sequence-for-potential-card longest-sequence))
                 (check-top-of-sequence-for-potential-card longest-sequence))
               (check-same-rank-pack-for-potential-card same-rank-pack)))))))

(conj [] 1)
(concat [1] [2])

(defn ordered-vector-intersection
  [vec1 vec2]
  (let [set2 (set vec2)]
    (vec (filter #(set2 %) vec1))))



(defn find-useful-cards
  [hand card remaining-cards]
  (let [potential-useful-cards (find-potential-useful-cards hand card)]
    (ordered-vector-intersection potential-useful-cards remaining-cards)))

(defn vector-difference
  [vec1 vec2]
  (let [to-remove (frequencies vec2)  ; Creates a map of elements to their counts in vec2
        result (reduce (fn [[res counts] item]  ; Destructure the accumulator into res and counts
                         (if (and (> (get counts item 0) 0)  ; If item should be removed
                                  (contains? counts item))
                           [res (update counts item dec)]  ; Decrement count in map, keep result the same
                           [(conj res item) counts]))  ; Else, add item to result
                       [[] to-remove]  ; Initial accumulator: empty result vector and to-remove map
                       vec1)]
    (first result)))


(defn count-card-occurrences
  [cards card-to-count]
  (count (filter #(= % card-to-count) cards)))

(defn find-best-pack-for-each-potential-card
  [hand card middle-pile]
  (let [remaining-cards (vector-difference (vector-difference (create-deck 4) middle-pile) hand)
        useful-cards (find-useful-cards hand card remaining-cards)]
    (map *
         (vec (map #(count (find-best-pack (conj hand %) card)) useful-cards))
         (vec (map #(* (count-card-occurrences remaining-cards %) (/ 1 (count remaining-cards))) useful-cards)))))

(map *
     (vec (map #(count (find-best-pack (conj [#remi.Card{:rank 7, :suit :diamonds, :type :standard}
                                                #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                                #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                                #remi.Card{:rank 10, :suit :spades, :type :standard}
                                                #remi.Card{:rank 8, :suit :spades, :type :standard}
                                                #remi.Card{:rank 7, :suit :spades, :type :standard}
                                                #remi.Card{:rank 2, :suit :diamonds, :type :standard}
                                                #remi.Card{:rank 14, :suit :hearts, :type :standard}
                                                #remi.Card{:rank 4, :suit :spades, :type :standard}
                                                #remi.Card{:rank 10, :suit :diamonds, :type :standard}
                                                #remi.Card{:rank 5, :suit :spades, :type :standard}
                                                #remi.Card{:rank 14, :suit :clubs, :type :standard}] %)
                                         #remi.Card{:rank 10, :suit :spades, :type :standard}))
                 [#remi.Card{:rank 9, :suit :spades, :type :standard}
                  #remi.Card{:rank 11, :suit :spades, :type :standard}
                  #remi.Card{:rank 10, :suit :clubs, :type :standard}]))
     (vec (map #(* (count-card-occurrences [#remi.Card{:rank 7, :suit :diamonds, :type :standard}
                                            #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                            #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                            #remi.Card{:rank 10, :suit :spades, :type :standard}
                                            #remi.Card{:rank 8, :suit :spades, :type :standard}
                                            #remi.Card{:rank 7, :suit :spades, :type :standard}
                                            #remi.Card{:rank 2, :suit :diamonds, :type :standard}
                                            #remi.Card{:rank 4, :suit :spades, :type :standard}
                                            #remi.Card{:rank 4, :suit :spades, :type :standard}
                                            #remi.Card{:rank 10, :suit :diamonds, :type :standard}
                                            #remi.Card{:rank 5, :suit :spades, :type :standard}
                                            #remi.Card{:rank 14, :suit :clubs, :type :standard}]
                                           %)
                   (/ 1 (count [#remi.Card{:rank 7, :suit :diamonds, :type :standard}
                                #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                #remi.Card{:rank 10, :suit :spades, :type :standard}
                                #remi.Card{:rank 8, :suit :spades, :type :standard}
                                #remi.Card{:rank 7, :suit :spades, :type :standard}
                                #remi.Card{:rank 2, :suit :diamonds, :type :standard}
                                #remi.Card{:rank 14, :suit :hearts, :type :standard}
                                #remi.Card{:rank 4, :suit :spades, :type :standard}
                                #remi.Card{:rank 10, :suit :diamonds, :type :standard}
                                #remi.Card{:rank 5, :suit :spades, :type :standard}
                                #remi.Card{:rank 14, :suit :clubs, :type :standard}]))) [#remi.Card{:rank 7, :suit :diamonds, :type :standard}
                                                                                         #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                                                                         #remi.Card{:rank 7, :suit :diamonds, :type :standard}]))
     )


(map * (1 2 3) (1 2 3))

(vector-difference (vector-difference (create-deck 4) []) [#remi.Card{:rank 7, :suit :diamonds, :type :standard}
                                                           #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                                           #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                                           #remi.Card{:rank 10, :suit :spades, :type :standard}
                                                           #remi.Card{:rank 8, :suit :spades, :type :standard}
                                                           #remi.Card{:rank 7, :suit :spades, :type :standard}
                                                           #remi.Card{:rank 2, :suit :diamonds, :type :standard}
                                                           #remi.Card{:rank 14, :suit :hearts, :type :standard}
                                                           #remi.Card{:rank 4, :suit :spades, :type :standard}
                                                           #remi.Card{:rank 10, :suit :diamonds, :type :standard}
                                                           #remi.Card{:rank 5, :suit :spades, :type :standard}
                                                           #remi.Card{:rank 14, :suit :clubs, :type :standard}])

(ordered-vector-intersection
  [#remi.Card{:rank 9, :suit :spades, :type :standard}
   #remi.Card{:rank 11, :suit :spades, :type :standard}
   #remi.Card{:rank 10, :suit :clubs, :type :standard}]
  (vector-difference (vector-difference (create-deck 4) []) [#remi.Card{:rank 7, :suit :diamonds, :type :standard}
                                                             #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                                             #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                                             #remi.Card{:rank 10, :suit :spades, :type :standard}
                                                             #remi.Card{:rank 8, :suit :spades, :type :standard}
                                                             #remi.Card{:rank 7, :suit :spades, :type :standard}
                                                             #remi.Card{:rank 2, :suit :diamonds, :type :standard}
                                                             #remi.Card{:rank 14, :suit :hearts, :type :standard}
                                                             #remi.Card{:rank 4, :suit :spades, :type :standard}
                                                             #remi.Card{:rank 10, :suit :diamonds, :type :standard}
                                                             #remi.Card{:rank 5, :suit :spades, :type :standard}
                                                             #remi.Card{:rank 14, :suit :clubs, :type :standard}]))
//Remaining cards
(vector-difference (vector-difference (create-deck 4) []) [#remi.Card{:rank 7, :suit :diamonds, :type :standard}
                                                           #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                                           #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                                           #remi.Card{:rank 10, :suit :spades, :type :standard}
                                                           #remi.Card{:rank 8, :suit :spades, :type :standard}
                                                           #remi.Card{:rank 7, :suit :spades, :type :standard}
                                                           #remi.Card{:rank 2, :suit :diamonds, :type :standard}
                                                           #remi.Card{:rank 14, :suit :hearts, :type :standard}
                                                           #remi.Card{:rank 4, :suit :spades, :type :standard}
                                                           #remi.Card{:rank 10, :suit :diamonds, :type :standard}
                                                           #remi.Card{:rank 5, :suit :spades, :type :standard}
                                                           #remi.Card{:rank 14, :suit :clubs, :type :standard}])

//useful-cards
(find-useful-cards [#remi.Card{:rank 7, :suit :diamonds, :type :standard}
                    #remi.Card{:rank 10, :suit :hearts, :type :standard}
                    #remi.Card{:rank 10, :suit :hearts, :type :standard}
                    #remi.Card{:rank 10, :suit :spades, :type :standard}
                    #remi.Card{:rank 8, :suit :spades, :type :standard}
                    #remi.Card{:rank 7, :suit :spades, :type :standard}
                    #remi.Card{:rank 2, :suit :diamonds, :type :standard}
                    #remi.Card{:rank 14, :suit :hearts, :type :standard}
                    #remi.Card{:rank 4, :suit :spades, :type :standard}
                    #remi.Card{:rank 10, :suit :diamonds, :type :standard}
                    #remi.Card{:rank 5, :suit :spades, :type :standard}
                    #remi.Card{:rank 14, :suit :clubs, :type :standard}]
                   #remi.Card{:rank 10, :suit :spades, :type :standard}
                   (vector-difference (vector-difference (create-deck 4) []) [#remi.Card{:rank 7, :suit :diamonds, :type :standard}
                                                                              #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                                                              #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                                                              #remi.Card{:rank 10, :suit :spades, :type :standard}
                                                                              #remi.Card{:rank 8, :suit :spades, :type :standard}
                                                                              #remi.Card{:rank 7, :suit :spades, :type :standard}
                                                                              #remi.Card{:rank 2, :suit :diamonds, :type :standard}
                                                                              #remi.Card{:rank 14, :suit :hearts, :type :standard}
                                                                              #remi.Card{:rank 4, :suit :spades, :type :standard}
                                                                              #remi.Card{:rank 10, :suit :diamonds, :type :standard}
                                                                              #remi.Card{:rank 5, :suit :spades, :type :standard}
                                                                              #remi.Card{:rank 14, :suit :clubs, :type :standard}]))

(ordered-vector-intersection
  (find-potential-useful-cards [#remi.Card{:rank 7, :suit :diamonds, :type :standard}
                                #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                #remi.Card{:rank 10, :suit :spades, :type :standard}
                                #remi.Card{:rank 8, :suit :spades, :type :standard}
                                #remi.Card{:rank 7, :suit :spades, :type :standard}
                                #remi.Card{:rank 2, :suit :diamonds, :type :standard}
                                #remi.Card{:rank 14, :suit :hearts, :type :standard}
                                #remi.Card{:rank 4, :suit :spades, :type :standard}
                                #remi.Card{:rank 10, :suit :diamonds, :type :standard}
                                #remi.Card{:rank 5, :suit :spades, :type :standard}
                                #remi.Card{:rank 14, :suit :clubs, :type :standard}]
                               #remi.Card{:rank 10, :suit :spades, :type :standard})
  (vector-difference (vector-difference (create-deck 4) []) [#remi.Card{:rank 7, :suit :diamonds, :type :standard}
                                                             #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                                             #remi.Card{:rank 10, :suit :hearts, :type :standard}
                                                             #remi.Card{:rank 10, :suit :spades, :type :standard}
                                                             #remi.Card{:rank 8, :suit :spades, :type :standard}
                                                             #remi.Card{:rank 7, :suit :spades, :type :standard}
                                                             #remi.Card{:rank 2, :suit :diamonds, :type :standard}
                                                             #remi.Card{:rank 14, :suit :hearts, :type :standard}
                                                             #remi.Card{:rank 4, :suit :spades, :type :standard}
                                                             #remi.Card{:rank 10, :suit :diamonds, :type :standard}
                                                             #remi.Card{:rank 5, :suit :spades, :type :standard}
                                                             #remi.Card{:rank 14, :suit :clubs, :type :standard}]))




(- (:number (:rank (first [#remi.Card{:rank #remi.Rank{:value 9, :number 2}, :suit :spades, :type :standard}
                           #remi.Card{:rank #remi.Rank{:value 9, :number 3}, :suit :spades, :type :standard}
                           #remi.Card{:rank #remi.Rank{:value 9, :number 4}, :suit :spades, :type :standard}]))) 1)

(check-same-rank-pack-for-potential-card [#remi.Card{:rank 5, :suit :diamonds, :type :standard}
                                          #remi.Card{:rank 5, :suit :spades, :type :standard}
                                          #remi.Card{:rank 5, :suit :hearts, :type :standard}])

(find-useful-cards [#remi.Card{:rank 7, :suit :diamonds, :type :standard}
                              #remi.Card{:rank 10, :suit :hearts, :type :standard}
                              #remi.Card{:rank 10, :suit :hearts, :type :standard}
                              #remi.Card{:rank 10, :suit :spades, :type :standard}
                              #remi.Card{:rank 8, :suit :spades, :type :standard}
                              #remi.Card{:rank 7, :suit :spades, :type :standard}
                              #remi.Card{:rank 2, :suit :diamonds, :type :standard}
                              #remi.Card{:rank 14, :suit :hearts, :type :standard}
                              #remi.Card{:rank 4, :suit :spades, :type :standard}
                              #remi.Card{:rank 10, :suit :diamonds, :type :standard}
                              #remi.Card{:rank 5, :suit :spades, :type :standard}
                              #remi.Card{:rank 14, :suit :clubs, :type :standard}]
                             #remi.Card{:rank 10, :suit :spades, :type :standard}
                              [])




(find-best-pack-for-each-potential-card [#remi.Card{:rank 7, :suit :diamonds, :type :standard}
                              #remi.Card{:rank 10, :suit :hearts, :type :standard}
                              #remi.Card{:rank 10, :suit :hearts, :type :standard}
                              #remi.Card{:rank 10, :suit :spades, :type :standard}
                              #remi.Card{:rank 8, :suit :spades, :type :standard}
                              #remi.Card{:rank 7, :suit :spades, :type :standard}
                              #remi.Card{:rank 2, :suit :diamonds, :type :standard}
                              #remi.Card{:rank 14, :suit :hearts, :type :standard}
                              #remi.Card{:rank 4, :suit :spades, :type :standard}
                              #remi.Card{:rank 10, :suit :diamonds, :type :standard}
                              #remi.Card{:rank 5, :suit :spades, :type :standard}
                              #remi.Card{:rank 14, :suit :clubs, :type :standard}]
                             #remi.Card{:rank 10, :suit :spades, :type :standard}
                                        [#remi.Card{:rank 10, :suit :diamonds, :type :standard}])


(count-card-occurrences [#remi.Card{:rank #remi.Rank{:value 9, :number 2}, :suit :diamonds, :type :standard}
                   #remi.Card{:rank #remi.Rank{:value 9, :number 3}, :suit :diamonds, :type :standard}
                   #remi.Card{:rank #remi.Rank{:value 9, :number 2}, :suit :diamonds, :type :standard}
                   #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :diamonds, :type :standard}]
                  #remi.Card{:rank #remi.Rank{:value 9, :number 2}, :suit :diamonds, :type :standard})

(build-desc-sequence [#remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :diamonds, :type :standard}
                      #remi.Card{:rank #remi.Rank{:value 9, :number 1}, :suit :diamonds, :type :standard}
                      #remi.Card{:rank #remi.Rank{:value 9, :number 9}, :suit :diamonds, :type :standard}
                      #remi.Card{:rank #remi.Rank{:value 9, :number 5}, :suit :diamonds, :type :standard}]
                     #remi.Card{:rank #remi.Rank{:value 9, :number 15}, :suit :diamonds, :type :standard})

(sort-by #(:number (:rank %)) (filter #(= (:suit %) :spades) [#remi.Card{:rank #remi.Rank{:value 9, :number 7}, :suit :diamonds, :type :standard}
                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 10}, :suit :hearts, :type :standard}
                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 10}, :suit :hearts, :type :standard}
                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :spades, :type :standard}
                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 2}, :suit :spades, :type :standard}
                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :spades, :type :standard}
                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 2}, :suit :diamonds, :type :standard}
                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :diamonds, :type :standard}
                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :spades, :type :standard}
                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 10}, :suit :diamonds, :type :standard}
                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :hearts, :type :standard}
                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 5}, :suit :clubs, :type :standard}]
                                      ))


(build-asc-sequence (sort-by #(:number (:rank %)) (filter #(= (:suit %) :spades) [#remi.Card{:rank #remi.Rank{:value 9, :number 7}, :suit :diamonds, :type :standard}
                                                                                  #remi.Card{:rank #remi.Rank{:value 9, :number 10}, :suit :hearts, :type :standard}
                                                                                  #remi.Card{:rank #remi.Rank{:value 9, :number 10}, :suit :hearts, :type :standard}
                                                                                  #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :spades, :type :standard}
                                                                                  #remi.Card{:rank #remi.Rank{:value 9, :number 2}, :suit :spades, :type :standard}
                                                                                  #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :spades, :type :standard}
                                                                                  #remi.Card{:rank #remi.Rank{:value 9, :number 2}, :suit :diamonds, :type :standard}
                                                                                  #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :diamonds, :type :standard}
                                                                                  #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :spades, :type :standard}
                                                                                  #remi.Card{:rank #remi.Rank{:value 9, :number 10}, :suit :diamonds, :type :standard}
                                                                                  #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :hearts, :type :standard}
                                                                                  #remi.Card{:rank #remi.Rank{:value 9, :number 5}, :suit :clubs, :type :standard}]
                                                          )) (assoc #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :spades, :type :standard} :rank (assoc (:rank #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :spades, :type :standard}) :number 1)))



(count
  (cons #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :diamonds, :type :standard} (find-same-rank-different-suits [#remi.Card{:rank #remi.Rank{:value 9, :number 7}, :suit :diamonds, :type :standard}
                                                                                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 10}, :suit :hearts, :type :standard}
                                                                                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 10}, :suit :hearts, :type :standard}
                                                                                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :spades, :type :standard}
                                                                                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 2}, :suit :spades, :type :standard}
                                                                                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :spades, :type :standard}
                                                                                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 2}, :suit :diamonds, :type :standard}
                                                                                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :diamonds, :type :standard}
                                                                                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :spades, :type :standard}
                                                                                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 10}, :suit :diamonds, :type :standard}
                                                                                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :hearts, :type :standard}
                                                                                                                              #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :clubs, :type :standard}]
                                                                                                                             #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :diamonds, :type :standard}))

  )

(count (find-sequence-cards [#remi.Card{:rank #remi.Rank{:value 9, :number 7}, :suit :diamonds, :type :standard}
                             #remi.Card{:rank #remi.Rank{:value 9, :number 10}, :suit :hearts, :type :standard}
                             #remi.Card{:rank #remi.Rank{:value 9, :number 10}, :suit :hearts, :type :standard}
                             #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :spades, :type :standard}
                             #remi.Card{:rank #remi.Rank{:value 9, :number 2}, :suit :spades, :type :standard}
                             #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :spades, :type :standard}
                             #remi.Card{:rank #remi.Rank{:value 9, :number 2}, :suit :diamonds, :type :standard}
                             #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :diamonds, :type :standard}
                             #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :spades, :type :standard}
                             #remi.Card{:rank #remi.Rank{:value 9, :number 10}, :suit :diamonds, :type :standard}
                             #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :hearts, :type :standard}
                             #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :clubs, :type :standard}]
                            #remi.Card{:rank #remi.Rank{:value 9, :number 14}, :suit :spades, :type :standard}))



(sort-hand @(:hand (get-player players 4)))



(find-same-rank-different-suits [#remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :diamonds, :type :standard}
                                 #remi.Card{:rank nil, :suit nil, :type :joker}
                                 #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :spades, :type :standard}
                                 #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :hearts, :type :standard}
                                 ] #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :diamonds, :type :standard})

(same-cards-different-suits [#remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :diamonds, :type :standard}
                             #remi.Card{:rank nil, :suit nil, :type :joker}
                             #remi.Card{:rank #remi.Rank{:value 9, :number 11}, :suit :diamonds, :type :standard}
                             ])



(sequential-cards
  (update-first-card-if-ace [{:rank {:name :ten, :value 5}, :suit :diamonds, :type :standard}
                             {:rank {:name :three, :value nil}, :suit :clubs, :type :joker}
                             {:rank {:name :ten, :value nil}, :suit :diamonds, :type :joker}
                             ]))


(if (=
      (+
        (:value
          (:rank {:rank {:name :three, :value 5}, :suit :diamonds, :type :standard})
          )  1)

      (:value (:rank (first [
                             {:rank {:name :ten, :value 6}, :suit :diamonds, :type :standard}
                             ]))))
  (recur (first [
                 {:rank {:name :ten, :value 6}, :suit :diamonds, :type :standard}
                 ]) )
  false)


(empty? (rest [
               {:rank {:name :ten, :value 6}, :suit :diamonds, :type :standard}
               ]))


SREDI DA VALUE ZA >10 BUDE 10