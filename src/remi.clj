(ns remi)

(defrecord Card [rank suit type])
(defrecord Rank [value number])

(def ranks
  (range 2 15))

(def suits #{:clubs :diamonds :hearts :spades})

(def joker (->Card nil nil :joker))

(defn create-deck
  "returns playing deck for number of players. If number of players is higher than 3
  2 standards decks of cards are combined into remi deck,
  otherwise 1 standard deck of cards is used as a remi deck"
  [number-of-players]
  (if
    (> number-of-players 3)
    (let [standard-cards (for [r ranks, s suits] (->Card r s :standard))
          jokers [joker joker]]
      (let [deck (concat standard-cards jokers)]
        (concat deck deck)))
    (let [standard-cards (for [r ranks, s suits] (->Card r s :standard))
          jokers [joker joker]]
      (concat standard-cards jokers))))

(def deck (atom (shuffle (create-deck 4))))
(def middle-pile (atom []))

(defn deal-hand
  "takes certain number of cards from passed deck and returns it"
  [num-cards deck]
  (let [hand (take num-cards @deck)]
    (swap! deck #(drop num-cards %))
    hand))

(defrecord Player [name hand])

(defn make-players
  "makes passed number of players and gives them 14 card each from passed deck"
  [number-of-players deck]
  (vec
    (for [n (range number-of-players)]
      (let [player-name (str "player" (inc n))
            player-hand (atom (deal-hand 14 deck))]
        (->Player player-name player-hand)))))

(defn get-player
  "returns nth player from passed players"
  [players number]
  (nth players (dec number)))

(defn get-player-hand
  "returns nth players hand from passed players"
  [players number]
  (let [player (nth players (dec number))
        player-hand (:hand player)]
    @player-hand))

(defn draw-from-deck
  "takes 1 card from passed deck and puts it into passed players hand"
  [player deck]
  (swap! (:hand player) concat (deal-hand 1 deck)))



(defn drop-card
  [player card-position]
  "removes card in passed card position from passed players hand
  and adds it into middle-lie"
  (let [player-hand @(:hand player)
        card (nth player-hand card-position)]
    (swap! (:hand player) #(concat (take card-position %) (drop (inc card-position) %)))
    (swap! middle-pile conj card)))


(count @middle-pile)


(defn all-same-suit?
  "returns true if all of passed cards are same suit and false if they are not. Jokers count as different suit."
  [cards]
  (let [sorted-cards (sort-by :type cards)]
    (let [last-suit (:suit (last sorted-cards))]
      (every? #(or (= last-suit (:suit %)) (= :joker (:type %))) sorted-cards))))

(defn same-cards
  "checks if passed cards are same rank,
  if yes returns sum value of that cards, otherwise returns 0"
  [cards]
  (let [sorted-cards (sort-by :type cards)]
    (let [last-value (:rank (last sorted-cards))]
      (if
        (every? #(or (= (:rank %) last-value) (= :joker (:type %))) sorted-cards)
        (* (count sorted-cards) last-value)
        0))))

(defn all-different-suits?
  "returns true if all passed cards are different suits, otherwise returns false"
  [cards]
  (let [suits (map :suit cards)
        unique-suits (set suits)]
    (= (count suits) (count unique-suits))))

(defn same-cards-different-suits
  "checks if passed cards are same rank and different suits,
  if yes returns sum value of that cards, otherwise returns 0"
  [cards]
  (let [value (same-cards cards)]
    (if (and (all-different-suits? cards) (> value 0))
      value
      0
      )))

(defn update-first-card-if-ace
  "checks if first card in a passed group is 14(Ace),
  if yes updates its rank 1 and returns updated group"
  [group]
  (if (= (:rank (first group)) 14)
    (let [updated-card (assoc-in (first group) [:rank] 1)
          updated-group (cons updated-card (rest group))]
      updated-group)
    group))

(defn find-first-standard
  "returns first card in passed cards that has standard type"
  [cards]
  (some #(if (= (:type %) :standard) % nil) cards))

(defn index-of
  "returns index of a specified element in a collection"
  [coll element]
  (first
    (keep-indexed
      (fn [idx el]
        (when
          (= el element)
          idx))
      coll)))

(defn sum-of-sequential-cards
  "returns sum value of cards in a passed group
  if cards are in a sequence, otherwise returns 0"
  [group]
  (loop [sum 0 first-card (first group)
         remaining (rest group)]
    (if
      (empty? remaining)
      (+ sum (:rank first-card))
      (if
        (= (:type first-card) :joker)
        (recur
          (let [first-standard (find-first-standard remaining)]
            (+ sum (- (:rank first-standard) (+ 1 (index-of remaining first-standard)))))
          (first remaining)
          (rest remaining))
        (if
          (= (+ (:rank first-card) 1) (:rank (first remaining)))
          (recur (+ sum (:rank first-card)) (first remaining) (rest remaining))
          (if
            (= :joker (:type (first remaining)))
            (recur (+ sum (:rank first-card)) {:rank (+ (:rank first-card) 1), :suit (:suit first-card), :type :standard} (rest remaining))
            0))))))

(defn in-a-row-same-suit
  "checks if cards in passed group are all the same suit,
  if yes returns sum of sequence of that group, otherwise returns 0"
  [group]
  (if
    (all-same-suit? group)
    (sum-of-sequential-cards (update-first-card-if-ace group))
    0))

(defn check-pack
  "checks if cards in passed hand on passed group numbers form remi pack,
  if yes returns value of that pack, otherwise returns 0.
  Remi pack is any pack that has 3 or more cards of same rank and different suits OR
  any pack that has 3 or more cards of the same suit in sequence."
  [hand group-numbers]
  (let [group (map #(nth hand %) group-numbers)
        value-in-a-row (in-a-row-same-suit group)]
    (if
      (> value-in-a-row 0)
      value-in-a-row
      (same-cards-different-suits group))))

(defn type-of-pack
  "returns type of pack that passed group forms,
  if it forms sequence of 3 or more cards of the same suit it returns string 'pack-in-a-row',
  if it forms pack of 3 or more cards of same rank and different suits it returns string 'pack-same-numbers',
  otherwise returns 0"
  [group]
  (let [value-in-a-row (in-a-row-same-suit group)]
    (if
      (> value-in-a-row 0)
      "pack-in-a-row"
      (if
        (> (same-cards-different-suits group) 0)
        "pack-same-numbers"
        0))))

(defn sort-hand [hand]
  "returns passed hand sorted by suits and ranks"
  (let [suit-order {:clubs 0, :diamonds 1, :hearts 2, :spades 3}]
    (sort-by
      (fn [card]
        [(suit-order (:suit card)) (:rank card)])
      hand)))


(defn count-same-rank-different-suits
  "returns count of cards in passed hand that have same rank and different suits as a passed reference card"
  [hand reference-card]
  (let [reference-rank (:rank reference-card)
        reference-suit (:suit reference-card)]
    (->> hand
         (filter
           #(and
              (= (:rank %) reference-rank)
              (not= (:suit %) reference-suit)))
         count)))

(defn find-same-rank-different-suits
  "returns all cards in passed hand that have same rank and different suits as a passed reference card"
  [hand reference-card]
  (let [reference-rank-number (:rank reference-card)
        reference-suit (:suit reference-card)]
    (->> hand
         (filter #(and (= reference-rank-number (:rank %))
                       (not= reference-suit (:suit %))))
         (into #{}))))

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
  (loop [reference-list [reference-card]                    ; Initialize with the reference card
         reference reference-card                           ; Start with the reference card
         remaining-cards lower-cards]                       ; Consider only the cards that are potentially lower
    (if (and (seq remaining-cards)                          ; Check if there are any remaining cards
             (= (- (:rank reference) 1)
                (:rank (first remaining-cards))))           ; Compare the rank number to find a descending match
      (recur (conj reference-list (first remaining-cards))  ; Add matching card to the list
             (first remaining-cards)                        ; Update the reference card to the new match
             (rest remaining-cards))                        ; Remove the used card from the remaining list
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
            desc-seq (build-desc-sequence (filter #(not= (:rank %) 14) (reverse sorted-cards)) reference-card)]
        (if (> (count desc-seq) (count asc-seq)) desc-seq asc-seq))
      (concat
        (reverse (rest (build-desc-sequence lower-cards reference-card)))
        (build-asc-sequence higher-cards reference-card)))))

(filter #(= (:suit %) :hearts) [#remi.Card{:rank 10, :suit :diamonds, :type :standard}
                                #remi.Card{:rank 11, :suit :diamonds, :type :standard}
                                #remi.Card{:rank 12, :suit :diamonds, :type :standard}
                                #remi.Card{:rank 5, :suit :clubs, :type :standard}
                                #remi.Card{:rank 5, :suit :hearts, :type :standard}
                                #remi.Card{:rank 13, :suit :spades, :type :standard}
                                #remi.Card{:rank 13, :suit :hearts, :type :standard}
                                #remi.Card{:rank 14, :suit :hearts, :type :standard}
                                #remi.Card{:rank 8, :suit :hearts, :type :standard}
                                #remi.Card{:rank 7, :suit :hearts, :type :standard}
                                #remi.Card{:rank 7, :suit :clubs, :type :standard}
                                #remi.Card{:rank 13, :suit :clubs, :type :standard}
                                #remi.Card{:rank 6, :suit :diamonds, :type :standard}
                                #remi.Card{:rank 10, :suit :clubs, :type :standard}])


(defn find-best-pack-for-a-card
  [hand card]
  (let [sorted-hand (sort-hand hand)
        same-rank-pack (cons card (find-same-rank-different-suits hand card))
        sequence-pack (find-sequence-cards sorted-hand card)]
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
  (let [length-best-pack (count (find-best-pack-for-a-card hand card))
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

(defn find-potential-useful-cards-for-pack
  [pack]
  (let [type-of-pack (type-of-pack pack)
        result []]
    (if
      (= type-of-pack "pack-in-a-row")
      (vec (conj
             (conj
               result
               (check-bottom-of-sequence-for-potential-card pack))
             (check-top-of-sequence-for-potential-card pack)))
      (if
        (= type-of-pack "pack-same-numbers")
        (vec (check-same-rank-pack-for-potential-card pack))))))

(find-potential-useful-cards-for-pack
  [#remi.Card{:rank 10, :suit :diamonds, :type :standard}
   #remi.Card{:rank 10, :suit :spades, :type :standard}
   #remi.Card{:rank 10, :suit :hearts, :type :standard}
   ])

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

(defn find-useful-cards-for-pack
  [pack remaining-cards]
  (let [potential-useful-cards (find-potential-useful-cards-for-pack pack)]
    (ordered-vector-intersection potential-useful-cards remaining-cards)))

(defn vector-difference
  [vec1 vec2]
  (let [to-remove (frequencies vec2)                        ; Creates a map of elements to their counts in vec2
        result (reduce (fn [[res counts] item]              ; Destructure the accumulator into res and counts
                         (if (and (> (get counts item 0) 0) ; If item should be removed
                                  (contains? counts item))
                           [res (update counts item dec)]   ; Decrement count in map, keep result the same
                           [(conj res item) counts]))       ; Else, add item to result
                       [[] to-remove]                       ; Initial accumulator: empty result vector and to-remove map
                       vec1)]
    (first result)))


(defn count-card-occurrences
  [cards card-to-count]
  (count (filter #(= % card-to-count) cards)))

(defn sum-vector
  [vec]
  (reduce + 0 vec))

(defn quality-of-a-pack
  [pack]
  (count pack))

(defn find-strength-of-card
  [hand card middle-pile]
  (let [remaining-cards (vector-difference (vector-difference (create-deck 4) middle-pile) hand)
        useful-cards (find-useful-cards hand card remaining-cards)
        current-best-pack (find-best-pack-for-a-card hand card)]
    (+ (quality-of-a-pack current-best-pack)
       (sum-vector (map
                     #(let
                        [new-best-pack (find-best-pack-for-a-card (conj hand %) card)]
                        (if
                          (> (quality-of-a-pack new-best-pack) (quality-of-a-pack current-best-pack))
                          (*
                            (quality-of-a-pack new-best-pack)
                            (* (count-card-occurrences remaining-cards %) (/ 1 (count remaining-cards))))
                          0)) useful-cards)))))

(defrecord Best-Pack [cards strength card])



(defn find-strength-of-pack
  [hand card taken-cards]
  (let [remaining-cards (vector-difference (vector-difference (create-deck 4) taken-cards) hand)
        longest-sequence (find-sequence-cards hand card)
        longest-same-rank-pack (cons card (find-same-rank-different-suits hand card))
        useful-cards-sequence (find-useful-cards-for-pack longest-sequence remaining-cards)
        useful-cards-same-rank (find-useful-cards-for-pack longest-same-rank-pack remaining-cards)]

    (vector
      (->Best-Pack longest-sequence (+
                                      (quality-of-a-pack longest-sequence)
                                      (sum-vector
                                        (map
                                          #(let
                                             [new-best-pack (find-sequence-cards (conj hand %) card)]
                                             (if
                                               (> (quality-of-a-pack new-best-pack) (quality-of-a-pack longest-sequence))
                                               (*
                                                 (quality-of-a-pack new-best-pack)
                                                 (* (count-card-occurrences remaining-cards %) (/ 1 (count remaining-cards))))
                                               0)) useful-cards-sequence))) card)
      (->Best-Pack longest-same-rank-pack (+
                                            (quality-of-a-pack longest-same-rank-pack)
                                            (sum-vector
                                              (map
                                                #(*
                                                   (quality-of-a-pack (cons % longest-same-rank-pack))
                                                   (* (count-card-occurrences remaining-cards %) (/ 1 (count remaining-cards))))
                                                useful-cards-same-rank))) card))))

(defn find-strength-of-whole-hand
  [hand taken-cards]
  (map #(find-strength-of-pack hand % taken-cards) hand))

(defn find-strength-of-some-cards
  [hand target-cards taken-cards]
  (map #(find-strength-of-pack hand % taken-cards) target-cards))

(defn find-length-of-best-pack-for-hand
  [hand]
  (map #(count (find-best-pack-for-a-card hand %)) hand))

(defn find-strongest-pack-in-each-vector
  [packs-list]
  (map (fn [pack-pair]                                      ; Each pack-pair is a vector with two packs
         (if (> (:strength (first pack-pair)) (:strength (second pack-pair)))
           (first pack-pair)
           (second pack-pair)))
       packs-list))

(defn find-highest-strength-pack
  [packs]                                                   ; Flatten the list of lists of packs
  (reduce (fn [max-pack pack]
            (if (> (:strength pack) (:strength max-pack))
              pack
              max-pack))
          (first packs) packs))

(defn find-packs-with-common-cards
  [chosen-pack packs-list]
  (let [chosen-cards (set (:cards chosen-pack))]            ; Create a set of cards from the chosen pack for quick lookup
    (filter (fn [pack]
              (not-empty (clojure.set/intersection chosen-cards (set (:cards pack))))) ; Check for common cards
            packs-list)))

(defn sort-cards-in-pack
  [pack]
  (update pack :cards #(sort-by (juxt :rank :suit) %)))


(defn pack-contains?
  [pack-list pack]
  (some (fn [existing-pack]
          (let [existing-cards (:cards (sort-cards-in-pack existing-pack))
                new-cards (:cards (sort-cards-in-pack pack))]
            (and (= existing-cards new-cards))))
        pack-list))


(defn add-unique-pack
  [pack-list pack]
  (if (pack-contains? pack-list pack)
    pack-list
    (conj pack-list pack)))

(defn collect-unique-packs
  [packs]
  (reduce add-unique-pack [] packs))

(defn sum-strengths
  [packs]
  (apply + (map :strength packs)))

(defn find-highest-strength-pack
  [packs]
  (reduce (fn [max-pack current-pack]
            (if (> (:strength current-pack) (:strength max-pack))
              current-pack
              max-pack))
          packs))


(defn find-next-best-pack-in-a-hand
  [hand taken-cards]
  (let [strength-of-whole-hand (find-strength-of-whole-hand hand taken-cards)
        strongest-pack-in-each-vector (find-strongest-pack-in-each-vector strength-of-whole-hand)
        highest-strength-pack (find-highest-strength-pack strongest-pack-in-each-vector)
        packs-common-cards (find-packs-with-common-cards highest-strength-pack strongest-pack-in-each-vector)
        unique-packs-common-cards (collect-unique-packs packs-common-cards)]
    (find-highest-strength-pack (map
                                  (fn [x]
                                    (->Best-Pack (:cards x) (+ (* (count (:cards x)) (:strength x)) (sum-strengths (vec (find-strongest-pack-in-each-vector
                                                                                                                          (find-strength-of-some-cards
                                                                                                                            (vector-difference hand (:cards x))
                                                                                                                            (vector-difference (map #(:card %) packs-common-cards) (:cards x))
                                                                                                                            (concat taken-cards (:cards x))
                                                                                                                            ))))) nil))
                                  unique-packs-common-cards))))


(defn find-worst-card-in-a-hand
  [hand taken-cards]
  (loop [current-hand hand
         current-taken-cards taken-cards]
    (let [next-best-pack (find-next-best-pack-in-a-hand current-hand current-taken-cards)]
      (println "Next best pack:" next-best-pack)
      (if (empty? (vector-difference current-hand (:cards next-best-pack)))
        next-best-pack
        (recur (vector-difference current-hand (:cards next-best-pack))
               (concat current-taken-cards (:cards next-best-pack)))))))