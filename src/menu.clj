(ns menu
  (:require [remi :as remi])
  (:require [clojure.string :as str]))

(defn starting-question []
  (println "Welcome, how many players are playing Remi?(0 - to exit)"))

(defn string-to-number-vector [s]
  (try
    (->> (str/split s #" ")
         (map #(Integer. %))
         (vec))
    (catch NumberFormatException e
      (println "Error: Invalid format")
      nil)))

(defn display-turn-options [player]
  (remi/draw-from-deck player deck)
  (println (str (:name player) "'s turn, your hand:"))
  (show-hand (:hand player))
  (println "")
  (println "1) Drop Card")
  (println "2) Open")
  (println "3) Exit"))

(defn get-choice []
  (print "Enter your choice: ")
  (flush)
  (Integer. (read-line)))

(defn get-choice-string []
  (print "Enter your choice: ")
  (flush)
  (read-line))

(defn handle-starting-question-choice [choice]
  (if (= choice 0)
    (do
      (println "Exiting..."))
    (do
      (def deck (atom (shuffle (remi/create-deck choice))))
      (def players (remi/make-players choice deck))
      (loop []
        (doseq [player players]
          (handle-turn player))
        (recur)))))

(defn show-hand
  [hand]
  (doseq [[idx card] (map-indexed vector @hand)]
    (if (= (:type card) :joker)
      (println (str idx ". Joker"))
      (println (str idx ". " (:rank card) " of " (:suit card))))))

(defn handle-turn
  [player]
  (do
    (display-turn-options player)
    (handle-turn-choice player (get-choice))))

(defn sets-contain-same-cards? [x y]
  (if (> (count (set (filter #(contains? y %) x))) 0) true false))


(defn handle-pack-choice
  [player string taken-cards]
  (let [pack-cards (string-to-number-vector string)]
    (if (sets-contain-same-cards? (set taken-cards) (set pack-cards))
      (do
        (println "You have chosen taken cards, invalid pack")
        0
        )
      (remi/check-pack @(:hand player) pack-cards))))

(defn handle-drop-card-menu
  [player choice sum taken-cards]
  (case choice
    1 (do
        (println "What cards do you want in a pack(Insert place of cards separated by spaces):")
        (let [choice (get-choice-string) pack-value (handle-pack-choice player choice taken-cards)]
          (if (> pack-value 0)
            (drop-card-menu player (+ sum pack-value) (vec (concat taken-cards (string-to-number-vector choice))))
            (drop-card-menu player (+ sum pack-value) taken-cards))
          ))
    2 (do
        (if (> sum 50)

          (println "Your sum needs to be at least 51")))
    3 (do
        )
    (println "Invalid choice. Please try again.")))

(defn drop-card-menu
  [player sum taken-cards]
  (let []
    (show-hand (:hand player))
    (println "taken cards: " taken-cards)
    (println "current sum of packs:" sum "(at least 51 needed for opening)")
    (println "1) Set up pack for dropping")
    (println "2) Drop")
    (println "3) Back")
    (handle-drop-card-menu player (get-choice) sum taken-cards)))

(defn handle-turn-choice [player choice]
  (case choice
    1 (do
        (println "What is the place of card that you want to drop?")
        (remi/drop-card player
                        (loop [] (let [choice (get-choice)]
                                   (if (and (< choice 16) (> choice 0)) (- choice 1) (recur))))
                        )
        (println "\n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n
        \n \n \n \n \n \n \n \n \n \n \n Insert anything to start your turn:")
        (get-choice)
        )
    2 (do
        (drop-card-menu player 0 [])
        )
    3 (do
        (println "Exiting..."))
    (println "Invalid choice. Please try again.")))





(defn -main []
  (starting-question)
  (handle-starting-question-choice (get-choice)))