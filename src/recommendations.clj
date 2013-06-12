(ns collective.intelligence.recommendations)


(def critics
  {"Lisa Rose" {"Lady in the Water" 2.5, "Snakes on a Plane" 3.5, "Just My Luck" 3.0, "Superman Returns" 3.5, "You, Me and Dupree" 2.5, "The Night Listener" 3.0},
   "Gene Seymour" {"Lady in the Water" 3.0, "Snakes on a Plane" 3.5, "Just My Luck" 1.5, "Superman Returns" 5.0, "You, Me and Dupree" 3.5, "The Night Listener" 3.0},
   "Michael Phillips" {"Lady in the Water" 2.5, "Snakes on a Plane" 3.0, "Superman Returns" 3.5, "The Night Listener" 4.0},
   "Claudia Puig" {"Snakes on a Plane" 3.5, "Just My Luck" 3.0, "Superman Returns" 4.0, "You, Me and Dupree" 2.5, "The Night Listener" 4.5},
   "Mick LaSalle" {"Lady in the Water" 3.0, "Snakes on a Plane" 4.0, "Just My Luck" 2.0, "Superman Returns" 3.0, "You, Me and Dupree" 2.0, "The Night Listener" 3.0},
   "Jack Matthews" {"Lady in the Water" 3.0, "Snakes on a Plane" 4.0, "Superman Returns" 5.0, "You, Me and Dupree" 3.5, "The Night Listener" 3.0},
   "Toby" {"Snakes on a Plane" 4.5, "You, Me and Dupree" 1.0, "Superman Returns" 4.0}})

(defn- ratings[person1 person2]
  (reduce + (vals (merge-with (fn[m1 m2] (Math/pow (- m1 m2) 2)) (critics person1) (critics person2)))))

(defn sim-distance[person1 person2]
  (/ 1 (+ 1 (Math/sqrt (ratings person1 person2)))))

;; Movies that have been rated mutually
(defn- common-movies[person1 person2]
  (clojure.set/intersection (set (keys (critics person1))) (set (keys (critics person2)))))

(defn- ratings-for[person common-movies]
  (map (critics person) common-movies))

(defn- ratings-for[person movies]
  (reduce (fn[acc movie] (conj acc {movie (get-in critics [person movie])})) {} movies))

(defn- square-of[kv]
  (* (val kv) (val kv)))

(defn- process-ratings[map-fn ratings-for-a-person]
  (reduce + (map map-fn ratings-for-a-person)))

(defn sim-pearson[person1 person2]
  (let [person1-ratings (ratings-for person1 (common-movies person1 person2))
        person2-ratings (ratings-for person2 (common-movies person1 person2))
        sum1 (process-ratings #(val %1) person1-ratings)
        sum2 (process-ratings #(val %1) person2-ratings)
        sum1Sq (process-ratings square-of person1-ratings)
        sum2Sq (process-ratings square-of person2-ratings)
        sumProducts (reduce + (map #(* (val %1) (val %2)) person1-ratings person2-ratings))
        n (count (common-movies person1 person2))
        num (- sumProducts (/ (* sum1 sum2) n))
        denominator (Math/sqrt (* (- sum1Sq (/ (* sum1 sum1) n)) (- sum2Sq (/ (* sum2 sum2) n))))
        ]
      (if (zero? n) 0
        (if (zero? denominator) 0 (/ num denominator))
        )
    )
  )

(defn- other-critics[person]
  (filter #(not= %1 person) (keys critics)))

;; Either sim_distance or sim_pearson
(defn top-matches[person n sim-fn]
  (let [others (other-critics person)
        scores (reduce (fn[acc curr] (conj acc {(sim-fn person curr) curr})) {} others)
        ]
    (take n (sort-by key > scores))))

(defn- collect[seed current]
  (conj seed [(first current) [(+ (first (second current)) (or (first (seed (first current))) 0.0))
                                (+ (second (second current)) (or (second (seed (first current))) 0.0))]]))

(defn recommendations[person sim-fn]
  (let [others (other-critics person)
        scores (reduce #(conj %1 {%2 (sim-fn person %2)}) {} others)
        all-rated-movies (distinct (flatten (reduce #(conj %1 (keys %2)) [] (map critics others))))
        movies-not-rated (keys (filter #(nil? (val %1)) (reduce #(conj %1 {%2 (get-in critics [person %2])}) {} all-rated-movies)))
        all-ratings (reduce collect {} (for [movie movies-not-rated, [critic score] scores] [movie [(* (get-in critics [critic movie] 0.0) score) score]]))
        rankings (reduce (fn[acc curr] (conj acc [(/ (first (val curr)) (second (val curr))) (key curr)])) {} all-ratings)
        ]
    (sort-by key > rankings))
  )


