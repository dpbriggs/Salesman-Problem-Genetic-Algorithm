(ns genetic-algorithm.core
  (:gen-class))

(defstruct city :name :x :y :pos)

;; Ascii map for city name gen
(def ascii-map #(get (vec (map char (range 65 91))) %))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Generate city information  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gen-city-name
  "Adds ascii char to number to identify city"
  [num]
  (if (>= num 26)
    (str (ascii-map (mod num 26)) (gen-city-name (- num 25)))
    (str (ascii-map num))))


(defn gen-random-city-scape
  "Generates num-cities amount of random cities on a map"
  [num-cities dist-x dist-y]
  ;; Create map of randomly generated cities
  (vec (loop [num    num-cities
              cities []]
         (if (zero? num)
           cities
           (let [n-num (dec num)
                 new-city (struct city
                                  (gen-city-name n-num)
                                  (rand-int dist-x)
                                  (rand-int dist-y)
                                  (dec num))
                 n-cities (conj cities new-city)]
             (recur n-num n-cities))))))

(defn scramble-city-order
  "Returns 6 times the number of cities of randomly permuted city routes"
  [cities]
  ; Find unique permutes of (6 times (count cities)) permutations of cities
  (->> #(shuffle cities)
       (repeatedly (* (count cities) 6)) ; repeatedly shuffle cities 6 * (count cities)
       distinct)) ; get me all the unique routes

(defn distance-between
  "Finds distance between two cities using pythagorean theorum"
  [city-1 city-2]
  (let [xdist (Math/abs (- (:x city-1) (:x city-2)))
        ydist (Math/abs (- (:y city-1) (:y city-2)))]
    (Math/pow (+ (* xdist xdist) (* ydist ydist)) 0.5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start of fitness-test ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Routes are a vector of cities in which index[0] is first stop, etc

(defn route-distance
  "Finds total distance taken in route"
  [route]
  (loop [current-city route
         next-city    (rest route) ;; ensure we never are on same city
         distance     0]
    (if (zero? (count next-city)) ; Ran out of cities
      {:distance distance :route route}
      ;; n --> new
      (let [n-current-city (rest current-city) ; Move heads up
            n-next-city    (rest next-city)
            n-distance     (+ distance (distance-between ; Add distance to total
                                        (first current-city)
                                        (first next-city)))]
        (recur n-current-city n-next-city n-distance)))))

(defn fitness-test
  "Grabs top N routes"
  [N routes]
  ; fitness is dummy variable to hold vector of fit routes
  (let [fitness    (->> routes
                        (map route-distance)
                        (sort-by :distance)
                        (take N)
                        vec)
        fittest    (:distance (first fitness))
        new-routes (vec (map :route fitness))]
    {:top-distance fittest :routes new-routes}))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Start of crossover ;;
;;;;;;;;;;;;;;;;;;;;;;;;


(defn gen-starting-pop
  "Convenience function to grab fit pop"
  [fitness-number num-cities city-dist-x city-dist-y]
  (->> city-dist-y
       (gen-random-city-scape num-cities city-dist-x)
       scramble-city-order
       (fitness-test fitness-number)))

(defn rand-int-between
  "Return a rand-int between two bounds"
  [upper-bound lower-bound]
  (+ (rand-int (- upper-bound lower-bound)) lower-bound))

(defn swap [pop i1 i2]
  "Swap two values i1 and i2 in vector v"
  (let [v (vec pop)]
   (vec (assoc v i2 (v i1) i1 (v i2)))))

(defn mutate
  "Randomly swich values in vector to simulate genetic mutations"
  [child]
  (if (>= (rand-int 99) 10) ;; 11% mutation chance
    child
    (loop [i1 (rand-int (count child))
           i2 (rand-int (count child))] ;; ensure we're not flipping same vals
      (if (not= i1 i2)
        (swap child i1 i2)
        (recur (rand-int (count child))
               (rand-int (count child)))))))

(defn non-swath-values
  "Removes values from parent-2 found in swath to avoid duplicates"
  [parent-2 swath]
  (filter #(not (contains? swath)) parent-2))

(defn simple-crossover
  "Splices two vectors together to produce two children"
  ;Parent 1         1 2 3 4 5 6 7 8        "
  ;Swath            1 2 3 - - - 7 8        "
  ;Parent 2         8 7 6 5 4 3 2 1        "
  ;                                        "
  ;Result    -->    1 2 3 5 4 3 2 1        "
  ;Parent 2 vals  --->    - - -            "
  ;Repeat with reversed parents            "

  [[parent-1 parent-2]]
  (let [len          (count parent-1)
        swath        (rand-int-between (- len 2) (quot len 4))
        starting-pos (rand-int (- len swath))

        ;; Parent 1
        swath-part-1 (->> parent-2
                          (take (+ swath starting-pos))
                          (drop starting-pos)
                          (vec))
        non-swath-1  (remove (set swath-part-1) parent-1)
        head-1       (take starting-pos non-swath-1)
        tail-1       (take-last (- len swath starting-pos) non-swath-1)
        child-1      (mutate (concat head-1 swath-part-1 tail-1))

        ;; Parent 2 (same thing except parents are flipped)
        swath-part-2 (->> parent-1
                          (take (+ swath starting-pos))
                          (drop starting-pos)
                          (vec))
        non-swath-2  (remove (set swath-part-2) parent-2)
        head-2       (take starting-pos non-swath-2)
        tail-2       (take-last (- len swath starting-pos) non-swath-2)
        child-2      (mutate (concat head-2 swath-part-2 tail-2))]
    [child-1 child-2]))


(defn apply-crossover
  "Takes the routes and forces pairs of routes to make babies"
  [routes]
  (let [partners     (partition 2 (shuffle routes))
        children-1   (map simple-crossover partners)
        children-2   (map simple-crossover partners) ;; Double the children (to 4)
        all-children (concat children-1 children-2)]
    (apply concat all-children)))

(defn evolution-cycles
  "applies crossover/fitness cycles num-cycles amount of times to a route"
  [route num-cycles top-fit-num]
  (loop [stop          num-cycles
         population    (->> route
                            scramble-city-order
                            (fitness-test top-fit-num)
                            :routes)
         fitness-dists []]
    (if (zero? stop)
      {:top-distances fitness-dists
       :best-time     (last fitness-dists)
       :population    population
       :top-route     (first population)}
      (let [n-stop          (dec stop)
            fit             (fitness-test top-fit-num (apply-crossover population))
            n-population    (:routes fit)
            n-fitness-dists (conj fitness-dists (:top-distance fit))]
        (recur n-stop n-population n-fitness-dists)))))

(defn run-multi-parallel
  "Run algorithm several times in parallel"
  [starting-route parallel-times num-cycles top-fit-num]
  (let [chroms (repeat parallel-times starting-route)]
    (pmap #(evolution-cycles % num-cycles top-fit-num) chroms))) ;pmap -> parallel

(def manu-and-i-data-set [{:name "P", :x 72, :y 92, :pos 15}
                          {:name "O", :x 75, :y 35, :pos 14}
                          {:name "N", :x 38, :y 99, :pos 13}
                          {:name "M", :x 95, :y 40, :pos 12}
                          {:name "L", :x 32, :y 50, :pos 11}
                          {:name "K", :x 1, :y 22, :pos 10}
                          {:name "J", :x 81, :y 23, :pos 9}
                          {:name "I", :x 83, :y 16, :pos 8}
                          {:name "H", :x 88, :y 13, :pos 7}
                          {:name "G", :x 36, :y 99, :pos 6}
                          {:name "F", :x 6, :y 54, :pos 5}
                          {:name "E", :x 53, :y 4, :pos 4}
                          {:name "D", :x 76, :y 80, :pos 3}
                          {:name "C", :x 74, :y 42, :pos 2}
                          {:name "B", :x 56, :y 54, :pos 1}
                          {:name "A", :x 44, :y 79, :pos 0}])

(defn -main
  "Fancy wrapper for run-multi-parallel function"
  ;; Run with custom route
  ([starting-route parallel-times num-cycles top-fit-num]
    (println (run-multi-parallel starting-route parallel-times num-cycles top-fit-num)))
  ;; Run without custom route
  ([parallel-times num-cycles top-fit-num]
     (println (run-multi-parallel manu-and-i-data-set
                                  (Integer. parallel-times)
                                  (Integer. num-cycles)
                                  (Integer. top-fit-num))))
  ([]
     (println (sort (map :best-time (run-multi-parallel manu-and-i-data-set 4 1000 10))))))
