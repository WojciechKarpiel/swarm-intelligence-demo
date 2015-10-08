(ns swarm.aco
  (:require [quil.core :as q]))

(def initial-ammount-of-nodes 10)
(def initial-edge-factor 2)
(def max-position 500)
(def evaporation-rate 0.00001)
(def initial-pheromone 100000.0)


(defn rand-pos [] [(rand-int max-position) (rand-int max-position)])

(defn make-node [position]
  [position (atom (list))])

(defn make-nodes [num]
  (loop [result {}
         left-to-do num]
    (if (zero? left-to-do)
      result
      (recur (conj result (make-node (rand-pos)))
             (dec left-to-do)))))

(def vertexes (atom (make-nodes initial-ammount-of-nodes)))
(def start (atom (first (first @vertexes))))
(def end (atom (first (second @vertexes))))

(defn distance [node1 node2]
  (Math/sqrt (+ (Math/pow (- (first node1)
                             (first node2)) 2)
                (Math/pow (- (second node1)
                             (second node2)) 2))))


(defn make-edge [pos1 pos2]
  (let [at1 (@vertexes pos1)
        at2 (@vertexes pos2)
        pher (atom initial-pheromone)
        dist (distance pos1 pos2)]
    (swap! at2 conj {:pos pos1 :pher pher :dist dist} )
    (swap! at1 conj {:pos pos2 :pher pher :dist dist})
    pher))

(defn make-random-edge [[pos at]]
  (let [taken-positions (map :pos @at)
        seq-vertexes  (seq @vertexes)] 
    (loop [new-pos (first (rand-nth seq-vertexes))]
       (if (or (= new-pos pos) (some #(= new-pos %) taken-positions))
         (recur (first (rand-nth seq-vertexes)))
         (make-edge pos new-pos)))))


(defn make-random-edges [passes]
  (loop [left-to-do passes
         result '()]
    (if (zero? left-to-do)
      result
      (recur (dec left-to-do) (concat result (map make-random-edge @vertexes))))))

(def edges-g (atom (doall (make-random-edges initial-edge-factor))))

(defn edge-visibility [edge]
  (* @(:pher edge) (/ 1.0 (:dist edge))))

(defn traverse-ant []
  (loop [visited-pos #{}
         visited-edges #{}
         tour-length 0.0
         pos @start]
    (if (= pos @end)
      {:visited-edges visited-edges :tour-length tour-length}
      (let [adjacent-edges (get @vertexes pos)
        avaliable-edges (filter #(not (contains? visited-pos (:pos %))) @adjacent-edges)]
        (if (empty? avaliable-edges)
          {:tour-length Double/POSITIVE_INFINITY :visited-edges #{}}
          ;;now picking random edge- refactor into separate function
          (let [sum (reduce (fn [r n] (+ r (edge-visibility n))) 0.0 avaliable-edges)
                goal (rand sum)
                picked-edge (loop [sum 0
                                   edges avaliable-edges]
                              (let [edge (first edges)
                                    visibility (edge-visibility edge)]
                                (if (<= goal (+ sum visibility))
                                  edge
                                  (recur (+ sum visibility) (rest edges)))))]     
            (recur (conj visited-pos pos) (conj visited-edges picked-edge) (+ tour-length (:dist picked-edge)) (:pos picked-edge))))))))

(def reward-factor 1000.0)
(defn reward-path [traversed]
  (let [reward (* reward-factor (/ 1 (:tour-length traversed)))]
    (dorun (map (fn [edge] (swap! (:pher edge) + reward)) (:visited-edges traversed)))
    reward))

(defn evaporate [at]
  (swap! at (fn [val] (* val (- 1 evaporation-rate)))))

(defn go-find-food [n-ants n-iterations]
  (loop [i n-iterations]
    (if (zero? i)
      :DONE
      (let [traversed-data (loop [j n-ants
                                  res (list)]
                             (if (zero? j)
                               res
                               (recur (dec j) (conj res (future (traverse-ant))))))
            best-tour (future (reduce (fn [res new] (if (< (:tour-length res) (:tour-length @new))
                                                       res
                                                       @new)) @(first traversed-data) (rest  traversed-data)))]
        
        (dorun (map (fn [at] (evaporate at)) @edges-g))
        (reward-path @best-tour)
        (recur (dec i))))))


(defn reset-pheromones []
  (dorun (map #(reset! % initial-pheromone) @edges-g)))


;; grafika

(defn setup []                 
  (q/frame-rate 5)                    
  (q/background 255))

(defn scale-colour [pher] (let [max-pher (apply max (map deref @edges-g))]
                            (Math/floor (* (/ 225 max-pher) pher))))

(defn draw []
  (q/background 255)
  
  (dorun (map (fn [[[x y] at]]
                (dorun (map (fn [{:keys [:pher :pos]}]
                              (let [ colour (scale-colour @pher)]
                                (q/stroke colour 0 0 )
                                (q/line x y (first pos) (second pos))))
                            @at))
                (cond
                  (= [x y] @end) (q/fill 0 225 0)
                  (= [x y] @start) (q/fill 0 225 225)
                  true (q/fill 0 0 0))
                (q/ellipse x y 8 8))
              @vertexes)))


(q/defsketch aco-sketch                  
 :title "ACO. Nest is blue; Food is green. Red is highway"
 :setup setup
 :draw draw
 :size [max-position max-position])

;; pomoc


(defn set-nest [pos] (reset! start pos))
(defn set-food [pos] (reset! end pos))

(defn reset-edges [factor]
  (dorun (map #(reset! (second %) (list)) @vertexes))
  (reset! edges-g (make-random-edges factor)))

(defn setup-vertexes
  ([ammount-of-nodes] (setup-vertexes ammount-of-nodes initial-edge-factor))
  ([ammount-of-nodes factor]
    (reset! vertexes (make-nodes ammount-of-nodes))
    (set-nest (first (first @vertexes)))
    (set-food (first (second @vertexes)))
    (reset-edges factor)))

(defn get-positions-list []
  (map first @vertexes))

