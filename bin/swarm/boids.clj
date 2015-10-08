(ns swarm.boids
  (:require [quil.core :as q]))

(def dimensions 2)
(def running (atom true))
(def scale 1)
(def initial-ammount 20)


(def max-distance 50.0)
(def min-distance 10.0)
(def max-angle (/ Math/PI 3))
(def weight-close 0.2)
(def weight-far-speed 0.1)
(def weight-far-cohesion 0.3)

(def max-position 500)
(def max-speed 40)
(def min-speed 12)

(def speed-factor 0.3)


(def sleep-time 25)

(defn rand-pos [] (rand-int max-position))
(defn rand-speed []  (let [randomized-speed (-  (rand-int (* 2 max-speed)) max-speed)]
                       (if (> randomized-speed 0)
                         (max randomized-speed min-speed)
                         (min randomized-speed (- min-speed)))))



(defn make-boid []
  (loop [result {:position [] :velocity []}
         passes-left dimensions]
    (if (zero? passes-left)
      (agent result)
      (recur (-> result
               (update-in [:position] #(conj % (rand-pos)))
               (update-in [:velocity] #(conj % (rand-speed))))
             (dec passes-left)))))

(defn make-boids [ammount]
  (loop [x ammount
         result #{}]
    (if (zero? x)
           result
           (recur (dec x ) (conj result (make-boid))))))

(def boids (atom (make-boids initial-ammount)))


(defn process-list [boid]
  (map (fn [other-boid]
         (let [deref-boid @other-boid
               [x1 y1] (:position boid)
               [x2 y2] (:position deref-boid)
               [v1x v1y] (:velocity boid)
               distance (Math/sqrt (+ (Math/pow (- x2 x1) 2)
                                      (Math/pow (- y2 y1) 2)))
               sight-angle (Math/abs (- (Math/atan2 v1y v1x)
                                        (Math/atan2 (- y2 y1) (- x2 x1))))]
           {:distance distance
            :see-and-in-range (and (< sight-angle max-angle)
                                   (< distance max-distance))
            :too-close (< distance min-distance)
            :position (:position deref-boid)
            :velocity (:velocity deref-boid)})) @boids))



(defn behave [boid]
  "function sent to boid agent"
  (let [distances-and-meta (process-list boid)
        close (filter #(or (:see-and-in-range %) (zero? (:distance %))) distances-and-meta)
        too-close (filter #( or (:too-close %) (zero? (:distance %))) distances-and-meta)
        close-count (count close)
        too-close-count (count too-close)
        mean-position (map #(/ % close-count)
                          (reduce (fn [[sum-x sum-y] {[x y] :position}]
                                    [(+ sum-x x) (+ sum-y y)])
                                [0 0] close))
        mean-distance (/ (reduce (fn [sum x] (+ sum (:distance x))) 0 close) close-count)
        mean-speed (map #(/ % close-count)
                           (reduce (fn [[sum-vx sum-vy] {[vx vy] :velocity}]
                                     [(+ sum-vx vx) (+ sum-vy vy)])
                                 [0 0] close))
        old-speed (:velocity boid)
        aligned-speed (map
                        (fn [f] (+ (f old-speed) (* weight-far-speed
                                                     (- (f mean-speed) (f old-speed)))))
                        [first second])
        cohesed-position (reduce (fn [my-pos {other-pos :position dist :distance}]
                                   (if (zero? dist)
                                     my-pos
                                      (map (fn [f] (+ (f my-pos)
                                                      (* weight-far-cohesion
                                                         (/ (* (- (f other-pos) (f my-pos))
                                                               (- dist mean-distance))
                                                            dist))))
                                           [first second])))
                                 (:position boid) close)
        separated-position-after-cohestion (reduce (fn [my-pos {other-pos :position dist :distance}]
                                                   (if (zero? dist)
                                                     my-pos
                                                      (map (fn [f] (- (f my-pos)
                                                                      (* weight-close (- (f other-pos) (f my-pos))
                                                                         (- (/ min-distance dist) 1))))
                                                           [first second])))
                                                 (:position boid) too-close)
        flewn-pos (map (fn [f] (+ (f separated-position-after-cohestion) (* speed-factor (f (:velocity boid))))) [first second])
        final-position (into [] ( map #(mod % max-position) flewn-pos))
        final-speed (into [] aligned-speed)]
    {:position final-position :velocity aligned-speed}))


(defn main-behave
  []
  (loop []
    (dorun (map #(send % behave) @boids))
    (Thread/sleep sleep-time)
    (recur)))

(def behaver (agent nil))

(defn make-them-fly [x]
  (when @running
    (send-off *agent* make-them-fly))
  (dorun (map #(send % behave) @boids))
  (Thread/sleep sleep-time)
  nil)




;; grafika

(defn setup []
  (q/smooth)                          
  (q/frame-rate 30)                    
  (q/background 255)
  (q/fill 225 0 0))

(def scale-forward 7)
(def scale-side 3)
(defn draw []
  (q/background 255)  
  (dorun (map (fn [refd-boid]
                 (let [boid @refd-boid
                       pos (:position boid)
                       vel (:velocity boid)
                       [x y] pos
                       [vx vy] vel
                       vel-lenght (Math/sqrt (+ (* vx vx) (* vy vy)))
                       nx (/ vx vel-lenght)
                       ny  (/ vy vel-lenght)]
                   (q/triangle (+ x (* scale-forward nx)) (+ y (* scale-forward ny))
                               (+ x (* scale-side ny)) (- y (* scale-side nx))
                               (- x (* scale-side ny)) (+ y (* scale-side nx)))))
              @boids)))


(q/defsketch boids-sketch                  
  :title "Boids"
  :setup setup
  :draw draw
  :size [(* scale max-position) (* scale max-position)])

;; pomoc

(defn setup-boids [ammount-of-boids]
  (reset! boids (make-boids ammount-of-boids)))

(defn off []
  (reset! running false))

(defn on []
  (reset! running true)
  (send behaver make-them-fly))



