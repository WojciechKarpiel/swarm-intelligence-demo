#Demonstration of swarm intelligence algorithms

## [Boids](https://en.wikipedia.org/wiki/Boids)

Load file boids.clj. You shoud see a new window containing red triangles. In repl:

```lisp
(on) ; make them fly
(off) ; pause
(setup-boids 20) ; 20 random boids
(on)
(setup-boids 24) ; you don't have to pause animation to swap boids
```

## [ACO (Ant Colony Optimization)](https://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms)

Load file aco.clj. You should see a new window containing randomly generated graph. In repl:

```lisp
;; if you don't like the graph, you can
(reset-edges 2) ; 2 is minimal number of edges adjacent to each vertex
;; if it's still wrong
(setup-vertexes 10) ; oprional second argument is parameter passed to reset-edges
;; if you don't like nest or food location
(get-positions-list)
(set-food [x y])
(set-nest [x y])
(go-find-food 50 100000) ; 50 ants will make 100000 passes through graph updating it after each iteration.
;; note that edge colours are scalled dynamically (linear regard to the most-pheromone-stacked edge).
;;if you want to see it again
(reset-pheromones)
```