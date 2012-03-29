(ns clj-graph.test
  (:use clojure.test
        clj-graph)
  (:import [clj_graph DirectedGraph]))

(def empty-graph (DirectedGraph. #{} {}))

(def test-graph-1
     (DirectedGraph.
             #{:a :b :c :d :e}
             {:a #{:b :c}
              :b #{:a :c}
              :c #{:d :e}
              :d #{:a :b}
              :e #{:d}}))

(deftest test-reverse-graph
  (is (= (reverse-graph test-graph-1)
         (DirectedGraph.
                 #{:a :b :c :d :e}
                 {:c #{:b :a}
                  :e #{:c}
                  :d #{:c :e}
                  :b #{:d :a}
                  :a #{:d :b}})))
  (is (= (reverse-graph (reverse-graph test-graph-1))
         test-graph-1))
  (is (= (reverse-graph empty-graph) empty-graph)))

(deftest test-add-loops
  (let [tg1 (add-loops test-graph-1)]
    (is (every? (fn [n] (contains? (neighbors tg1 n) n)) (nodes tg1))))
  (is (= (add-loops empty-graph) empty-graph)))

(deftest test-remove-loops
  (let [tg1 (remove-loops (add-loops test-graph-1))]
    (is (not-any? (fn [n] (contains? (neighbors tg1 n) n)) (nodes tg1))))
  (is (= (remove-loops empty-graph) empty-graph)))


(def test-graph-2
     (DirectedGraph.
             #{:a :b :c :d :e :f :g :h :i :j}
             {:a #{:b :c}
              :b #{:a :c}
              :c #{:d :e}
              :d #{:a :b}
              :e #{:d}
              :f #{:f}
              :g #{:a :f}
              :h #{}
              :i #{:j}
              :j #{:i}}))


(deftest test-lazy-walk
  (is (= (lazy-walk test-graph-2 :h) [:h]))
  (is (= (lazy-walk test-graph-2 :j) [:j :i])))

(deftest test-transitive-closure
  (let [tc-1 (transitive-closure test-graph-1)
        tc-2 (transitive-closure test-graph-2)
        get (fn [n] (set (neighbors tc-2 n)))]
    (is (every? #(= #{:a :b :c :d :e} (set %))
                (map (partial neighbors tc-1) (nodes tc-1))))
    (is (= (get :a) #{:a :b :c :d :e}))
    (is (= (get :h) #{}))
    (is (= (get :j) #{:i :j}))
    (is (= (get :g) #{:a :b :c :d :e :f}))))


(deftest test-post-ordered-nodes
  (is (= (set (post-ordered-nodes test-graph-2))
         #{:a :b :c :d :e :f :g :h :i :j}))
  (is (empty? (post-ordered-nodes empty-graph))))


(deftest test-scc
  (is (= (set (scc test-graph-2))
         #{#{:h} #{:g} #{:i :j} #{:b :c :a :d :e} #{:f}}))
  (is (empty? (scc empty-graph))))

(deftest test-component-graph
  (let [cg (component-graph test-graph-2)
        ecg (component-graph empty-graph)]
    (is (= (nodes cg) (set (scc test-graph-2))))
    (is (= (neighbors cg #{:a :b :c :d :e})
           #{#{:a :b :c :d :e}}))
    (is (= (neighbors cg #{:g})
           #{#{:a :b :c :d :e} #{:f}}))
    (is (= (neighbors cg #{:i :j})
           #{#{:i :j}}))
    (is (= (neighbors cg #{:h})
           #{}))
    (is (= (apply max (map count (self-recursive-sets cg))) 1))
    (is (= ecg empty-graph))))


(deftest test-recursive-component?
  (let [sccs (scc test-graph-2)]
    (is (= (set (filter (partial recursive-component? test-graph-2) sccs))
           #{#{:i :j} #{:b :c :a :d :e} #{:f}}))))


(deftest test-self-recursive-sets
  (is (= (set (self-recursive-sets test-graph-2))
         (set (filter
               (partial recursive-component? test-graph-2)
               (scc test-graph-2)))))
  (is (empty? (self-recursive-sets empty-graph))))


(def test-graph-3
     (DirectedGraph.
             #{:a :b :c :d :e :f}
             {:a #{:b}
              :b #{:c}
              :c #{:d}
              :d #{:e}
              :e #{:f}
              :f #{}}))

(def test-graph-4
     (DirectedGraph.
             #{:a :b :c :d :e :f :g :h}
             {:a #{}
              :b #{:a}
              :c #{:a}
              :d #{:a :b}
              :e #{:d :c}
              :f #{:e}
              :g #{:d}
              :h #{:f}}))

(def test-graph-5
     (DirectedGraph.
             #{:a :b :c :d :e :f :g :h}
             {:a #{}
              :b #{}
              :c #{:b}
              :d #{}
              :e #{}
              :f #{}
              :g #{:f}
              :h #{}}))

(deftest test-dependency-list
  (is (thrown-with-msg? Exception #".*Fixed point overflow.*"
                        (dependency-list test-graph-2)))
  (is (= (dependency-list test-graph-3)
         [#{:f} #{:e} #{:d} #{:c} #{:b} #{:a}]))
  (is (= (dependency-list test-graph-4)
         [#{:a} #{:b :c} #{:d} #{:g :e} #{:f} #{:h}]))
  (is (= (dependency-list test-graph-5)
         [#{:f :b :a :d :h :e} #{:g :c}]))
  (is (= (dependency-list empty-graph)
         [#{}])))

(deftest test-stratification-list
  (is (thrown-with-msg? Exception #".*Fixed point overflow.*"
                        (stratification-list test-graph-2 test-graph-2)))
  (is (= (stratification-list test-graph-4 test-graph-5)
         [#{:a} #{:b :c} #{:d} #{:e} #{:f :g} #{:h}]))
  (is (= (stratification-list empty-graph empty-graph)
         [#{}])))
