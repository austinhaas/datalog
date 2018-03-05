(ns pettomato.datalog
  "A simple, unoptimized, purely functional Datalog implementation.

  Facts (atoms) are Entity-Attribute-Value (EAV) triples.

  Rules are vectors of atoms. The first element is the head of the
  rule and the rest are the body.

  Naming convention: '*' suffix means plural.

  The docstring for unify-1 was adapted from:

    William E. Byrd
    Relational Programming in miniKanren: Techniques, Applications, and Implementations.
    Indiana University, Bloomington, IN,
    September 30, 2009.")

;;------------------------------------------------------------------------------
;; Logic programming

(defn lvar?
  "Returns true if x is a logic variable (lvar), otherwise returns
  false. A logic variable is a symbol that begins with '?'. Logic
  variables are only bound once!"
  [x]
  (and (symbol? x) (= (first (name x)) \?)))

(def empty-s
  "An empty substitution. A substitution is a map (of bindings) from lvars to values,
  which may be other lvars."
  {})

(defn walk
  "Returns the value of u with respect to substitution s."
  [s u]
  (if (contains? s u)
    (recur s (get s u))
    u))

(defn instantiate
  "Instantiate a template. Takes a substitution s and an atom a, and
  returns a new atom that results from substituting logic variables in
  a with their associated values in s."
  [s a]
  (mapv (fn [u] (walk s u)) a))

(defn unify-1
  "Unify two scalar terms u and v with respect to a substitution
  s. Returns a (potentially extended) substitution if unification
  succeeds, and returning nil if unification fails.

  Tells how to make two things equal.

  Examples:

    (unify-1 empty-s   1 2) => nil
    (unify-1 empty-s   1 1) => {}
    (unify-1 empty-s '?x 1) => {'?x 1}"
  [s u v]
  (let [u (walk s u)
        v (walk s v)]
    (cond
      (= u v)   s
      (lvar? u) (assoc s u v)
      (lvar? v) (assoc s v u)
      :else     nil)))

(defn unify-eav
  "Like unify-1, but the terms to be unified, u and v, are EAV triples.

  Example:

    (unify-eav empty-s '[?son :father Cronus] '[Zeus :father ?father])
    => {?son Zeus, ?father Cronus}"
  [s u v]
  (let [[e1 a1 v1] u
        [e2 a2 v2] v]
    (some-> s
      (unify-1 e1 e2)
      (unify-1 a1 a2)
      (unify-1 v1 v2))))

;;------------------------------------------------------------------------------
;; Database

(def empty-db #{})

(defn expand-record
  "When given a record in EAV format, returns a single element sequence
  containing that vector. This is essentially a no-op, but the result
  must always be a sequence.

  When given a map, the map must contain an :id key. The remaining map
  fields will be converted to EAV triples with the value associated to
  the :id key used as the entity.

  Examples:

    (expand-record '[Zeus :father Cronus])
    => [[Zeus :father Cronus]]

    (expand-record '{:id Zeus :father Cronus :mother Rhea})
    => [[Zeus :father Cronus] [Zeus :mother Rhea]]"
  [rec]
  (cond
    (map? rec) (let [e (get rec :id)
                     m (dissoc rec :id)]
                 (for [[a v] m]
                   [e a v]))
    :else      [rec]))

(defn insert
  "Takes a database db and a collection of records rec*. Returns a new
  database consisting of db with rec* inserted."
  [db rec*]
  (into db (mapcat expand-record rec*)))

(defn query
  "Takes a substitution s, a database db, and an atom goal (which may
  contain variables). Returns a sequence of substitutions, one for
  each way the goal unified with an element in the database.

  In a practical implementation, the db would be indexed."
  [s db goal]
  (keep #(unify-eav s goal %) db))

(defn query*
  "Like query, but takes a sequence of goals, representing a
  conjuction. The goals will be unified with the database with respect
  to each other. In other words, any variables in the goals range over
  the entire conjunction.

  Unlike query, the subsitution argument is optional. If not supplied,
  the empty substitution is used.

  Example:

    (let [s     {'?son 'Zeus}
          db    '[[Zeus :father Cronus] [Cronus :father Uranus]]
          goals '[[?son :father ?father] [?father :father ?grandfather]]]
      (query* s db goals))
    => [{?son Zeus, ?father Cronus, ?grandfather Uranus}]"
  ([db goal*] (query* empty-s db goal*))
  ([s db goal*]
   (if (empty? goal*)
     [s]
     (mapcat (fn [s] (query* s db (rest goal*)))
             (query s db (first goal*))))))

;;------------------------------------------------------------------------------
;; Forward evaluation (naive method)

(defn eval-rule
  "Returns every new fact that can be deduced from the given database
  and rule. The new facts are instantiations of the heads of any rules
  whose bodies can be satisfied by the database."
  [db rule]
  (let [[head & body] rule]
    (for [s (query* db body)]
      (instantiate s head))))

(defn naive-method
  "Repeatedly applies rules to database. Returns the fixpoint database
  that results when no new facts can be derived. Also known as the
  Gauss-Seidel method, forward chaining, and bottom up evaluation."
  [db rule*]
  (let [fact* (mapcat #(eval-rule db %) rule*)
        db'   (insert db fact*)]
    ;; Check for fixpoint.
    (if (= db db')
      db'
      (recur db' rule*))))

;;------------------------------------------------------------------------------
;; Forward evaluation (semi-naive method)

;; Ullman, Principles of Database and Knowledge-Base Systems Volume I, ch. 3

(defn generalized-query*
  "Like query*, but instead of taking a single db, takes a sequence db*,
  each of which is a subset of the others. Each goal in goal* is
  queried against a corresponding db in db*. The idea is to provide a
  constrained database for one of the goals."
  [s db* goal*]
  (assert (= (count db*) (count goal*)) "The number of db* and count* must be equal.")
  (if (empty? goal*)
    [s]
    (mapcat (fn [s] (generalized-query* s (rest db*) (rest goal*)))
            (query s (first db*) (first goal*)))))

(defn rotations
  "(rotations [1 2 3]) => ((1 2 3) (2 3 1) (3 1 2))"
  [coll]
  (let [n (count coll)]
    (->> (cycle coll)
         (iterate rest)
         (map #(take n %))
         (take n))))

(defn eval-rule-incr
  "Incremental rule evaluation. db-delta is a subset of db."
  [db db-delta rule]
  ;; Ullman, Principles of Database and Knowledge-Base Systems Volume I, p. 125
  (let [[head & body] rule
        n             (count body)
        ;; Make a seq of databases, one per goal. All are db except
        ;; one is db-delta.
        db*           (conj (repeat (dec n) db)
                            db-delta)
        ;; Get every permutation of the preceding seq of
        ;; databases. The only difference is which field gets
        ;; db-delta.
        db**          (rotations db*)]
    (distinct
     (for [db* db**
           s   (generalized-query* empty-s db* body)]
       (instantiate s head)))))

(defn semi-naive-method
  "Repeatedly applies rules to database. Returns the fixpoint database
  that results when no new facts can be derived.

  The main idea is that, after the first iteration of applying rules,
  the only rules that will generate new facts will be those that
  depend on at least one fact that was generated in the previous
  round."
  [db rule*]
  ;; Ullman, Principles of Database and Knowledge-Base Systems Volume I, p. 126
  (let [fact* (mapcat #(eval-rule db %) rule*)]
    (loop [db       db
           db-delta (insert empty-db fact*)]
      (let [db       (insert db db-delta)
            fact*    (mapcat #(eval-rule-incr db db-delta %) rule*)
            db-delta (insert empty-db fact*)]
        ;; Check for fixpoint.
        (if (empty? db-delta)
          db
          (recur db db-delta))))))

;;------------------------------------------------------------------------------
;; Backward chaining

(defn standardize-apart
  "Variables in a rule range over that rule only. An ?x in one rule has
  no relationship with an ?x in another rule. To avoid interference,
  rules must be standardized apart. Returns a rule that is equivalent
  to the input rule, but all of the lvars have been consistently
  replaced with new unique names."
  [rule]
  (let [f (memoize #(if (lvar? %) (gensym %) %))]
    (mapv #(mapv f %) rule)))

(defn find-lvars
  "Returns a sequence of all lvars occuring in a sequence of goals
  goal*."
  [goal*]
  (filter lvar? (flatten goal*)))

(defn minimize-substitution
  "Takes a substitution s and a sequence of lvars lvar* and returns s
  minimized with respect to lvar*. In other words, the result will
  only have the keys lvar*."
  [s lvar*]
  (zipmap lvar*
          (map #(walk s %) lvar*)))

(defn bc-query
  "Backwards-chaining query."
  [db rule* goal*]
  ;; Breadth-first search.
  (loop [open [[empty-s goal*]]
         s*   []]
    (if (empty? open)
      ;; Done. Return results.
      (let [lvar* (find-lvars goal*)]
        (->> s*
             (map (fn [s] (minimize-substitution s lvar*)))
             distinct))
      ;; Continue.
      (let [[s goal*] (first open)]
        (if (empty? goal*)
          ;; No more goals. Successful branch. Save the substitution.
          (recur (rest open) (conj s* s))
          ;; Try to resolve the next goal.
          (let [goal  (first goal*)
                ;; Resolve goal with elements in the database.
                open1 (keep (fn [fact]
                              (when-let [s' (unify-eav s goal fact)]
                                [s' (rest goal*)]))
                            db)
                ;; Resolve goal with rules.
                open2 (keep (fn [[head & body]]
                              (when-let [s' (unify-eav s goal head)]
                                [s' (concat (rest goal*) body)]))
                            (map standardize-apart rule*))]
            (recur (concat (rest open) open1 open2)
                   s*)))))))
