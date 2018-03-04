(ns pettomato.datalog-test
  (:require
   [clojure.test :refer :all]
   [pettomato.datalog :refer :all]))

(def records
  '[{:id Uranus    :gender male}
    {:id Gaia      :gender female}

    {:id Oceanus    :gender male   :father Uranus  :mother Gaia}
    {:id Tethys     :gender female :father Uranus  :mother Gaia}
    {:id Iapetus    :gender male   :father Uranus  :mother Gaia}
    {:id Coeus      :gender male   :father Uranus  :mother Gaia}
    {:id Phoebe     :gender female :father Uranus  :mother Gaia}
    {:id Cronus     :gender male   :father Uranus  :mother Gaia}
    {:id Rhea       :gender female :father Uranus  :mother Gaia}
    {:id Themis     :gender female :father Uranus  :mother Gaia}
    {:id Mnemosyne  :gender female :father Uranus  :mother Gaia}
    {:id Hyperion   :gender male   :father Uranus  :mother Gaia}
    {:id Theia      :gender female :father Uranus  :mother Gaia}
    {:id Crius      :gender male   :father Uranus  :mother Gaia}
    {:id Atlas      :gender male   :father Iapetus :mother Clymene}
    {:id Prometheus :gender male   :father Iapetus :mother Clymene}
    {:id Epimetheus :gender male   :father Iapetus :mother Clymene}
    {:id Monoetius  :gender male   :father Iapetus :mother Clymene}
    {:id Leto       :gender female :father Coeus   :mother Phoebe}
    {:id Asteria    :gender female :father Coeus   :mother Phoebe}

    {:id Clymene :gender female :father Oceanus :mother Tethys}
    {:id Pleione :gender female :father Oceanus :mother Tethys}

    {:id Maia :gender female :father Atlas :mother Pleione}

    {:id Zeus       :gender male   :father Cronus :mother Rhea}
    {:id Hera       :gender female :father Cronus :mother Rhea}
    {:id Poseidon   :gender male   :father Cronus :mother Rhea}
    {:id Hestia     :gender female :father Cronus :mother Rhea}
    {:id Hades      :gender male   :father Cronus :mother Rhea}
    {:id Demeter    :gender female :father Cronus :mother Rhea}
    {:id Dionysus   :gender male   :father Zeus   :mother Semele}
    {:id Hermes     :gender male   :father Zeus   :mother Maia}
    {:id Apollo     :gender male   :father Zeus   :mother Leto}
    {:id Artemis    :gender female :father Zeus   :mother Leto}
    {:id Athena     :gender female :father Zeus}
    {:id Ares       :gender male   :father Zeus   :mother Hera}
    {:id Hephaestus :gender male   :father Zeus   :mother Hera}

    {:id Semele :gender female :father Cadmus :mother Harmonia}])

(def db (insert empty-db records))

(def rules '[[[?x :parent ?y] [?x :mother ?y]]
             [[?x :parent ?y] [?x :father ?y]]

             [[?x :child ?y] [?y :parent ?x]]

             [[?x :grandparent ?y] [?x :parent ?z] [?z :parent ?y]]

             [[?x :ancestor ?y] [?x :parent ?y]]
             [[?x :ancestor ?y] [?x :parent ?z] [?z :ancestor ?y]] ;; Recursive

             ;; Needs disequality: [not= ?x ?y]
             #_[[?x :sibling ?y] [?z :parent ?x] [?z :parent ?y]]])

(defn naive-query [db rule* goal*]
  (-> db
      (naive-method rule*)
      (query* goal*)))

(defn semi-naive-query [db rule* goal*]
  (-> db
      (semi-naive-method rule*)
      (query* goal*)))

(deftest basic-tests

  (testing "All facts."
    (is (= (naive-method db rules)
           (semi-naive-method db rules))))

  (testing "All Zeus's sons."
    (let [goal* '[[Zeus :child ?son]
                  [?son :gender male]]]
      (is (= (set (naive-query db rules goal*))
             (set (semi-naive-query db rules goal*))
             (set (bc-query db rules goal*))
             '#{{?son Apollo} {?son Hephaestus} {?son Dionysus} {?son Ares} {?son Hermes}}))))

  (testing "All grandparents and grandchildren."
    (let [goal* '[[?grandchild :grandparent ?grandparent]]]
      (is (= (set (naive-query db rules goal*))
             (set (semi-naive-query db rules goal*))
             (set (bc-query db rules goal*))
             '#{{?grandchild Hera, ?grandparent Gaia} {?grandchild Apollo, ?grandparent Coeus} {?grandchild Athena, ?grandparent Cronus} {?grandchild Atlas, ?grandparent Uranus} {?grandchild Maia, ?grandparent Oceanus} {?grandchild Poseidon, ?grandparent Uranus} {?grandchild Asteria, ?grandparent Uranus} {?grandchild Hades, ?grandparent Gaia} {?grandchild Demeter, ?grandparent Gaia} {?grandchild Maia, ?grandparent Clymene} {?grandchild Epimetheus, ?grandparent Oceanus} {?grandchild Hera, ?grandparent Uranus} {?grandchild Artemis, ?grandparent Rhea} {?grandchild Ares, ?grandparent Rhea} {?grandchild Apollo, ?grandparent Rhea} {?grandchild Prometheus, ?grandparent Tethys} {?grandchild Dionysus, ?grandparent Rhea} {?grandchild Monoetius, ?grandparent Oceanus} {?grandchild Clymene, ?grandparent Uranus} {?grandchild Atlas, ?grandparent Gaia} {?grandchild Atlas, ?grandparent Oceanus} {?grandchild Maia, ?grandparent Iapetus} {?grandchild Atlas, ?grandparent Tethys} {?grandchild Artemis, ?grandparent Phoebe} {?grandchild Demeter, ?grandparent Uranus} {?grandchild Dionysus, ?grandparent Cronus} {?grandchild Artemis, ?grandparent Coeus} {?grandchild Hestia, ?grandparent Uranus} {?grandchild Prometheus, ?grandparent Uranus} {?grandchild Zeus, ?grandparent Gaia} {?grandchild Clymene, ?grandparent Gaia} {?grandchild Dionysus, ?grandparent Cadmus} {?grandchild Pleione, ?grandparent Gaia} {?grandchild Monoetius, ?grandparent Uranus} {?grandchild Pleione, ?grandparent Uranus} {?grandchild Zeus, ?grandparent Uranus} {?grandchild Hephaestus, ?grandparent Rhea} {?grandchild Hermes, ?grandparent Atlas} {?grandchild Leto, ?grandparent Uranus} {?grandchild Epimetheus, ?grandparent Tethys} {?grandchild Hephaestus, ?grandparent Cronus} {?grandchild Prometheus, ?grandparent Gaia} {?grandchild Prometheus, ?grandparent Oceanus} {?grandchild Hades, ?grandparent Uranus} {?grandchild Monoetius, ?grandparent Gaia} {?grandchild Monoetius, ?grandparent Tethys} {?grandchild Dionysus, ?grandparent Harmonia} {?grandchild Poseidon, ?grandparent Gaia} {?grandchild Hermes, ?grandparent Cronus} {?grandchild Hestia, ?grandparent Gaia} {?grandchild Hermes, ?grandparent Rhea} {?grandchild Epimetheus, ?grandparent Uranus} {?grandchild Hermes, ?grandparent Pleione} {?grandchild Ares, ?grandparent Cronus} {?grandchild Epimetheus, ?grandparent Gaia} {?grandchild Maia, ?grandparent Tethys} {?grandchild Leto, ?grandparent Gaia} {?grandchild Artemis, ?grandparent Cronus} {?grandchild Athena, ?grandparent Rhea} {?grandchild Apollo, ?grandparent Cronus} {?grandchild Apollo, ?grandparent Phoebe} {?grandchild Asteria, ?grandparent Gaia}}))))

  (testing "All Athena's ancestors."
    (let [goal* '[[Athena :ancestor ?ancestor]]]
      (is (= (set (naive-query db rules goal*))
             (set (semi-naive-query db rules goal*))
             (set (bc-query db rules goal*))
             '#{{?ancestor Zeus} {?ancestor Uranus} {?ancestor Cronus} {?ancestor Gaia} {?ancestor Rhea}})))))
