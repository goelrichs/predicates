(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)));; => #'predicates/sum-f
(sum-f inc dec 4);; => 8
(sum-f inc identity 5);; => 11
(sum-f identity - 10);; => 0

(defn less-than [n]
  (fn [k] (< k n)));; => #'predicates/less-than

(defn equal-to [n]
  (fn [k] (== n k)));; => #'predicates/equal-to
(filter (less-than 3) [1 2 3 4 5]);; => (1 2)  
(filter (less-than 4) [-2 12 3 4 0]);; => (-2 3 0)
(filter (equal-to 2) [2 1 3 2.0]);; => (2 2.0)   
(filter (equal-to 2) [3 4 5 6]);; => ()

(def graphic-novels [{:name "Yotsuba 1" :series "Yotsuba"}
                     {:name "Yotsuba 5" :series "Yotsuba"}
                     {:name "Persepolis"}
                     {:name "That Yellow Bastard" :series "Sin City"}
                     {:name "The Hard Goodbye" :series "Sin City"}
                     {:name "Maus"}
                     {:name "Solanin"}
                     {:name "Monster 3" :series "Monster"}])
(filter :series graphic-novels)
;; => ({:name "Yotsuba 1", :series "Yotsuba"}
;;     {:name "Yotsuba 5", :series "Yotsuba"}
;;     {:name "That Yellow Bastard", :series "Sin City"}
;;     {:name "The Hard Goodbye", :series "Sin City"}
;;     {:name "Monster 3", :series "Monster"})
(:name graphic-novels);; => nil ....huh?

(defn key->predicate [a-key]
  (fn [a-map] (contains? a-map a-key)));; => #'predicates/key->predicate
(filter (key->predicate "Name") [{"Name" "Joe"} {"Blargh" 3}]);; => ({"Name" "Joe"})

(defn set->predicate [a-set]
  (fn [k]
    (cond
      (= k nil) true
      (= k false) true
      :else (a-set k))));; => #'predicates/set->predicate
(filter (set->predicate #{1 2 3}) [0 2 4 6]);; => (2)
(filter (set->predicate #{1 2 3 nil}) [2 0 nil 4 nil 6]);; => (2 nil nil)

(defn pred-and [pred1 pred2]
  (fn [k] (and (pred1 k) (pred2 k))));; => #'predicates/pred-and
(filter (pred-and pos? even?) [1 2 -4 0 6 7 -3]);; => (2 6)
(filterv (pred-and pos? odd?) [1 2 -4 0 6 7 -3]);; => (1 7)
(filterv (pred-and (complement nil?) empty?) [[] '() nil {} #{}]);; => ([] () {} #{})

(defn pred-or [pred1 pred2]
  (fn [k] (or (pred1 k) (pred2 k))));; => #'predicates/pred-or
(filter (pred-or pos? odd?) [1 2 -4 0 6 7 -3]);; => (1 2 6 7 -3)
(filter (pred-or pos? even?) [1 2 -4 0 6 7 -3]);; => (1 2 -4 0 6 7)

(defn whitespace? [character]
  (Character/isWhitespace character))
(whitespace? \ );; => true

(defn blank? [string]
  (every? whitespace? string));; => #'predicates/blank?
(blank? "gregory Oelrichs");; => false
(blank? " \t\n\t ");; => true
(blank? "  \t a");; => false  
(blank? "");; => true

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def kay {:name "Guy Gavriel Kay" :birth-year 1954})
(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

(def authors #{china, octavia, kay, dick, zelazny})

(def cities {:title "The City and the City" :authors #{china}
             :awards #{:locus, :world-fantasy, :hugo}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def lord-of-light {:title "Lord of Light", :authors #{zelazny}
                    :awards #{:hugo}})
(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})
(def ysabel {:title "Ysabel", :authors #{kay}, :awards #{:world-fantasy}})
(def scanner-darkly {:title "A Scanner Darkly" :authors #{dick}})

(def books #{cities, wild-seed, lord-of-light,
             deus-irae, ysabel, scanner-darkly})

(defn has-award? [book award]
  (if (= nil (:awards book))
    false
    (= award ((:awards book) award))));; => #'predicates/has-award?
(has-award? ysabel :world-fantasy);; => true
(has-award? cities :world-fantasy);; => true
(has-award? scanner-darkly :hugo);; => false

(defn HAS-ALL-THE-AWARDS? [book awards];book is a map, and awards is a set
  (every? (set->predicate (:awards book)) awards ))
(HAS-ALL-THE-AWARDS? cities #{:locus});; => true
(HAS-ALL-THE-AWARDS? cities #{:locus :world-fantasy :hugo});; => true
(HAS-ALL-THE-AWARDS? cities #{:locus :world-fantasy :hugo :pulitzer});; => false
(HAS-ALL-THE-AWARDS? lord-of-light #{:locus :world-fantasy :hugo});; => false
(HAS-ALL-THE-AWARDS? lord-of-light #{:hugo});; => true
(HAS-ALL-THE-AWARDS? scanner-darkly #{});; => true

(some whitespace? "Kekkonen");; => nil
(some whitespace? "Kekkonen Kekkonen");; => true
(some even? [1 2 3]);; => true                  
(some even? [1 3]);; => nil    online textbook says "=> false"

(defn my-some [pred a-seq]
  (if (not (empty? (filter nil? a-seq)))
    true
    (first (filter pred a-seq))));; => #'predicates/my-some
(my-some even? [1 3 5 7]);; => nil       ;=> falsey
(my-some even? [1 3 5 7 8]);; => 8     ;=> true
(my-some neg? [1 3 5 0 7 8]);; => nil    ;=> falsey
(my-some neg? [1 3 5 0 7 -1 8]);; => -1 ;=> true
(my-some neg? []);; => nil               ;=> falsey
(my-some first [[false] [1]]);; => [1]   ;=> 1
(my-some first [[false] []]);; => nil    ;=> falsey
(my-some nil? [1 2]);; => nil            ;=> falsey
(my-some nil? [1 nil 2]);; => true    
(= nil (first (filter nil? [1 2])));; => true
(first (filter nil? [1 nil 2]));; => nil
(first (filter nil? [1 2])) ;; => nil  didn't expect first to act this way!
;;need to remember that first returns nil if the list is () and if the list is (nil)
(filter nil? [1 2]);; => ()    
(filter nil? [1 nil 2]);; => (nil)
(first (filter nil? [1 nil 2]));; => nil
(= () (filter nil? [1 2]));; => true
(empty? (filter nil? [1 nil 2]));; => false

(defn my-every? [pred a-seq]
  (empty? (filter (fn [k] (= k false)) (map pred a-seq))));; => #'predicates/my-every?
(my-every? pos? [1 2 3 4])   ;=> true
(my-every? pos? [1 2 3 4 0]) ;=> false
(my-every? even? [2 4 6])    ;=> true
(my-every? even? [])         ;=> true
(filter (fn [k] (= false k)) (map pos? [1 2 3 4]));; => ()

(defn prime? [n]
  (let [pred (fn [k] (= 0 (mod n k))) ]
    (not (some pred (range 2 n)))))
(prime? 2);; => true
(prime? 3);; => true
(prime? 5);; => true
(prime? 4) ;=> false
(prime? 7) ;=> true
(prime? 10) ;=> false
(filter prime? (range 2 50)) ;=> (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
(filter prime? (range 2 1000))
;^^
