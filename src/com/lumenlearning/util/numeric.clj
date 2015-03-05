;; std_asmt_parameters Source Code
;; Copyright (C) 2013 Lumen LLC. 
;; 
;; std_asmt_parameters is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; std_asmt_parameters is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public License
;; along with std_asmt_parameters. If not, see <http://www.gnu.org/licenses/>.

(ns com.lumenlearning.util.numeric)

(defn abs
  "return the absolute value of a number"
  [x]
  (if (< x 0) (* -1 x) x))

(defn floor
  "Return the integer portion of a floating point number."
  [x]
  (int x))

(defn ceil
  "Return the floating point rounded to the next highest integer."
  [x]
  (+ (floor x) 1))

(defn how-many
  "How many items in c return true when passed to predicate function pred?"
  [pred c]
  (count (filter pred c)))

(defn percent-true
  "Given a collection c of true/false values, return the percentage of items that are true. "
  [c]
  (/
   (how-many true? c)
   (count c)))

(defn one?
  "Is x equal to 1?"
  [x]
  (= 1 x))

(defn numify
  "Return 1 for truthy value, 0 for falsey value."
  [x]
  (if x 1 0))

(defn numify-coll
  "Numify each value in c."
  [c]
  (map numify c))

(defn sum
  "Assuming c is a collection of numbers, add them up."
  [c]
  (apply + c))

(defn mean
  "Assuming c is a collection of numbers, return the mean."
  [c]
  (/ (sum c) (count c)))

(defn n-rand
  "Return n random numbers between 0 and 1."
  [n]
  (vec (map rand (repeat n 1))))

(comment (defn parse-number [s]
            (if (re-find #"^-?\d+\.?\d*$" s)
              (read-string s))))

(defn parse-number [s] (read-string s))

(defn parse-number-seq [v]
  (vec (map parse-number v)))

(defn equal-groups-by-count
  "If we divide n items into grp-count groups to keep them as equally sized as possible, what will be the size of each group?"
  [n grp-count]
  ;; TODO: random result presents the group sizes in a random order
  ;; TODO: sorted result presents the group sizes in a sorted order
  ;; TODO: load-balanced result
  (let [ideal-size (/ n grp-count)
        small-size (floor ideal-size)
        large-size (ceil ideal-size)
        large-count (- n (* small-size grp-count))
        small-count (- grp-count large-count)]
    (concat (repeat small-count small-size) (repeat large-count large-size))))

(defn equal-groups-by-size
  ([n size] (equal-groups-by-size n size :smaller))
  ([n size size-pref]
     (cond
      (= size-pref :smaller) (equal-groups-by-count n (ceil (/ n size)))
      (= size-pref :larger) (equal-groups-by-count n (floor (/ n size)))
      true (throw (IllegalArgumentException. "If given, size-pref must be :smaller (prefer groups no larger than size) or :larger (prefer groups no smaller than size)")))))


(defn format-decimal [x decimals]
  (if (number? x)
    (format (str "%." decimals "f") x)
    x))

(defn format-percent
  ([x]
     (format-percent x 0))
  ([x decimals]
     (if (number? x)
       (str (format-decimal (* x 100) decimals) "%")
       x)))

(defn first-non-nil [s])

(defn prefer-non-nil
  "
  Expects all arguments to be sequences of the same length. Return a
  sequence populated from the argument sequences.  Prefer for input the
  first sequence to the second, the second to the third, etc. If the first
  sequence has a nil, try the second.  If the second is nil, try the third,
  etc.  If all input sequences have a nil value for a specific index, return
  sequence will have nil there also.
  "
  [& sequences]
  (loop [candidates (apply (partial map vector) sequences)
         result (vector)]
    (if (nil? candidates)
      result)
    (let [c (first candidates)]
      )))
