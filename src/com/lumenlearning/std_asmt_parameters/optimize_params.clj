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

(ns com.lumenlearning.std-asmt-parameters.optimize-params
  (:require [com.lumenlearning.std-asmt-parameters.gradient-descent :as gd])
  (:require [com.lumenlearning.util.numeric :as u]))

(defn optimize-params-iter
  "Update the appropriate student and assessment parameters once for each data point."
  [student-params    ; hash-map - student parameters
   assessment-params ; hash-map - assessment parameters
   ord               ; Vector - order to iterate through data
   student-ids       ; Vector - student id column of data set
   assessment-ids    ; Vector - assessment id column of data set
   inputs            ; Vector - input column of data set
   outputs           ; Vector - output column of data set
   eta               ; Scalar - learning rate
   ]

  ;; We'll be updating ord, student-params, and assessment-params each time
  (loop [ord ord
         student-params student-params
         assessment-params assessment-params]
    ;; Get the next index to process
    (let [data-idx (first ord)]
      (if (nil? data-idx)
        ;; If we don't have any more data to process, return the new
        ;; parameters.
        [student-params assessment-params]
        ;; Otherwise, update the parameters with the next data item
        (let [input (nth inputs data-idx)
              target (nth outputs data-idx)
              sp-key (nth student-ids data-idx)
              ap-key (nth assessment-ids data-idx)
              sp (get student-params sp-key)
              ap (get assessment-params ap-key)
              y (* input sp ap)
              new-params (gd/update-params input sp ap target y eta)]
          (recur
           (next ord)
           (assoc student-params sp-key (nth new-params 0))
           (assoc assessment-params ap-key (nth new-params 1))))))))

(defn optimize-params
  [student-ids       ; Vector - student id column of data set
   assessment-ids    ; Vector - assessment id column of data set
   inputs            ; Vector - input column of data set
   outputs           ; Vector - output column of data set
   eta               ; Scalar - learning rate
   max-iters         ; Scalar - stop if we've iterated this many times
   precision         ; Scalar - stop if parameters haven't changed
                     ;          at least this much.
   ]

  ;; Check to make sure that student-ids, assessment-ids, inputs, and
  ;; outputs vectors are all the same length.
  (if (not (= (count student-ids)
              (count assessment-ids)
              (count inputs)
              (count outputs)))
    (throw (Exception. "Data vectors student-ids, assessment-ids, inputs, and outputs must all be the same length!")))

  (let [data-count (count student-ids)
        uniq-students (set student-ids)
        uniq-assessments (set assessment-ids)
        student-count (count uniq-students)
        assessment-count (count uniq-assessments)]
    (loop [i                 0

           ;; Start out with random values between 0 and 1
           ;; for both student and assessment parameters.
           student-params    (apply hash-map
                                    (interleave uniq-students
                                                (u/n-rand student-count)))
           assessment-params (apply hash-map
                                    (interleave uniq-assessments
                                                (u/n-rand assessment-count)))

           ;; Initial value for parameter change is maximum
           ;; value able to be represented by a Double.
           max-param-diff    (. Double MAX_VALUE)]

      ;; If we've hit max number of iterations or have acheived
      ;; sufficient precision, then stop and return the parameters
      ;; as they currently stand.
      (if (or (= i max-iters) (< max-param-diff precision))
        [student-params assessment-params]
        (let [new-params (optimize-params-iter student-params
                                               assessment-params
                                               (shuffle (range data-count))
                                               student-ids
                                               assessment-ids
                                               inputs
                                               outputs
                                               eta)
              new-std-params (nth new-params 0)
              new-asmt-params (nth new-params 1)

              ;; This works, but is ugly.  Consider breaking out into
              ;; its own function.
              max-diff (apply max
                              (concat (map u/abs
                                           (map -
                                                (map new-std-params
                                                     uniq-students)
                                                (map student-params
                                                     uniq-students)))
                                      (map u/abs
                                           (map -
                                                (map new-asmt-params
                                                     uniq-assessments)
                                                (map assessment-params
                                                     uniq-assessments)))))]
          (recur (inc i)
                 new-std-params
                 new-asmt-params
                 max-diff))))))
