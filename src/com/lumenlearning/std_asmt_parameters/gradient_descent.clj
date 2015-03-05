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

(ns com.lumenlearning.std-asmt-parameters.gradient-descent)

;; These functions are based on simple, modified neural network with
;; only one input, one hidden node, and one output node.  The nodes
;; have a linear activation function (y = x).

(defn- error-y
  "Calculate the error at the y node."
  [target y]
  (* -1 (- target y)))

(defn- error-h
  "Calculate the error at the h node."
  [hy y-err]
  (* hy y-err))

(defn- new-hy
  "Calculate an update for the current-hy parameter."
  [current-hy eta y-err h]
  (- current-hy (* eta y-err h)))

(defn- new-ih
  "Calculate an update for the current-ih parameter."
  [current-ih eta h-err input]
  (- current-ih (* eta h-err input)))

(defn update-params
  "
  Calculate updated values for the ih and hy parameters given one training
  instance.
  "
  [input ih hy target y eta]
  (let [h (* input ih)
        y (* h hy)
        y-err (error-y target y)
        h-err (error-h hy y-err)]
    
    [(new-ih ih eta h-err input)
     (new-hy hy eta y-err h)]))
