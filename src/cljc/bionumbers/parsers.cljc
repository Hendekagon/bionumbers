(ns bionumbers.parsers
  "Functions for parsing strings to useful data"
  (:require [clojure.string :as string]))

(defn int-range [s]
  (let [[l u] (map read-string (string/split (string/replace s #"," "") #"\s*-\s*"))]
    [l u]))

(defn >range [s]
  (let [[_ u] (string/split (string/replace s #"," "") #">")]
    [(read-string u) Double/POSITIVE_INFINITY]))

(defn <range [s]
  (let [[_ u] (string/split (string/replace s #"," "") #"<")]
    [Double/NEGATIVE_INFINITY (read-string u)]))

(defn ±double [s]
  (let [[_ vs] (string/split s #"\+/-\s*|±\s*") v (read-string vs)]
    [(* -1 v) v]))

(defn a-number [s]
  (let [v (read-string s)]
   [v v]))

(defn approx [s]
  (let [[_ vs] (string/split s #"~\s*") v (read-string vs)]
   [v v]))

(defn double-range [s]
  (let [[l u] (map read-string (string/split s #"\s*-\s*"))]
   [l u]))

(defn exp-range [s]
  (let [[l u] (map read-string (string/split s #"\s+-\s+"))]
    [l u]))

(defn pow-range [s]
  (let [[l u e] (map read-string (string/split (string/replace (string/replace s #"," "") #"\^" "e") #"\s*-\s*|×"))]
    [l u e]))

(defn double-e-range [s]
  (let [[l u] (map read-string (string/split s #"\s*[^e^E]-\s*"))]
   [l u]))

(defn to-range [s]
  (let [[l u] (map read-string (string/split s #"\s*to\s*"))]
   [l u]))