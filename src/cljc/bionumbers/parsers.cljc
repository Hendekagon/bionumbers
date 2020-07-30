(ns bionumbers.parsers
  "Functions for parsing strings to useful data"
  (:require [clojure.string :as string]))

(defn ^{:patterns [#"\d+\s*-\s*\d+" #"(\d+,?)+\d+\s*-\s*(\d+,?)+\d+"]}
  int-range [s]
  (let [[l u] (map read-string (string/split (string/replace s #"," "") #"\s*-\s*"))]
    [l u]))

(defn ^{:patterns [#">\d+" #">(\d+,?)+"]}
  >range [s]
  (let [[_ u] (string/split (string/replace s #"," "") #">")]
    [(read-string u) Double/POSITIVE_INFINITY]))

(defn ^{:patterns [#"<\d+" #"<(\d+,?)+"]}
  <range [s]
  (let [[_ u] (string/split (string/replace s #"," "") #"<")]
    [Double/NEGATIVE_INFINITY (read-string u)]))

(defn ^{:patterns [#"\+/-\s*\d+.?\d*" #"±\s*\d+\.?\d*"]}
  ±double [s]
  (let [[_ vs] (string/split s #"\+/-\s*|±\s*") v (read-string vs)]
    [(* -1 v) v]))

(defn ^{:patterns [#"\d+" #"\d+\.?\d*" #"\d+\.\d+[eE]-?\d+"]}
  a-number [s]
  (let [v (read-string s)]
   [v v]))

(defn ^{:patterns [#"~\s*\d+" #"~\s*\d+\.\d+" #"~\s*\d+\.\d+[eE]-?\d+"]}
  approx [s]
  (let [[_ vs] (string/split s #"~\s*") v (read-string vs)]
   [v v]))

(defn ^{:patterns [#"\d+\.?\d+\s*-\s*\d+\.?\d+"]}
  double-range [s]
  (let [[l u] (map read-string (string/split s #"\s*-\s*"))]
   [l u]))

(defn
  ^{:patterns [#"\d+\.\d+[eE]-?\d+\s+-\s+\d+[eE]-?\d+"
               #"\d+[eE]-?\d+\s+-\s+\d+\.\d+[eE]-?\d+"
               #"\d+\.\d+[eE]-?\d+\s+-\s+\d+\.\d+[eE]-?\d+"]}
  exp-range [s]
  (let [[l u] (map read-string (string/split s #"\s+-\s+"))]
    [l u]))

(defn ^{:patterns [#"\d+\s*-\s*\d+×\d+\^\d+"]}
  pow-range [s]
  (let [[l u e] (map read-string (string/split (string/replace (string/replace s #"," "") #"\^" "e") #"\s*-\s*|×"))]
    [l u e]))

(defn ^{:patterns [#"\d+\.\d+\s*-\s*\d+\.\d+[eE]-?\d+"]}
  double-e-range [s]
  (let [[l u] (map read-string (string/split s #"\s*[^e^E]-\s*"))]
   [l u]))

(defn ^{:patterns [#"\d+[eE]-?\d+\s*to\s*\d+[eE]-?\d+"]}
  to-range [s]
  (let [[l u] (map read-string (string/split s #"\s*to\s*"))]
   [l u]))