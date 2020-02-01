(ns bionumbers.parse
  "
    Functions for parsing & converting
    Bionumbers HTML table to data
  "
  (:require
    [clojure.string :as string]
    [hickory.core :as h]
    [bionumbers.parsers :as p]))


(defn default-patterns []
  [
   {:name :int-range
    :patterns [#"\d+\s*-\s*\d+" #"(\d+,?)+\d+\s*-\s*(\d+,?)+\d+"]
    :fn p/int-range}

   {:name :>range
    :patterns [#">\d+" #">(\d+,?)+"]
    :fn p/>range}

   {:name :<range
    :patterns [#"<\d+" #"<(\d+,?)+"]
    :fn p/<range}

   {:name :±double
    :patterns [#"\+/-\s*\d+.?\d*" #"±\s*\d+\.?\d*"]
    :fn p/±double}

   {:name :a-number
    :patterns [#"\d+" #"\d+\.?\d*" #"\d+\.\d+[eE]-?\d+"]
    :fn p/a-number}

   {:name :approx
    :patterns [#"~\s*\d+" #"~\s*\d+\.\d+" #"~\s*\d+\.\d+[eE]-?\d+"]
    :fn p/approx}

   {:name :double-range
    :patterns [#"\d+\.?\d+\s*-\s*\d+\.?\d+"]
    :fn p/double-range}

   {:name :e-range
    :patterns [#"\d+\.\d+[eE]-?\d+\s+-\s+\d+[eE]-?\d+"
               #"\d+[eE]-?\d+\s+-\s+\d+\.\d+[eE]-?\d+"
               #"\d+\.\d+[eE]-?\d+\s+-\s+\d+\.\d+[eE]-?\d+"]
    :fn p/exp-range}

   {:name :pow-range
    :patterns [#"\d+\s*-\s*\d+×\d+\^\d+"]
    :fn p/pow-range}

   {:name :double-e-range
    :patterns [#"\d+\.\d+\s*-\s*\d+\.\d+[eE]-?\d+"]
    :fn p/double-e-range}

   {:name :to-range
    :patterns [#"\d+[eE]-?\d+\s*to\s*\d+[eE]-?\d+"]
    :fn p/to-range}
   ])

(defn parser
  "
   Returns a function to parse strings
   matched by various regexs
   - the fn associated with the first
   regex to match the string is applied
  "
  ([]
    (parser (default-patterns)))
  ([pp]
   (fn
     ([] pp)
     ([s]
       (let [[_ {f :fn ps :patterns n :name}]
            (first
              (filter (fn [[r _ _]] (some identity r))
                (map
                  (fn [{ps :patterns f :fn n :name :as pf}]
                    [(mapcat (fn [p] (if s (re-matches p s) nil)) ps) pf]) pp)))]
        (if f
          (try (f s)
            (catch Exception e
              (throw (Exception.
                       (str "a problem with '" s "' matched with " ps " " n)
                       e))))
          s))))))

(defn with-parser [parser patterns-fns]
  (let [q (parser)] (parser (into q patterns-fns))))

(defn overlapping?
  "Are the given intervals overlapping ?"
  [[al au] [bl bu]]
  (or
    (and (> bl al) (< bl au))
    (and (> al bl) (< al bu))))

(defn convert-strs
  "
    Convert the given list of bionumbers
    maps' values from strings to useful values
    where possible
  "
  [bionumbers-clj]
  (let [rp (parser)]
    (map
      (fn [q]
        (-> q
          (update :range (fn [x] (-> x (string/replace #"'|," "") rp)))
          (update :units string/lower-case)
          (update :value (fn [x] (if (and x (not= x "")) (read-string x) x)))))
    bionumbers-clj)))

(defn row-strs [row]
  (map (comp string/trim last)
    (filter (fn [x] (and (vector? x) (= :td (first x)))) row)))

(defn html-clj
  "
    Convert the given html string
    to maps of column-header-based keywords
    to values
  "
  ([html-str]
   (html-clj html-str
     (-> html-str
      h/parse
      h/as-hiccup
      (->>
        (tree-seq (some-fn seq? list? vector?) seq)
        (filter
          (fn [x] (and (vector? x) (= (first x) :tr))))))))
   ([html [columns-html & rows-html]]
    (let [columns (map
                    (fn [s] (-> s string/lower-case
                               (string/replace #"\s+|_" "-")
                               keyword))
                    (row-strs columns-html))]
      (convert-strs
        (map
         (fn [row]
           (zipmap columns row))
         (map row-strs rows-html))))))

(defn overlapping-ranges [bionumbers-clj]
  (let [u (remove (comp (complement vector?) :range) bionumbers-clj)]
    (for [{xr :range xu :units :as x} u {yr :range yu :units :as y} u]
      (if (and (not= x y) (= xu yu) (overlapping? xr yr)) [x y]))))