(ns data-frame.core
  (:require [schema.core :as s])
  (:import [clojure.lang ILookup]))

(s/defn index :- {s/Any s/Int}
  [xs :- [s/Any]]
  (->> xs
       (map-indexed (comp vec reverse vector))
       (into {})))

(s/defn idx-from :- s/Int
  [label-index :- {s/Keyword s/Int}
   idx-or-label :- (s/either s/Int s/Keyword)]
  (if (integer? idx-or-label)
    idx-or-label
    (label-index idx-or-label)))
(comment
  (defprotocol PLabeledVector
    (labels [this]
      "Returns labels.")
    (get-val [this idx-or-label]
      "Returns value corresponding to label (or index)."))

  (deftype BasicLabeledVector [labels
                               label-index
                               data]
    PLabeledVector
    (labels [this]
      labels)
    (get-val [this idx-or-label]
      (nth data (idx-from label-index idx-or-label)))
    ILookup
    (valAt [this key]
      (get-val this key))
    (valAt [this key not-found]
      (or (get-val this key)
          not-found)))

  (s/defn labeled-vector :- PLabeledVector
    [labels :- [s/Keyword]
     data :- [s/Any]]
    (BasicLabeledVector. labels (index labels) data)))

;;; Need a few basic operations which could be mapped to standard ops:
;;; - Modify existing row
;;; - Modify existing column
;;; - Modify existing cell
;;; - Add new row
;;; - Add new column

(defprotocol PDataFrame
  (rows [this]
    "Provides access to rows of the data frame.")
  (cols [this]
    "Provides access to columns of the data frame.")
  (row-count [this]
    "Returns the number of rows in the frame.")
  (col-count [this]
    "Returns the number of column in the frame.")
  (row-labels [this]
    "Returns the row labels.")
  (col-labels [this]
    "Returns the column labels.")
  (get-val [this row-idx-or-label col-idx-or-label]
    "Returns the value for the corresponding element.
    Rows and columns may be referenced by index (Integer)
    or label (Keyword).")
  (get-row [this row-idx-or-label]
    "Returns corresponding row.")
  (get-col [this col-idx-or-label]
    "Returns corresponding column."))

(deftype BasicDataFrame [row-labs
                         row-index
                         col-labs
                         col-index
                         cols]
  PDataFrame
  (rows [this]
    (for [row-idx (range (row-count this))]
      (get-row this row-idx)))
  (cols [this]
    cols)
  (row-count [this]
    (count (first cols)))
  (col-count [this]
    (count cols))
  (row-labels [this]
    row-labs)
  (col-labels [this]
    col-labs)
  (get-val [this row-idx-or-label col-idx-or-label]
    (let [c (get-col this col-idx-or-label)
          row-idx (idx-from row-index row-idx-or-label)]
      (nth c row-idx)))
  (get-col [this col-idx-or-label]
    (let [col-idx (idx-from col-index col-idx-or-label)]
      (nth cols col-idx)))
  (get-row [this row-idx-or-label]
    (let [row-idx (idx-from row-index row-idx-or-label)]
      (map #(nth % row-idx) cols))))

(s/defn data-frame :- PDataFrame
  [row-labs :- [s/Keyword]
   col-labs :- [s/Keyword]
   rows :- [[s/Any]]]

  ;; Check shape of data and keys match
  (when (not= (count row-labs)
            (count (distinct row-labs)))
    (throw (Exception. "Row keys are not distinct.")))

  (when (not= (count col-labs)
            (count (distinct col-labs)))
    (throw (Exception. "Column keys are not distinct.")))

  (when (not= (count rows) (count row-labs))
    (throw (Exception. "Number of rows doesn't match the number of row keys.")))
  
  (when (not (every? (comp (partial = (count col-labs)) count) rows))
    (throw (Exception. "Number of columns doesn't match the number of column keys.")))

  (let [num-rows (count rows)
        num-cols (count (first rows))
        row-index (into {} (index row-labs))
        col-index (into {} (index col-labs))
        cols (->> (for [col-idx (range num-cols)
                        row rows]
                    (clojure.core/get row col-idx))
                  (partition num-rows))]
    (BasicDataFrame. row-labs row-index col-labs col-index cols)))

