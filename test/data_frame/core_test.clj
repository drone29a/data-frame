(ns data-frame.core-test
  (:require [clojure.test :refer :all]
            [data-frame.core :refer :all]))

(deftest init-test
  (testing "Ensure created data frame agrees with initial args."
    (let [df (data-frame [:row-0 :row-1]
                         [:col-0 :col-1 :col-2]
                         [[42 1.23 "foo"]
                          [1  90.0 "bar"]])]
      (is (= 2 (row-count df)))
      (is (= 3 (col-count df)))
      (is (= [[42 1.23 "foo"]
              [1  90.0 "bar"]] (rows df)))
      (is (= [[42 1]
              [1.23 90.0]
              ["foo" "bar"]] (cols df) ))
      (is (= [:row-0 :row-1] (row-labels df)))
      (is (= [:col-0 :col-1 :col-2] (col-labels df)))))
  (testing "When the data shape doesn't match the sizes of row or column labels."
    (is (thrown-with-msg? Exception #"Number of rows"
                          (data-frame [:row-0]
                                      [:col-0 :col-1 :col-2]
                                      [[42 1.23 "foo"]
                                       [1  90.0 "bar"]])))
    (is (thrown-with-msg? Exception #"Number of columns"
                          (data-frame [:row-0 :row-1]
                                      [:col-0 :col-1]
                                      [[42 1.23 "foo"]
                                       [1  90.0 "bar"]])))))

(deftest read-test
  (testing "Access rows, columns, and elements by index."
    (let [df (data-frame [:row-0 :row-1]
                         [:col-0 :col-1 :col-2]
                         [[42 1.23 "foo"]
                          [1  90.0 "bar"]])]
      (is (= [42 1.23 "foo"]
             (get-row df 0)))
      (is (= [1.23 90.0]
             (get-col df 1)))
      (is (= "bar"
             (get-val df 1 2)))))
  (testing "Access rows, columns, and elements by label."
    (let [df (data-frame [:row-0 :row-1]
                         [:col-0 :col-1 :col-2]
                         [[42 1.23 "foo"]
                          [1  90.0 "bar"]])]
      (is (= [42 1.23 "foo"]
             (get-row df :row-0)))
      (is (= [1.23 90.0]
             (get-col df :col-1)))
      (is (= "bar"
             (get-val df :row-1 :col-2))))))
