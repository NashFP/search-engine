(ns com.potetm.search-engine-test
  (:require [com.potetm.search-engine :as se]
            [clojure.spec :as spec]
            [clojure.spec.gen :as sgen]
            [clojure.spec.test :as stest]
            [clojure.test.check.generators :as gen]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [nio2.core :as nio2])
  (:import (com.google.common.jimfs Jimfs Configuration)
           (java.nio.charset StandardCharsets)))

;; from https://gist.github.com/alexanderkiel/931387c7a86c1879b2267ad064067af7
(defmethod assert-expr 'check [msg form]
  (let [args (rest form)]
    `(let [result# (stest/check ~@args)]
       (if (some :failure result#)
         (do-report {:type :fail
                     :message ~msg
                     :expected :no-errors
                     :actual (map (comp ex-data :failure)
                                  result#)})
         (do-report {:type :pass
                     :message ~msg
                     :expected '~form
                     :actual result#}))
       result#)))

(defn read-files-gen []
  ;; todo: check unanticipated directory structure
  (gen/bind (sgen/tuple (gen/such-that (complement str/blank?) gen/string-alphanumeric)
                        (gen/list (sgen/tuple (gen/such-that (complement str/blank?) gen/string-alphanumeric)
                                              (gen/list gen/string-alphanumeric))))
            (fn [[search-dir files]]
              (let [fs (Jimfs/newFileSystem (Configuration/unix))
                    search-path (nio2/path fs search-dir)]
                (nio2/create-dir search-path)
                (doseq [[f contents] files]
                  (let [p (nio2/path search-path f)]
                    (nio2/create-file p)
                    (nio2/write-lines p contents StandardCharsets/UTF_8)))
                (gen/return [fs search-dir])))))

(deftest generated-tests
  (testing "generated-tests"
    (is (check [`se/title
                `se/normalize
                `se/tokenize-and-rank
                `se/build-index
                `se/search]))))

(deftest read-files
  (testing "read-files generated tests"
    (is (check `se/read-files
               {:clojure.spec.test.check/opts {:num-tests 100}
                :gen {::se/read-files-args read-files-gen}}))))
