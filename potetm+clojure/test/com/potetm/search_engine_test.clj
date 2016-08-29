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

(defn article-gen []
  (sgen/fmap (fn [[s1 s2]]
               (str s1 "\n" s2))
             (sgen/tuple (sgen/string)
                         (sgen/string))))

(defn token-gen []
  (sgen/fmap str/lower-case
             (sgen/string-alphanumeric)))

(defn search-args-gen []
  (let [gen-index (sgen/fmap (fn [index]
                               (let [doc-ids (set (mapcat (comp keys second)
                                                          index))]
                                 {::se/doc-id->attributes (if (seq doc-ids)
                                                            (sgen/generate
                                                              (spec/gen ::se/doc-id->attributes
                                                                        {::se/doc-id #(spec/gen doc-ids)}))
                                                            {})
                                  ::se/inverted-index index}))
                             (spec/gen ::se/inverted-index
                                       {::se/token token-gen}))]
    (sgen/fmap (fn [[index include-word? rando-word]]
                 (let [inverted-index-keys (set (keys (::se/inverted-index index)))]
                   [index (if (and include-word? (seq inverted-index-keys))
                            (sgen/generate (spec/gen inverted-index-keys))
                            rando-word)]))
               (sgen/tuple gen-index
                           (sgen/boolean)
                           (sgen/string-alphanumeric)))))

(defn file-gen []
  (gen/vector-distinct-by ::se/filename
                          (gen/fmap (fn [[filename article]]
                                      {::se/filename filename
                                       ::se/article article})
                                    (gen/tuple (gen/such-that (complement str/blank?)
                                                              gen/string-alphanumeric)
                                               (article-gen)))))

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

(deftest title-test
  (testing "title generated tests"
    (is (check `se/title
               {:gen {::se/article article-gen}}))))

(deftest normalize-test
  (testing "normalize generated tests"
    (is (check `se/normalize))))

(deftest tokenize-and-rank-test
  (testing "tokenize-and-rank generated tests"
    (is (check `se/tokenize-and-rank
               {:gen {::se/article article-gen}}))))

(deftest build-index-test
  (testing "build-index generated tests"
    (is (check `se/build-index
               {:gen {::se/build-index-args #(gen/vector (file-gen)
                                                         1)}}))))

(deftest search-test
  (testing "search generated tests"
    (is (check `se/search
               {:gen {::se/search-arguments search-args-gen}}))))

(deftest read-files
  (testing "read-files generated tests"
    (is (check `se/read-files
               {:clojure.spec.test.check/opts {:num-tests 100}
                :gen {::se/read-files-args read-files-gen}}))))
