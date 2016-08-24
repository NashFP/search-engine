(ns com.potetm.search-engine
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.util Scanner]))

(defn title [^String s]
  (subs s 0 (.indexOf s (int \newline))))

(defn tokenize-and-rank [^String s]
  (frequencies (re-seq #"[0-9a-z]+"
                       (.toLowerCase s))))

(defn build-index [files]
  (reduce (fn [index {:keys [::doc-id ::filename ::title ::tokens-and-rank]}]
            (reduce-kv (fn [{:keys [::doc-id->display-info ::inverted-index]}
                            token
                            rank]
                         {::doc-id->display-info (assoc doc-id->display-info
                                                   doc-id {::filename filename
                                                           ::title title})
                          ::inverted-index (update inverted-index
                                                   token
                                                   merge
                                                   {doc-id rank})})
                       index
                       tokens-and-rank))
          {}
          ;; here there be parallelization
          (pmap (fn [i f]
                  (let [contents (slurp f)]
                    {::doc-id i
                     ::filename f
                     ::title (title contents)
                     ::tokens-and-rank (tokenize-and-rank contents)}))
                (range)
                files)))

(defn search [{:keys [::doc-id->display-info
                      ::inverted-index]}
              word]
  (map (fn [[doc-id rank]]
         (let [{:keys [::filename
                       ::title]} (doc-id->display-info doc-id)]
           {::filename filename
            ::title title
            ::rank rank}))
       (sort-by second >
                (get inverted-index (.toLowerCase word)))))

(defn -main [& [search-dir]]
  {:pre [(.isDirectory (io/file search-dir))]}
  (let [search-dir (io/file search-dir)
        scanner (Scanner. System/in)
        wrap-build-index (fn []
                           (println "building index...")
                           (let [t (System/currentTimeMillis)
                                 idx (build-index (map (partial io/file search-dir)
                                                       (.list search-dir)))]
                             (println (str "finished building index, took: "
                                           (- (System/currentTimeMillis) t)
                                           " msecs"))
                             idx))
        index (wrap-build-index)
        format-results (fn [results]
                         (str/join "\n"
                                   (take 10
                                         (map (fn [{:keys [::filename ::title ::rank]}]
                                                (str "    " filename " - " title " - " rank))
                                              results))))]
    (while true
      (print "> ")
      (flush)
      (let [word (.next scanner)]
        (println (format-results (search index word)))))))
