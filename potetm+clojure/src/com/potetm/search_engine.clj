(ns com.potetm.search-engine
  (:require [clojure.string :as str])
  (:import [java.util Scanner]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files
                          FileSystem
                          FileSystems]))

(defn title [^String s]
  (subs s 0 (.indexOf s (int \newline))))

(defn normalize [^String s]
  (.toLowerCase s))

(defn tokenize-and-rank [^String s]
  (frequencies (re-seq #"[0-9a-z]+"
                       (normalize s))))

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
          (pmap (fn [i {:keys [::filename ::file-contents]}]
                  {::doc-id i
                   ::filename filename
                   ::title (title file-contents)
                   ::tokens-and-rank (tokenize-and-rank file-contents)})
                (range)
                files)))

(defn read-files [^FileSystem fs ^String search-dir]
  (let [path (.getPath fs search-dir (make-array String 0))]
    (map (fn [p]
           {::filename (str p)
            ::file-contents (String. (Files/readAllBytes p)
                                     StandardCharsets/UTF_8)})
         (Files/newDirectoryStream path))))


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
                (get inverted-index
                     (normalize word)))))

(defn -main [& [search-dir]]
  (let [fs (FileSystems/getDefault)
        scanner (Scanner. System/in)
        wrap-build-index (fn []
                           (println "building index...")
                           (let [t (System/currentTimeMillis)
                                 idx (build-index (read-files fs
                                                              search-dir))]
                             (println (str "finished building index, took: "
                                           (- (System/currentTimeMillis) t)
                                           " msecs"))
                             idx))
        format-results (fn [results]
                         (str/join "\n"
                                   (take 10
                                         (map (fn [{:keys [::filename ::title ::rank]}]
                                                (str "    " filename " - " title " - " rank))
                                              results))))
        index (wrap-build-index)]
    (while true
      (print "> ")
      (flush)
      (let [word (.next scanner)]
        (println (format-results (search index word)))))))
