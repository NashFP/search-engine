(ns com.potetm.search-engine
  (:require [clojure.string :as str]
            [clojure.spec :as spec]
            [clojure.spec.gen :as sgen])
  (:import (java.util Scanner)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files
                          FileSystem
                          FileSystems
                          LinkOption
                          Path)))

(spec/def ::title
  (spec/and string?
            #(not (.contains % "\n"))))

(spec/def ::article
  (spec/with-gen
    (spec/and string?
              #(.contains % "\n"))
    #(sgen/fmap (partial str/join "\n")
                (sgen/vector (sgen/string)))))

(spec/fdef title
  :args (spec/cat :article ::article)
  :ret ::title
  :fn #(.contains ^String (-> % :args :article)
                  ^String (-> % :ret)))

(defn title [^String s]
  (subs s 0 (.indexOf s (int \newline))))

(spec/fdef normalize
  :args (spec/cat :input string?)
  :ret (spec/spec string?)
  :fn #(and (= (-> % :args :input count)
               (-> % :ret count))
            (not (re-find #"[A-Z]" (:ret %)))))

(defn normalize [^String s]
  (str/lower-case s))

(spec/def ::token
  (spec/with-gen
    (spec/and string?
              (partial re-matches #"[a-z0-9]+"))
    #(sgen/fmap str/lower-case
                (sgen/string-alphanumeric))))

(spec/def ::rank
  (spec/and integer?
            pos?))

(spec/def ::tokens-and-rank
  (spec/map-of ::token
               ::rank
               :conform-keys true))

(spec/fdef tokenize-and-rank
  :args (spec/cat :article ::article)
  :ret ::tokens-and-rank
  :fn (fn [{:keys [args ret]}]
        (let [normal-art (normalize (:article args))]
          (every? (fn [[t r]]
                    (.contains ^String normal-art
                               ^String t))
                  ret))))

(defn tokenize-and-rank [^String s]
  (frequencies (re-seq #"[0-9a-z]+"
                       (normalize s))))

(spec/def ::filename
  (spec/and string?
            (complement str/blank?)))

(spec/def ::file
  (spec/keys :req
             [::filename
              ::article]))

(spec/def ::doc-id
  (spec/and integer?
            (complement neg?)))

(spec/def ::doc-attributes
  (spec/keys :req
             [::filename
              ::title]))

(spec/def ::doc-id->attributes
  (spec/map-of ::doc-id
               ::doc-attributes
               :conform-keys true))

(spec/def ::inverted-index
  (spec/map-of ::token
               (spec/map-of ::doc-id
                            ::rank
                            :conform-keys true)
               :conform-keys true))

(spec/def ::index
  (spec/with-gen
    (spec/keys :req
               [::doc-id->attributes
                ::inverted-index])
    (fn []
      (sgen/fmap (fn [index]
                   (let [doc-ids (set (mapcat (comp keys second)
                                              index))]
                     {::doc-id->attributes (if (seq doc-ids)
                                             (sgen/generate
                                               (spec/gen ::doc-id->attributes
                                                         {::doc-id #(sgen/elements doc-ids)}))
                                             {})
                      ::inverted-index index}))
                 (spec/gen ::inverted-index)))))

(spec/fdef build-index
  :args (spec/with-gen
          (spec/cat :files (spec/coll-of ::file))
          #(sgen/fmap (fn [filenames]
                        [(map (fn [f]
                                {::filename f
                                 ::article (sgen/generate (spec/gen ::article))})
                              filenames)])
                      (sgen/vector-distinct
                        (sgen/such-that (complement str/blank?)
                                        (sgen/string-alphanumeric)))))
  :ret ::index
  :fn (fn [{{:keys [files]} :args
            {:keys [::doc-id->attributes
                    ::inverted-index]} :ret}]
        (let [filename->article (into {} (map (juxt ::filename
                                                    (comp str/lower-case ::article))
                                              files))
              doc-id->filename (into {} (map (fn [[doc-id {:keys [::filename]}]]
                                               [doc-id filename])
                                             doc-id->attributes))]
          (every? (fn [[token doc-id->rank]]
                    (every? (fn [doc-id]
                              (.contains (filename->article (doc-id->filename doc-id))
                                         token))
                            (keys doc-id->rank)))
                  inverted-index))))

(defn build-index [files]
  (let [analyze-doc (fn [i {:keys [::filename ::article]}]
                      {::doc-id i
                       ::filename filename
                       ::title (title article)
                       ::tokens-and-rank (tokenize-and-rank article)})
        merge-doc-in-index (fn [index {:keys [::doc-id
                                              ::filename
                                              ::title
                                              ::tokens-and-rank]}]
                             (reduce-kv (fn [{:keys [::doc-id->attributes
                                                     ::inverted-index]}
                                             token
                                             rank]
                                          {::doc-id->attributes (assoc doc-id->attributes
                                                                  doc-id {::filename filename
                                                                          ::title title})
                                           ::inverted-index (update inverted-index
                                                                    token
                                                                    assoc
                                                                    doc-id rank)})
                                        index
                                        tokens-and-rank))]
    (reduce merge-doc-in-index
            {::doc-id->attributes {}
             ::inverted-index {}}
            ;; here there be parallelization
            (pmap analyze-doc
                  (range)
                  files))))

(defn ^Path path [^FileSystem fs ^String p & rest]
  (.getPath fs
            p
            (into-array String rest)))

(spec/def ::read-files-args
  (spec/and (spec/cat :fs #(instance? FileSystem %)
                      :search-dir string?)
            (fn [{:keys [fs search-dir]}]
              (Files/exists (path fs search-dir)
                            (make-array LinkOption 0)))
            (fn [{:keys [fs search-dir]}]
              (Files/isDirectory (path fs search-dir)
                                 (make-array LinkOption 0)))))

(spec/fdef read-files
  :args ::read-files-args
  :ret (spec/coll-of ::file)
  :fn (fn [{{:keys [fs search-dir]} :args
            files :ret}]
        (every? (fn [{:keys [::article ::filename]}]
                  (let [p (path fs filename)]
                    (and (.startsWith (path fs filename)
                                      ^String search-dir)
                         (= article (String. (Files/readAllBytes p)
                                             StandardCharsets/UTF_8)))))
                files)))

(defn read-files
  "Retrieve all of the files that appear to be valid articles
   from search-dir."
  [^FileSystem fs ^String search-dir]
  (let [search-path (path fs search-dir)]
    (keep (fn [p]
            (let [article (String. (Files/readAllBytes p)
                                   StandardCharsets/UTF_8)]
              (when (and (Files/isRegularFile p
                                              (make-array LinkOption 0))
                         (spec/valid? ::article article))
                {::filename (str p)
                 ::article article})))
          (iterator-seq (.iterator (Files/list search-path))))))

(spec/def ::search-result
  (spec/keys :req
             [::doc-id
              ::filename
              ::title
              ::rank]))

(spec/fdef search
  :args (spec/with-gen
          (spec/cat :index ::index
                    :word string?)
          #(sgen/fmap (fn [[index include-word? rando-word]]
                        (let [inverted-index-keys (keys (::inverted-index index))]
                          [index (if (and include-word? (seq inverted-index-keys))
                                   (sgen/generate (sgen/elements inverted-index-keys))
                                   rando-word)]))
                      (sgen/tuple (spec/gen ::index)
                                  (sgen/boolean)
                                  (sgen/string-alphanumeric))))
  :ret (spec/coll-of ::search-result)
  :fn (fn [{{{:keys [::inverted-index
                     ::doc-id->attributes]} :index
             word :word} :args
            search-results :ret}]
        (let [doc-id->rank (get inverted-index
                                (normalize word))]
          (and (every? (fn [{:keys [::doc-id
                                    ::filename
                                    ::rank
                                    ::title]}]
                         (and (doc-id->rank doc-id)
                              (= rank (doc-id->rank doc-id))
                              (= filename (get-in doc-id->attributes
                                                  [doc-id ::filename]))
                              (= title (get-in doc-id->attributes
                                               [doc-id ::title]))))
                       search-results)))))

(defn search
  "Search for your word in the index.

   Does not check for index inconsistencies.
   For example, if a doc-id exists in the ::inverted-index
   but not in ::doc-id->attributes, the doc is assumed to have
   been deleted, but the inverted index has not yet been updated."
  [{:keys [::doc-id->attributes
           ::inverted-index]}
   token]
  (keep (fn [[doc-id rank]]
          (let [{:keys [::filename
                        ::title]} (doc-id->attributes doc-id)]
            (when filename
              {::doc-id doc-id
               ::filename filename
               ::title title
               ::rank rank})))
        (sort-by second >
                 (get inverted-index
                      (normalize token)))))

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
