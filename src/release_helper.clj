(ns release-helper
  (:require [java-time :as time]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [selmer.parser :as parser]
            [selmer.util :refer [without-escaping]]
            [clojure.java.io :as io])
  (:import java.time.format.DateTimeFormatter))

;; Release dates are always on Monday of ISO week 10, 23, 36 and 49
(def release-weeks [10 23 36 49])

(defn to-date
  "Given a `year` and an ISO-8601 `week` return a localdate of the
  monday of that week"
  [week year]
  (time/local-date (DateTimeFormatter/ISO_WEEK_DATE) (format "%d-W%d-1" year week)))

(def release-dates
  "An infinite sequence of release dates, based on `release-weeks`"
  (map
   to-date
   (cycle release-weeks) ;; weeks
   (mapcat #(repeat (count release-weeks) %) (iterate inc 2019)))) ;; years


(defn next-release-date
  "Return the next release date which is *past* the given `date`. If no
  date is given it will return the next possible release date which is
  a week past the current date."
  ([]
   (next-release-date (time/plus (time/local-date) (time/weeks 1))))
  ([date]
   (first (drop-while #(time/after? (time/plus date (time/days 1)) %) release-dates))))

(defn changes
  "Extract the news blurb from the NEWS file"
  [news-file]
  (second (re-find #"(?sm)^(\* Noteworthy.*?)^\* Noteworthy" (slurp news-file))))

(defn extract-milestone-url [changes]
  (re-find #"(?sm)https://github.com/liblouis/liblouis(?:utdml)?/milestone/\d+\?closed=1" changes))

(defn extract-version [changes]
  (second(re-find #"(?sm)Noteworthy changes in release (\d+\.\d+\.\d+) \(" changes)))

(defn milestone-link-to-footnote [changes]
  ;; we assume that the template contains a footnote with the milestone link, so we
  ;; replace the link in the changes text with a footref
  (string/replace changes #"(?sm)\[\[https://github.com/liblouis/liblouis(?:utdml)?/milestone/\d+\?closed=1\]\[(.*)\]\]" "$1[fn:3]"))

(defn normalize-title [changes]
  (string/replace changes #"(?sm)^\* Noteworthy changes in release \d+\.\d+\.\d+ \(\d+-\d+-\d+\)" "* Noteworthy changes in this release"))

(defn drop-title [changes]
  (string/replace changes #"(?sm)^\* Noteworthy changes in release \d+\.\d+\.\d+ \(\d+-\d+-\d+\)" ""))

(defn inject-toc [changes]
  (string/replace changes #"(?sm)\(https://github.com/liblouis/liblouis/milestone/\d+\?closed=1\)\." "$0

* Will be replaced with the ToC
{:toc}
"))

(defn announcement
  "Given the `news` for a release, generate a text announcement that can
  be used in an email. The file will be placed in `target-path`."
  [project news target-path]
  (let [env {:version (extract-version news)
             :milestone-url (extract-milestone-url news)
             :next-release-date (time/format "MMMM d yyyy" (next-release-date))
             :changes (-> news milestone-link-to-footnote normalize-title)}]
    (shell/sh "pandoc" "--from=org" "--to=rst"
              (format "--output=%s" (.getPath target-path))
              :in (without-escaping
                   (parser/render-file (format "announcement-%s.org" project) env)))))

(defn news-post-name
  "Extract the version from a `news` and combine with todays date to
  generate a post name suitable for a Jekyll website at location
  `target-path`."
  [project news target-path]
  (let [version (extract-version news)
        iso-today (time/format "yyyy-MM-dd" (time/local-date))]
    (.getPath (io/file target-path "_posts" (format "%s-%s-release-%s.md" iso-today project version)))))

(defn news-post
  "Given the `news` for a release, generate a markdown news post that
  can be used in a Jekyll website. The file will be placed in
  `target-path`."
  [project news target-path]
  (let [version (extract-version news)]
    (->>
     (shell/sh "pandoc" "--from=org" "--to=markdown" "--wrap=none"
               "--standalone" ;; so that meta data is emitted
               (str "--metadata=title:" (format "%s Release " (string/capitalize project)) version)
               (format "--metadata=category:%s" (string/capitalize project)) :in (-> news drop-title))
     :out
     ;; inject a magic string into the generated markdown that will cause a toc to be generated
     inject-toc
     (spit target-path))))

(defn download-index
  "Given the `news` for a release, generate a markdown download index
  that can be used in a Jekyll website. The file will be placed in
  `target-path`."
  [project news target-path]
  (let [version (extract-version news)
        content (slurp target-path)
        regexps {"liblouis"      [[#"liblouis-(\d+\.\d+\.\d+)(\.tar\.gz|\.zip)" (format "liblouis-%s$2" version)]
                                  [#"liblouis-(\d+\.\d+\.\d+)-(win32\.zip|win64\.zip)" (format "liblouis-%s-$2" version)]
                                  [#"download/v(\d+\.\d+\.\d+)/liblouis-" (format "download/v%s/liblouis-" version)]]
                 "liblouisutdml" [[#"liblouisutdml-(\d+\.\d+\.\d+)(\.tar\.gz|\.zip)" (format "liblouisutdml-%s$2" version)]
                                  [#"liblouisutdml-(\d+\.\d+\.\d+)-(win32\.zip|win64\.zip)" (format "liblouisutdml-%s-$2" version)]
                                  [#"download/v(\d+\.\d+\.\d+)/liblouisutdml-" (format "download/v%s/liblouisutdml-" version)]]}
        new-content (reduce (fn [content [regexp replacement]] (string/replace content regexp replacement)) content (get regexps project))]
    (spit target-path new-content)))

(defn online-documentation
  "Given the html documentation for a release massage it so that it
  can be placed on the Jekyll based website. Basically rip out the
  embedded style info so that the one from the Jekyll theme is used.
  The file will be placed in `target-path`."
  [project documentation-file target-path]
  (let [doc (slurp documentation-file)
        title (format "---
title: %s User's and Programmer's Manual
---

" (string/capitalize project))
        fixed (->
               doc
               (string/replace #"(?sm)<!DOCTYPE html>.*</a></span></h1>\s*" title)
               (string/replace #"(?sm)\s+</body>\s+</html>" ""))]
    (spit target-path fixed)))

(defn create-release-description
  [project news target-path]
  (let [version (extract-version news)
        markdown (:out (shell/sh "pandoc" "--from=org" "--to=gfm" "--wrap=none" :in (-> news normalize-title)))
        env {:project project
             :version version
             :changes markdown}
        description (without-escaping (parser/render-file "release-file.txt" env))]
    (spit target-path description)))

(defn create-release-command
  [project news description-file]
  (let [version (extract-version news)
        tag (format " v%s" version)
        zip (format "%s-%s.zip" project version)
        tar (format "%s-%s.tar.gz" project version)
        win32 (format "%s-%s-win32.zip" project version)
        win64 (format "%s-%s-win64.zip" project version)]
    (format "hub release create -a %s -a %s -a %s -a %s -F %s %s" zip tar win32 win64
            (.getName description-file) tag)))

(defn -main [source-path website-path]
  (let [project (if (string/includes? source-path "utdml") "liblouisutdml" "liblouis")
        news-file (io/file source-path "NEWS")
        news (changes news-file)
        news-post-file (news-post-name project news website-path)
        download-index-file (io/file website-path "downloads" "index.md")
        documentation-file (io/file source-path "doc" (format "%s.html" project))
        online-documentation-file (io/file website-path "documentation" (format "%s.html" project))
        release-description-file (io/file source-path "release-description.txt")
        announcement-file (io/file source-path "ANNOUNCEMENT")
        env {:source-path source-path
             :news-post news-post-file
             :download-index (.getPath download-index-file)
             :online-documentation (.getPath online-documentation-file)
             :command (create-release-command project news release-description-file)}]
    (announcement project news announcement-file)
    (news-post project news news-post-file)
    (download-index project news download-index-file)
    (online-documentation project documentation-file online-documentation-file)
    (create-release-description project news release-description-file)
    (println (without-escaping (parser/render-file "feedback.txt" env)))
    ;; see https://clojureverse.org/t/why-doesnt-my-program-exit/3754/7 why exit is needed
    (System/exit 0)))
