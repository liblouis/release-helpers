(ns release-helper
  (:require [java-time :as time]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [cljstache.core :as template]
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
  (re-find #"(?sm)https://github.com/liblouis/liblouis/milestone/\d+\?closed=1" changes))

(defn extract-version [changes]
  (second(re-find #"(?sm)Noteworthy changes in release (\d+\.\d+\.\d+) \(" changes)))

(defn milestone-link-to-footnote [changes]
  ;; we assume that the template contains a footnote with the milestone link, so we
  ;; replace the link in the changes text with a footref
  (string/replace changes #"(?sm)\[\[https://github.com/liblouis/liblouis/milestone/\d+\?closed=1\]\[(.*)\]\]" "$1[fn:3]"))

(defn normalize-title [changes]
  (string/replace changes #"(?sm)^\* Noteworthy changes in release \d+\.\d+\.\d+ \(\d+-\d+-\d+\)" "* Noteworthy changes in this release"))

(defn inject-toc [changes]
  (string/replace changes #"(?sm)\[the\s+list\s+of\s+closed\s+issues\]\]." "$0

#+begin_export markdown
* Will be replaced with the ToC
{:toc}
#+end_export
"))

(defn announcement
  "Given the `news` for a release, generate a text announcement that can
  be used in an email. The file will be named \"ANNOUNCEMENT\". It
  will be placed in the current working directory."
  [news]
  (let [env {:version (extract-version news)
             :milestone-url (extract-milestone-url news)
             :next-release-date (time/format "MMMM d yyyy" (next-release-date))
             :changes (-> news milestone-link-to-footnote normalize-title)}]
    (shell/sh "pandoc" "--from=org" "--to=rst"
              "--output=ANNOUNCEMENT"
              :in (template/render-resource "announcement.org" env))))

(defn news-post-name
  "Extract the version from a `news` and combine with todays date to
  generate a post name suitable for a Jekyll website at location
  `target-path`."
  [news target-path]
  (let [version (extract-version news)
        iso-today (time/format "yyyy-MM-dd" (time/local-date))]
    (.getPath (io/file target-path (format "%s-release-%s.md" iso-today version)))))

(defn news-post
  "Given the `news` for a release, generate a markdown news post that
  can be used in a Jekyll website. The file will be placed in
  `target-path`."
  [news target-path]
  (let [news (-> news inject-toc)
        version (extract-version news)]
    (shell/sh "pandoc" "--from=org" "--to=markdown" "--wrap=none"
              "--standalone" ;; so that meta data is emitted
              (str "--output=" target-path)
              (str "--metadata=title:" "Liblouis Release " version)
              "--metadata=category:Liblouis" :in news)))

(defn download-index
  "Given the `news` for a release, generate a markdown download index
  that can be used in a Jekyll website. The file will be placed in
  `target-path`."
  [news target-path]
  (let [env {:version (extract-version news)}]
    (spit target-path (template/render-resource "download-index.md" env))))

(defn online-documentation
  "Given the html documentation for a release massage it so that it
  can be placed on the Jekyll based website. Basically rip out the
  embedded style info so that the one from the Jekyll theme is used.
  The file will be placed in `target-path`."
  [documentation-file target-path]
  (let [doc (slurp documentation-file)
        fixed (->
               doc
               (string/replace #"(?sm)<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">.*<h1 class=\"settitle\" align=\"center\">Liblouis User&rsquo;s and Programmer&rsquo;s Manual</h1>\s*" "---
title: Liblouis User's and Programmer's Manual
---

")
               (string/replace #"(?sm)\s+</body>\s+</html>" ""))]
    (spit target-path fixed)))

(defn create-release-description
  [news target-path]
  (let [version (extract-version news)
        markdown (:out (shell/sh "pandoc" "--from=org" "--to=markdown" "--wrap=none" :in (-> news normalize-title)))
        env {:version version
             :changes markdown}
        description (template/render-resource "release-file.txt" env)]
    (spit target-path description)))

(defn create-release-command
  [news description-file]
  (let [version (extract-version news)
        tag (format " v%s" version)
        zip (format "liblouis-%s.zip" version)
        tar (format "liblouis-%s.tar.gz" version)]
    (format "hub release create -a %s -a %s -F %s %s" zip tar (.getName description-file) tag)))

(defn -main [source-path website-path]
  (let [news-file (io/file source-path "NEWS")
        news (changes news-file)
        news-post-file (news-post-name news website-path)
        download-index-file (io/file website-path "downloads" "index.md")
        documentation-file (io/file source-path "doc" "liblouis.html")
        online-documentation-file (io/file website-path "documentation" "liblouis.html")
        release-description-file (io/file source-path "release-description.txt")
        env {:source-path source-path
             :news-post news-post-file
             :download-index (.getPath download-index-file)
             :online-documentation (.getPath online-documentation-file)
             :command (create-release-command news release-description-file)}]
    (announcement news)
    (news-post news news-post-file)
    (download-index news download-index-file)
    (online-documentation documentation-file online-documentation-file)
    (create-release-description news release-description-file)
    (println (template/render-resource "feedback.txt" env))
    ;; see https://clojureverse.org/t/why-doesnt-my-program-exit/3754/7 why exit is needed
    (System/exit 0)))
