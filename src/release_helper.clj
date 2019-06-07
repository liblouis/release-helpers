(ns release-helper
  (:require [java-time :as time]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [cljstache.core :as template])
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
  "Return the next release date."
  []
  (first (drop-while #(time/after? (time/local-date) %) release-dates)))

(defn changes [news-file]
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

(defn announcement
  "Extract the news blurb from the NEWS file and generate a text
  announcement that can be used in an email. The file will be named
  \"announcement.txt\". It will be placed in the current working
  directory."
  [news-file next-release-date]
  (let [changes (changes news-file)
        env {:version (extract-version changes)
             :milestone-url (extract-milestone-url changes)
             :next-release-date (time/format "MMMM d yyyy" next-release-date)
             :changes (-> changes milestone-link-to-footnote normalize-title)}]
    (shell/sh "pandoc" "--from=org" "--to=rst"
              "--output=announcement.txt"
              :in (template/render-resource "announcement.org" env))))

(defn news-post
  "Extract the news blurb from the NEWS file and generate a markdown
  news post that can be used in a Jekyll website. The file will be
  named as required by Jekyll. It will be placed in the current
  working directory."
  [news-file]
  (let [changes (changes news-file)
        version (extract-version changes)
        filename (str (time/format "yyyy-MM-dd" (time/local-date)) "-release-" version ".md")]
    (shell/sh "pandoc" "--from=org" "--to=markdown" "--wrap=none"
              "--standalone" ;; so that meta data is emitted
              (str "--output=" filename)
              (str "--metadata=title:" "Liblouis Release " version)
              "--metadata=category:Liblouis" :in changes)))
