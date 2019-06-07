(ns release-helper
  (:require [java-time :as time]
            [clojure.java.shell :as shell]
            [clojure.string :as string])
  (:import java.time.format.DateTimeFormatter))

;; Release dates are always on Monday of ISO week 10, 23, 36 and 49
(def release-weeks (cycle [10 23 36 49]))
(def release-years (mapcat #(repeat 4 %) (iterate inc 2019)))

(defn to-date [year week]
  (time/local-date (DateTimeFormatter/ISO_WEEK_DATE) (format "%d-W%d-1" year week)))

(def release-dates (map to-date release-years release-weeks))

(defn next-release-date
  "Return the next release date."
  []
  (first (drop-while #(time/after? (time/local-date) %) release-dates)))

(defn changes [news-file]
  (second (re-find #"(?sm)^(\* Noteworthy.*?)^\* Noteworthy" (slurp news-file))))

(defn extract-milestone [changes]
  (re-find #"(?sm)https://github.com/liblouis/liblouis/milestone/\d+\?closed=1" changes))

(defn extract-version [changes]
  (second(re-find #"(?sm)Noteworthy changes in release (\d+\.\d+\.\d+) \(" changes)))

(defn milestone-link-to-footnote [changes]
  (string/replace changes #"(?sm)\[\[https://github.com/liblouis/liblouis/milestone/\d+\?closed=1\]\[(.*)\]\]" "$1[fn:3]"))

(defn normalize-title [changes]
  (string/replace changes #"(?sm)^\* Noteworthy changes in release \d+\.\d+\.\d+ \(\d+-\d+-\d+\)" "* Noteworthy changes in this release"))

(defn announcement-fmt []
  (slurp "announcement.org"))

(defn announcement [news-file next-release-date]
  (let [changes (changes news-file)
        version (extract-version changes)
        milestone-url (extract-milestone changes)
        changes (milestone-link-to-footnote changes)
        changes (normalize-title changes)
        text (format (announcement-fmt)  version changes next-release-date milestone-url)]
    (:out (shell/sh "pandoc" "--from=org" "--to=rst" :in text))))

