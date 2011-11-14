(ns spot-tracks.spot-parser
  (:use [clojure.string :only [split-lines]]
        [clojure.pprint :only [pprint]]
        [clj-time.core :as time :only [from-time-zone date-time time-zone-for-offset]]
        [clj-time.coerce :as time-coerce]))

(defn create-spot-tracks [filename]
  (let [lines (split-lines (slurp filename))
        latitude-re #"Latitude:(-?\d*\.\d*)"
        longitude-re #"Longitude:(-?\d*\.\d*)"
        time-re #"GPS location Date/Time:(\d\d)/(\d\d)/(\d\d\d\d) (\d\d):(\d\d):(\d\d) (...)"
        parse-int (fn [x] (Integer/parseInt x))
        latitudes (->> lines
                       (map (partial re-seq latitude-re))
                       (remove nil?)
                       (map first)
                       (map second))
        longitudes (->> lines
                        (map (partial re-seq longitude-re))
                        (remove nil?)
                        (map first)
                        (map second))
        times
        (->> lines
             (map (partial re-seq time-re))
             (remove nil?)
             (map first)
             (map rest)
             (map vec)
             (map #(conj (vec (map parse-int (butlast %))) (last %)))
             (map (fn [[month day year hour minute second tz]]
                    (from-time-zone (date-time year month day hour minute second)
                                     (if (= tz "EDT")
                                       (time-zone-for-offset -4)
                                       (time-zone-for-offset -5)))))
             (map time-coerce/to-long))
        lat-longs (map (fn [lat long time] {:lat lat :long long :time time}) latitudes longitudes times)]
    lat-longs))

(comment
 (pprint (sort-by :time (create-spot-tracks "src/spot_tracks/spot-tracks.txt"))))