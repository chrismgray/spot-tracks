(ns spot-tracks.spot-parser
  (:use [clojure.string :only [split-lines]]
        [clojure.pprint :only [pprint]]
        [clj-time.core :as time :only [from-time-zone date-time time-zone-for-offset]]
        [clj-time.coerce :as time-coerce]))

(def num-millis 60000)

(defn create-spot-tracks [filename]
  (let [lines (split-lines (slurp filename))
        latitude-re #"Latitude:(-?\d*\.\d*)"
        longitude-re #"Longitude:(-?\d*\.\d*)"
        time-re #"GPS location Date/Time:(\d\d)/(\d\d)/(\d\d\d\d) (\d\d):(\d\d):(\d\d) (...)"
        parse-int (fn [x] (Integer/parseInt x))
        parse-float (fn [x] (Float/parseFloat x))
        latitudes (->> lines
                       (map (partial re-seq latitude-re))
                       (remove nil?)
                       (map first)
                       (map second)
                       (map parse-float))
        longitudes (->> lines
                        (map (partial re-seq longitude-re))
                        (remove nil?)
                        (map first)
                        (map second)
                        (map parse-float))
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
             (map time-coerce/to-long)             )
        lat-longs (map (fn [lat long time] {:lat lat :long long :time time}) latitudes longitudes times)
        lat-longs (sort-by :time lat-longs)
        first-time (:time (first lat-longs))
        lat-longs (map #(update-in % [:time] (fn [time] (- time first-time))) lat-longs)
        last-time (:time (last lat-longs))
        lat-longs (map #(update-in % [:time] (fn [time] (double (/ time (/ last-time num-millis))))) lat-longs)
        ]
    lat-longs))

(comment
 (pprint (sort-by :time (create-spot-tracks "src/spot_tracks/spot-tracks.txt"))))