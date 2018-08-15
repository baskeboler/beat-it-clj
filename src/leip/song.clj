(ns leip.song
  (:require [overtone.live :refer :all]
            [overtone.inst.drum :refer [snare kick2]]
            [leipzig.melody :refer :all]
            [leipzig.scale :refer [A B C D E F G from lower high major minor] :as scale]
            [leipzig.live :as live]
            [leipzig.chord :refer [triad seventh inversion] :as chord]
            [leipzig.canon :refer [canon table interval crab mirror]]
            [leipzig.temperament :as temperament]))


; Instruments

(definst overchauffeur [freq 110 dur 1.0 top 1500 vol 0.25]
  (-> (sin-osc freq)
      (+ (* 1/3 (sin-osc (* 2.01 freq))))
      (+ (* 1/2 (sin-osc (* 3.01 freq))))
      (+ (* 1/8 (sin-osc (* 5.01 freq))))
      (+ (* 2 (sin-osc (* 0.5 freq))))
      (clip2 0.7)
      (lpf top)
      (hpf 20)
      (* (env-gen (adsr 0.01 0.2 0.8 0.2) (line:kr 1 0 dur) :action FREE))
      (* vol)))


; Arrangement
(defmethod live/play-note :default
  [{midi :pitch seconds :duration vol :amp}]
  (some-> midi
          midi->hz
          (overchauffeur seconds 1500)))
(defmethod live/play-note :kick [note]
  (kick2 130))
(defmethod live/play-note :snare [note]
  (snare 400 1.5))
;; (kick2 150 1 1)
;; (snare 1000 1 0.1)
                                        ;Composition

(defn beat-it-bass [root]
  (->>
   (phrase (cycle [1/2 1/4 1/4]) (repeat 12 0))
   ;; (phrase (repeat 8 1/2) (repeat 8 0))
   (where :pitch (from root))
   (canon (interval 7))
   (where :pitch (comp lower lower))))

(defn beat-it-chorus [root]
  (->>
   (phrase (concat (repeat 4 1/2) [2])
           (concat (repeat 4 triad) [nil]))
   (where :pitch (from root))
   (all :amp 0.5)))
(def beat-melody
  (->>
   (phrase (repeat 8 1/2) [nil 4 4 4 3 4 3 4])
   (then (phrase [1/2 1/2 1 1 1/2 1/2] [6 4 3 4 0 4]))
   (then (phrase (repeat 8 1/2) [4 4 4 4 4 4 3 2]))
   (then (phrase [1/2 1/2 1 1 1/2 1/2] [3 2 4 0 0 4]))
   (then (phrase [1/2 1/2 1/2 1/2 1 1/2 1/2] [3 2 0 -1 0 0 0]))
   (then (phrase [1/2 1/2 1/2 1/2 1 1] [1 0 -1 -3 -1 0]))
   (then (phrase [1/2 1/2 2 1] [2 0 nil 0]))
   (then (phrase [1/2 1/2 3] [1 -1 nil]))
   ;; (all :part :accompaniment)
   ;; (canon (interval -7))
   ;; (where :pitch (comp scale/D scale/sharp scale/minor))
))

(def beat-it-intro-melody
  (->>
   (phrase [1 1/2 1/2 1/2 3/2 1 1/2 1/2 1/2 1/2 1   1   1/2 1/2 1/2 3/2 1 1/2 1/2 2]
           [0 2   4   9   7   8   7   6   nil 6   nil 0   2   4   9   7   8 7   6   nil])
   (where :time #(- % 1/2))))

(defn beat-it-kick [root]
  (->>
   (phrase [1/2 1/2] [0 0])
   (all :part :kick)))

(defn beat-it-snare [root]
  (->>
   (phrase [1] [0])
   (all :part :snare)))

(defn beat-it-drums [root]
  (->>
   (beat-it-kick root)
   (then (beat-it-snare root))
   (times 2)))

                                        ; Track
(def progression [0 6 0 6 5 6 0 -1])

(def intro-progression [0 6 0 6])

(def beat-it-intro
  (->>
   intro-progression
   (mapthen (fn [k] (->>
                     (phrase (cycle [1/4 1/4 1/2]) (repeat 12 0))
                     (where :pitch (from k))
                     (where :pitch (comp lower lower))
                     (where :time #(- % 1/2))
                     (canon (interval 7)))))
   ;; (where :pitch (comp lower lower))
   (with (mapthen beat-it-drums intro-progression))
   (with beat-it-intro-melody)
   (tempo (bpm 135))
   (where :pitch (comp D scale/sharp minor))))

(def beat-it-verse
  (->>
   (mapthen beat-it-bass progression)
   (with (mapthen beat-it-drums progression))
   (times 3)
   (with (->>
          beat-melody
          (times 2)
          (then (mapthen beat-it-chorus progression))))
   (where :pitch (comp D scale/sharp minor))
   (tempo (bpm 137))))

(def beat-it-verse-2
  (->>

   (->>
    beat-it-intro-melody
    (where :pitch (comp lower lower))
    (canon (interval 7))
    (then (->> [5 6 0 -1]
               (mapthen beat-it-bass))))
   (with (->> progression
              (mapthen beat-it-drums)))
   (with beat-melody)
   (where :pitch (comp D scale/sharp minor))
   (tempo (bpm 137))))

(def beat-it
  (->>
   beat-it-intro
   (then beat-it-verse-2)
   (then beat-it-verse)))

(defn -main []
  (live/play beat-it))

(comment
  (ticker 400)
  (volume 0.8)
  (bass 100 1 1)
  (organ 200 4 1)
  (mooger 2)
  (supersaw 400 1)                              ; Loop the track, allowing live editing.
  (live/play beat-it-verse-2)
  (live/jam (var beat-it))
  (live/jam (var beat-it))
  (organ 200 1 1)
  (live/stop))

  ;; track
