(ns reagent-native.camera-roll
  "Wraps the camera roll api.

  To use with IOS, one needs to go through some hoops: refer to the
  React Native docs for details."
  (:require [cljs.core.async :as async]
            ["react-native" :as ReactNative]
            [reagent-native.core :as r]
            [reagent-native.util :as util]))



(def CameraRoll (r/$ ReactNative :CameraRoll))

(def type-translator
  {:photo "photo"
   :video "video"})

(defn save
  "Saves an item to the camera roll.

   The tag must be as in the RN docs. One may optionally
   specify `:type`, which must be one of `:photo`, `:video`.
   Defaults to `:photo`."
  [tag & {:keys [type] :or {type :photo}}]
  (if-let [type (type-translator type)]
    (r/$ CameraRoll :saveToCameraRoll tag type)
    (throw (js/Error. (str "Unknown type " type)))))


(def group-types-translator
  {:album "Album"
   :all "All"
   :event "Event"
   :faces "Faces"
   :library "Library"
   :photo-stream "PhotoStream"
   :saved-photos "SavedPhotos"})

(def asset-type-translator
  {:all "All"
   :photos "Photos"
   :videos "Videos"})

(defn process-node
  [node]
  (let [node (.-node node)]
    {:type (.-type node)
     :group-name (.-group_name node)
     :image (when-some [image (.-image node)]
              {:uri (.-uri image)
               :height (.-height image)
               :width (.-width image)
               :stored? (.-isStored image)
               :playable-duration (.-playableDuration image)})
     :timestamp (.-timestamp node)
     :location (when-some [location (.-location node)]
                 {:latitude (.-latitude node)
                  :longitude (.-longitude node)
                  :altitude (.-altitude node)
                  :heading (.-heading node)
                  :speed (.-speed node)})}))


(defn load-batch
  "Loads a batch of items from the camera roll and places them on a channel for consumption.

  The channel contains exactly one item, the batch, a map with keys :items, :next. Items
  is a collection of nodes, next is a cursor to use for getting the next batch, or nil if
  there are no more batches available.

  Options:
  :after A cursor returned by next.
  :batch-size Behind the scenes, load calls `getPhotos` in batches of this size. Defaults to 30.
  :group-types One of :album, :all, :event, :faces, :library, :photo-stream or :saved-photos.
               Defaults to `:saved-photos`.
  :group-name A string. Corresponds to `groupName` in `CameraRoll`. Defaults to nil.
  :asset-type One of :all, :videos, :photos. Defaults to :photos .
  :mime-types Collection of strings of mime types to filter by. Defaults to nil."
  [& {:keys [batch-size group-types group-name asset-type mime-types after]
      :or {batch-size 30
           group-types :saved-photos
           group-name nil
           asset-type :photos}}]
  (let [group-types-js (group-types-translator group-types)
        asset-type-js (asset-type-translator asset-type)]
    (when-not group-types-js
      (throw (js/Error. (str "Unknown group type: " group-types))))
    (when-not asset-type-js
      (throw (js/Error. (str "Unknown asset type: " asset-type))))
    (let [res (async/chan)
          args {:first batch-size
                :assetType asset-type-js}
          args (if (= :ios r/platform)
                 (assoc args :groupTypes group-types-js) args)
          args (if after (assoc args :after after) args)
          args (if (and (= :ios r/platform)
                        group-name ())
                 (assoc args :groupName group-name) args)
          args (if mime-types (assoc args :mimeTypes mime-types) args)
          args (clj->js args)]
      (->
       (r/$ CameraRoll :getPhotos args)
       (.then #(if (nil? %)
                 (.error js/console "Nil in chan")
                 (async/put! res %)))
       (async/go
         (let [raw-result (async/<! res)
               nodes (.-edges raw-result)
               page-info (.-page_info raw-result)]
           {:items (map process-node nodes)
            :next (when (.-has_next_page page-info)
                    (.-end_cursor page-info))}))))))



(defn load
  "Loads items from the camera roll and places them on a channel for consumption.

  The channel is filled with clojurified nodes, and :end is places on the channel if there are
  no more photos.

  Options:
  :batch-size Behind the scenes, load calls `getPhotos` in batches of this size. Defaults to 30.
  :group-types One of :album, :all, :event, :faces, :library, :photo-stream or :saved-photos.
               Defaults to `:saved-photos`.
  :group-name A string. Corresponds to `groupName` in `CameraRoll`. Defaults to nil.
  :asset-type One of :all, :videos, :photos. Defaults to :photos .
  :mime-types Collection of strings of mime types to filter by. Defaults to nil."
  [& {:keys [batch-size group-types group-name asset-type mime-types]
      :or {batch-size 30
           group-types :saved-photos
           group-name nil
           asset-type :photos}}]
  (let [res (async/chan)]
    (async/go-loop
        [{:keys [items next]}
         (<! (load-batch
              :batch-size batch-size
              :asset-type asset-type
              :group-types group-types
              :group-name group-name
              :mime-types mime-types))]
      (loop
          [items (seq items)]
        (when (seq items)
          (async/>! res (first items))
          (recur (rest items))))
      (if next
        (recur
         (<!
          (load-batch
           :batch-size batch-size
           :asset-type asset-type
           :group-types group-types
           :group-name group-name
           :mime-types mime-types
           :after next)))
        (async/>! res :end)))
    res))
