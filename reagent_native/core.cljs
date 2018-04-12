(ns reagent-native.core
  (:require [reagent.core :as r]
            [clojure.string]))

(def node? (.-title js/process)) ;; null for React Native
(def ReactNative (when-not node? (js/require "react-native")))

(defn r<-
  [k]
  (if node?
    k
    (r/adapt-react-class
     (apply
      aget ReactNative
      (clojure.string/split (name k) ".")))))

;; React Native Components
(def activity-indicator (r<- :ActivityIndicator))
(def button (r<- :Button))
(def date-picker-ios (r<- :DatePickerIOS))
(def drawer-layout-android (r<- :DrawerLayoutAndroid))
(def flat-list (r<- :FlatList))
(def image (r<- :Image))
#_(def input-accessory-view (r<- :InputAccessoryView))
(def keyboard-avoiding-view (r<- :KeyboardAvoidingView))
(def list-view (r<- :ListView))
(def masked-view-ios (r<- :MaskedViewIOS))
(def modal (r<- :Modal))
(def navigator-ios (r<- :NavigatorIOS))
(def picker (r<- :Picker))
(def picker-ios (r<- :PickerIOS))
(def progress-bar-android (r<- :ProgressBarAndroid))
(def progress-view-ios (r<- :ProgressViewIOS))
(def refresh-control (r<- :RefreshControl))
(def safe-area-view (r<- :SafeAreaView))
(def scroll-view (r<- :ScrollView))
(def section-list (r<- :SectionList))
(def segmented-control-ios (r<- :SegmentedControlIOS))
(def slider (r<- :Slider))
(def snapshot-view-ios (r<- :SnapshotViewIOS))
(def status-bar (r<- :StatusBar))
(def switch (r<- :Switch))
(def tab-bar-ios (r<- :TabBarIOS))
(def tab-bar-ios-item (r<- :TabBarIOS.Item))
(def view (r<- :View))
(def text-input (r<- :TextInput))
(def toolbar-android (r<- :ToolbarAndroid))
(def touchable-highlight (r<- :TouchableHighlight))
(def touchable-native-feedback
  (r<- :TouchableNativeFeedback))
(def touchable-opacity (r<- :TouchableOpacity))
(def touchable-without-feedback
  (r<- :TouchableWithoutFeedback))
(def text (r<- :Text))
(def view-pager-android (r<- :ViewPagerAndroid))
(def virtualized-list (r<- :VirtualizedList))
(def web-view (r<- :WebView))


(defonce counter (r/atom 1))

(defn reload! [] (swap! counter inc))

(defn register-component
  "Registers a `component` with the app-registry as the given `name`.

   Note, if you have a pure reagent app, you need to call this on your
   main app component, with the name of your app."
  [name component]
  (let [reloading-component (fn [] @counter [component @counter])]
    ((aget ReactNative "AppRegistry" "registerComponent") 
     name
     #(r/reactify-component reloading-component))))
