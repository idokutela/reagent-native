(ns reagent-native.core
  (:require [reagent.core :as r]
            [cljs.spec.alpha :as s]
            [clojure.string]
            ["react-native" :as ReactNative]))

(defn- split-on-dots
  "Splits the keyword on dots.

   If a string, does nothing. So:
      :foo.bar -> '(\"foo\" \"bar\")
      \"foo.bar\" -> '(\"foo.bar\")"
  [kw]
  (if (string? kw) '(kw)
      (clojure.string/split (name kw) ".")))

(defn $
  "Safely accesses a member of a js object in the react environment.

   Does so in a way that won’t be mangled by the clojure compiler.
   The member is either specified by a string or a keyword `name`. In
   the latter case, the keyword may have inner periods, which directly
   accesses subobjects.

   If further arguments are specified, the member is assumed to be a
   function, and is called on those arguments.

   By default, the accessor stubs the result with the keyword name if
   in the node environment. One can replace this stub with another by
   adding the final opional `:stub` argument.

   Example usages:

   ; in react
   ($ js/console :log \"Hello, world!\") ; outputs \"Hello, world!\"
   ($ js/exports :hot.accept :stub (fn [& args])) ; => module.hot.accept
   (= 2 ($ #js {:foo 2} :foo)) ; true

   ; in node
   ($ js/console :log \"Hello, world!\") ; => :log
  b
   ($ js/exports :hot.accept :stub (fn [& args])) ; => (fn [& args])
   (= 2 ($ #js {:foo 2} :foo)) ; => :foo
  "
  [obj accessor & args]
  (let [access-path (split-on-dots accessor)
        item (apply aget obj access-path)
        get? (zero? (count args))]
    (if get?
      item
      (apply item args))))

(s/fdef $
        :args (s/cat :obj any? :accessor ::string-or-kw :args (s/* any?))
        :ret any?)


(def dimensions (js->clj ($ ReactNative :Dimensions.get "window") :keywordize-keys true))
(def width (:width dimensions))
(def height (:height dimensions))
(def vw (/ width 100))
(def vh (/ height 100))

(def platform
  (if (= "ios" ($ ReactNative :Platform.OS))
    :ios
    :android))



(defn r<-
  [k]
  (r/adapt-react-class ($ ReactNative k)))


(s/fdef r<-
        :args (s/cat :accessor ::string-or-kw)
        :ret any?)


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

(defn reload!
  "Forces react-native to redraw the component."
  [] (swap! counter inc))

(defn register-component
  "Registers a `component` with the app-registry as the given `name`.

   Note, if you have a pure reagent app, you need to call this on your
   main app component, with the name of your app."
  [name component]
  (let [reloading-component (fn [] @counter [component @counter])]
    ((aget ReactNative "AppRegistry" "registerComponent")
     name
     #(r/reactify-component reloading-component))))
