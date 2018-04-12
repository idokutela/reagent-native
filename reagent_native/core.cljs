(ns reagent-native.core
  (:require ["react-native" :as ReactNative]
            [reagent.core :as r])
  (:require-macros
   [reagent.interop :refer [$]]))

(def reagent<-  r/adapt-react-class)

;; React Native Components
(def activity-indicator (reagent<- ($ ReactNative :ActivityIndicator)))
(def button (reagent<- ($ ReactNative :Button)))
(def date-picker-ios (reagent<- ($ ReactNative :DatePickerIOS)))
(def drawer-layout-android (reagent<- ($ ReactNative :DrawerLayoutAndroid)))
(def flat-list (reagent<- ($ ReactNative :FlatList)))
(def image (reagent<- ($ ReactNative :Image)))
#_(def input-accessory-view (reagent<- ($ ReactNative :InputAccessoryView)))
(def keyboard-avoiding-view (reagent<- ($ ReactNative :KeyboardAvoidingView)))
(def list-view (reagent<- ($ ReactNative :ListView)))
(def masked-view-ios (reagent<- ($ ReactNative :MaskedViewIOS)))
(def modal (reagent<- ($ ReactNative :Modal)))
(def navigator-ios (reagent<- ($ ReactNative :NavigatorIOS)))
(def picker (reagent<- ($ ReactNative :Picker)))
(def picker-ios (reagent<- ($ ReactNative :PickerIOS)))
(def progress-bar-android (reagent<- ($ ReactNative :ProgressBarAndroid)))
(def progress-view-ios (reagent<- ($ ReactNative :ProgressViewIOS)))
(def refresh-control (reagent<- ($ ReactNative :RefreshControl)))
(def safe-area-view (reagent<- ($ ReactNative :SafeAreaView)))
(def scroll-view (reagent<- ($ ReactNative :ScrollView)))
(def section-list (reagent<- ($ ReactNative :SectionList)))
(def segmented-control-ios (reagent<- ($ ReactNative :SegmentedControlIOS)))
(def slider (reagent<- ($ ReactNative :Slider)))
(def snapshot-view-ios (reagent<- ($ ReactNative :SnapshotViewIOS)))
(def status-bar (reagent<- ($ ReactNative :StatusBar)))
(def switch (reagent<- ($ ReactNative :Switch)))
(def tab-bar-ios (reagent<- ($ ReactNative :TabBarIOS)))
(def tab-bar-ios-item (reagent<- ($ ReactNative :TabBarIOS.Item)))
(def view (reagent<- ($ ReactNative :View)))
(def text-input (reagent<- ($ ReactNative :TextInput)))
(def toolbar-android (reagent<- ($ ReactNative :ToolbarAndroid)))
(def touchable-highlight (reagent<- ($ ReactNative :TouchableHighlight)))
(def touchable-native-feedback
  (reagent<- ($ ReactNative :TouchableNativeFeedback)))
(def touchable-opacity (reagent<- ($ ReactNative :TouchableOpacity)))
(def touchable-without-feedback
  (reagent<- ($ ReactNative :TouchableWithoutFeedback)))
(def text (reagent<- ($ ReactNative :Text)))
(def view-pager-android (reagent<- ($ ReactNative :ViewPagerAndroid)))
(def virtualized-list (reagent<- ($ ReactNative :VirtualizedList)))
(def web-view (reagent<- ($ ReactNative :WebView)))


(defonce counter (r/atom 1))

(defn reload! [] (swap! counter inc))

(defn register-component
  "Registers a `component` with the app-registry as the given `name`.

   Note, if you have a pure reagent app, you need to call this on your
   main app component, with the name of your app."
  [name component]
  (let [reloading-component (fn [] @counter [component @counter])]
    ($ ReactNative
       AppRegistry.registerComponent
       name
       #(r/reactify-component reloading-component))))
