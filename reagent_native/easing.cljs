(ns reagent-native.easing
  [:require [reagent-native.core :as r]
            ["react-native" :as ReactNative]])

(def Easing (r/$ ReactNative :Easing))

(def step0 (r/$ Easing :step0))
(def step1 (r/$ Easing :step1))
(def linear (r/$ Easing :linear))
(def ease (r/$ Easing :ease))
(def quad (r/$ Easing :quad))
(def cubic (r/$ Easing :cubic))
(def poly (r/$ Easing :poly))
(def sin (r/$ Easing :sin))
(def circle (r/$ Easing :circle))
(def exp (r/$ Easing :exp))
(def elastic (r/$ Easing :elastic))
(def back (r/$ Easing :back))
(def bounce (r/$ Easing :bounce))
(def bezier (r/$ Easing :bezier))
(def in (r/$ Easing :in))
(def out (r/$ Easing :out))
(def in-out (r/$ Easing :in-out))
