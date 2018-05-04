(ns reagent-native.animated
  "Adapts ReactNative.Animated to reagent."
  (:require [clojure.core :exclude [+ - * / mod]]
            [cljs.core.async :as async]
            [clojure.core.async.impl.protocols :refer [ReadPort Channel]]
            [reagent-native.core :as r]
            [reagent-native.easing :as ease]
            [reagent.core :as reagent]))

;;; Utils

(defn js-array<-
  [coll]
  (if (map? coll)
    (-> coll vals js-array<-)
    (-> coll seq clj->js)))

(defn spit->camel
  "Takes a spit-case-string and makes it camel-case."
  [s]
  (let [[first & rest] (clojure.string/split s "-")]
    (apply str
           first
           (map clojure.string/capitalize rest))))

(defn clj->rejs
  "Exactly like `clj->js`, except that spit-case keywords are converted
  to CamelCase strings."
  [x]
  (cond
    (string? x) x
    (keyword? x) (spit->camel (name x))
    (map? x) (apply
              js-obj (transduce
                        (map #(clj->rejs %1 clj->rejs %2))
                        into
                        []
                        x))
    (coll? x) (apply array (map clj->rejs x))
    :else x))


;;; Basic components

(def Animated (r/$ r/ReactNative :Animated))

(def view (r/r<- :Animated.View))
(def scroll-view (r/r<- :Animated.ScrollView))
(def text (r/r<- :Animated.Text))
(def image (r/r<- :Aminated.Image))

(defn animated<-
  "Turns a component into an animated reagent component."
  [component]
  (let [create-animated-component ($ Animated :createAnimatedComponent)])
  (-> component
      create-animated-component
      reagent/adapt-react-class))

(def ^:private Value ($ Animated :Value))
(def ^:private ValueXY ($ Animated :ValueXY))

;;; Values

(defn scalar<-
  "Creates an animated scalar. Defaults to zero."
  ([] (<-val 0))
  ([initial] (Value. initial)))


(defn vector<-
  "Creates an animated vector. Defautls to (0,0)."
  ([] (<- val-xy 0 0))
  ([x y] (ValueXY. x y)))


;;; Watching animation

(defn start!
  "Starts an animation.
  Returns a channel that is filled with `true` if the animation finishes normally,
  `false` otherwise."
  [animation]
  (let [done (async/chan)]
    ($ animation :start #(async/put! done %))
    (async/go
      (-> (async/<! done)
          ($ :finished)))))

(defn stop!
  "Stops an animation."
  []
  (($ animation :stop)))


(defn stop-animation!
  "Stops an animation, and returns a channel with the final value.

  If the final value is a pair, placed as a vector [x y]."
  [val]
  (let [done (async/chan)]
    ($ :stopAnimation #(async/put! done %))
    (async/go
      (let [r (async/<! done)]
        (if (number? r)
          r
          [($ r :x) ($ r :y)])))))


(defn xy? [x] (instance? ValueXY (first x)))

(defrecord EventStream [ch owner listener]
  ReadPort
  (take! [s fn] (take! ch listener))

  Channel
  (close! [x]
    ($ owner :removeListener listener)
    (async/close! ch))
  (closed? [x] (clojure.core.async.impl.protocols/closed? ch)))

(defmulti x-form xy?
  [& args])

(defmethod x-form true [v]
  (fn [xy] [($ xy :x) ($ xy :y)]))

(defmethod x-form false [v] identity)


(defn subscribe
  "Subscribes to an animated value, returning a channel with the changing values.

  If the value is a scalar, the channel receives a stream of numbers. If it is a
  vector, the channel receives a stream of pairs [x y]. Closing the channel
  automatically removes the event listener."
  [v]
  (let [out-ch (async/chan (async/sliding-buffer 1000))
        in-ch (async/chan (async/sliding-buffer 1000))
        listener ($ v :addListener #(async/put! in-ch %))
        x<- (x-form v)])
  (async/go-loop [x (async/<! in-ch)]
    (when x
      (async/>! out-ch (x<- x))))
  (->EventStream out-ch v listener))


;;; Config animations

(defn xy<-
  "Turns a pair [x y] into a js object {x y}, but leaves numbers untouched."
  [x]
  (if (vector? x)
    #js {:x (first x) :y (second x)}
    x))


(defn xy<-ranges
  "Turns a pair of ranges into {x y}, leaves single ranges alone."
  [p]
  (let [pair? (and (vector? p) (vector? (first p)))]
    (if pair?
      #js {:x (js-array<- (first p))
           :y (js-array<- (second p))}
      (js-array<- p))))


(defmulti interpolate
  "Creates a new animated value that interpolates another.

  Example: (interpolate x :<-range [0 1] :->range [0 100] :extrapolate :clamp)"
  [& args] xy?)


(defmethod interpolate false
  [val & {:keys <-range ->range easing extrapolate extrapolate-left extrapolate-right}]
  ($ val :interpolate
     (clj->js
      {:inputRange <-range
       :outputRange ->range
       :easing easing
       :extrapolate extrapolate
       :extrapolateLeft extrapolate-left
       :extrapolateRight extrapolate-right})))

(defmethod interpolate true
  [val & {:keys <-range ->range easing extrapolate extrapolate-left extrapolate-right}]
  ($ val :interpolate
     (clj->js
      {:inputRange (xy<-ranges <-range)
       :outputRange (xy<-ranges ->range)
       :easing (xy<- easing)
       :extrapolate (xy<- extrapolate)
       :extrapolateLeft (xy<- extrapolate-left)
       :extrapolateRight (xy<- extrapolate-right)})))


(defmulti decay
  "Animates `val` to decay. One must speciify the :velocity argument.

  Other optional arguments are `deceleration` `interaction?` and `native?`."
  [& args] xy?)

(defmethod decay false
  [val & {:keys [velocity
                 deceleration
                 interaction?
                 native?
                 value<-]
          :or [deceleration 0.997
               interaction? true
               native? false]}]
  ($ Animated :decay val
     #js {:toValue value<-
          :velocity velocity
          :deceleration deceleration
          :isInteraction interaction?
          :useNativeDriver native?}))

(defmethod decay true
  [val & {:keys [velocity
                 deceleration
                 interaction?
                 native?
                 value<-]
          :or [deceleration 0.997
               interaction? true
               native? false]}]
  ($ Animated :decay val
     #js {:toValue (xy<- value<-)
          :velocity (xy<- velocity)
          :deceleration (xy<- deceleration)
          :isInteraction interaction?
          :useNativeDriver native?}))



(defmulti timing
  "Animates `val` along a timed easing curve.

  Optional arguments are `duration`, `easing`, `delay`, `interaction?`
  and `native?`."
  [& args] xy?)

(defmethod timing false
  [val & {:keys [value<- duration easing delay interaction? native?]
          :or {duration 500
               easing ease/in-out
               delay 0
               interaction? true
               native? false}}]
  ($ Animated :timing val
     #js {:toValue value<-
          :duration duration
          :easing easing
          :delay delay
          :isInteraction interaction?
          :useNativeDriver use-native-driver}))

(defmethod timing true
  [val & {:keys [value<- duration easing delay interaction? native?]
          :or {duration 500
               easing ease/in-out
               delay 0
               interaction? true
               native? false}}]
  ($ Animated :timing val
     #js {:toValue (xy<- value<-)
          :duration (xy<- duration)
          :easing (xy<- easing)
          :delay (xy<- delay)
          :isInteraction interaction?
          :useNativeDriver use-native-driver}))


(defmulti spring
  "Animates a value according to an analytical spring model.

  If one specifies spring constants, one must either use the quadruple
  `friction`, `tension`, `speed` and `bounciness`, or the triple
  `stiffness`, `damping` and `mass`. One may also specify `velocity`,
  `overshoot-clamping`, `rest-displacement-threshold`, `rest-speed-threshold`,
  `interaction?` and `native?`."
  [& args] xy?)

(defmethod spring false
  [val $ {:keys [value<-
                 friction tension speed bounciness
                 stiffness damping mass
                 velocity overshoot-clamping rest-displacement-threshold
                 rest-speed-threshold interaction? native?]
          :or {friction 7
               tension 40
               speed 12
               bounciness 8
               velocity 0
               overshoot-clamping false
               rest-displacement-threshold 0.001
               rest-speed-threshold 0.001
               delay 0
               interaction? true
               native? false}}]
  (let [props (if stiffness
                #js {:toValue value<-
                     :stiffness stiffness
                     :damping damping
                     :mass mass
                     :velocity velocity
                     :overshootClamping overshoot-clamping
                     :restDisplacementThreshold rest-displacement-threshold
                     :restSpeedThreshold rest-speed-threshold
                     :isInteraction interaction?
                     :useNativeDriver native?}
                #js {:toValue value<-
                     :friction friction
                     :tension tension
                     :speed speed
                     :bounciness bounciness
                     :velocity velocity
                     :overshootClamping overshoot-clamping
                     :restDisplacementThreshold rest-displacement-threshold
                     :restSpeedThreshold rest-speed-threshold
                     :isInteraction interaction?
                     :useNativeDriver native?})]
    ($ Animated :spring val props)))

(defmethod spring true
  [val $ {:keys [value<-
                 friction tension speed bounciness
                 stiffness damping mass
                 velocity overshoot-clamping rest-displacement-threshold
                 rest-speed-threshold interaction? native?]
          :or {friction 7
               tension 40
               speed 12
               bounciness 8
               velocity 0
               overshoot-clamping false
               rest-displacement-threshold 0.001
               rest-speed-threshold 0.001
               delay 0
               interaction? true
               native? false}}]
  (let [props (if stiffness
                #js {:toValue (xy<- value<-)
                     :stiffness (xy<- stiffness)
                     :damping (xy<- damping)
                     :mass (xy<- mass)
                     :velocity (xy<- velocity)
                     :overshootClamping (xy<- overshoot-clamping)
                     :restDisplacementThreshold (xy<- rest-displacement-threshold)
                     :restSpeedThreshold (xy<- rest-speed-threshold)
                     :isInteraction interaction?
                     :useNativeDriver native?}
                #js {:toValue (xy<- value<-)
                     :friction (xy<- friction)
                     :tension (xy<- tension)
                     :speed (xy<- speed)
                     :bounciness (xy<- bounciness)
                     :velocity (xy<- velocity)
                     :overshootClamping (xy<- overshoot-clamping)
                     :restDisplacementThreshold (xy<- rest-displacement-threshold)
                     :restSpeedThreshold (xy<- rest-speed-threshold)
                     :isInteraction interaction?
                     :useNativeDriver native?})]
    ($ Animated :spring val props)))


;;; Arithmetic

(defn +
  "Adds any number of animated values.

  Arity 0 returns the zero scalar."

  ([] (scalar<-))
  ([x0] x0)
  ([x0 x1] ($ Animated :add x0 x1))
  ([x0 x1 x2] (+ (+ x0 x1) x2))
  ([x0 x1 x2 x3] (+ (+ x0 x1 x2) x3))
  ([x0 x1 x2 x3 x4] (+ (+ x0 x1 x2 x3) x4))
  ([x0 x1 x2 x3 x4 & xs]
   (reduce + (+ x0 x1 x2 x3 x4) xs)))


(defn *
  "Multiplies any number of animated values.

  Arity 0 returns the unit scalar."
  ([] (scalar<- 1))
  ([x0] x0)
  ([x0 x1] ($ Animated :multiply x0 x1))
  ([x0 x1 x2] (* (* x0 x1) x2))
  ([x0 x1 x2 x3] (* (* x0 x1 x2) x3))
  ([x0 x1 x2 x3 x4] (* (* x0 x1 x2 x3) x4))
  ([x0 x1 x2 x3 x4 xs] (reduce * (* x0 x1 x2 x3 x4) xs)))


(defn -
  "Subtracts any positive number of animated values."
  ([x0] (* (scalar<- -1) x0))
  ([x0 x1] (+ x0 (- x1)))
  ([x0 x1 xs] (+ (x0 (- (apply + x1 xs))))))


(defn /
  "Divides any *positive* number of animated values."
  ([x0] x0)
  ([x0 x1] ($ Animated :divide x0 x1))
  ([x0 x1 xs] (/ x0 (apply * x1 xs))))


(defn mod
  "Takes the modulus of two animated values."
  [num div]
  ($ Animated :modulo num div))


(defmulti diff-clamp
  "Creates a new animated value `v` that is limited between `min` and `max`.

   It uses the difference between the last stationary value and the current,
   so that even if the value is far from the bounds it will start changing
   when the value starts moving closer again."
  [& args] xy?)

(defmethod diff-clamp false
  [v min max]
  ($ Animated :diffClamp v min max))

(defmethod diff-clamp true
  [v min max]
  ($ Animated :diffClamp v (xy<- min) (xy<- max)))


;;; Sequencing

(defn delay
  "Starts an animation after a given time."
  [t]
  ($ Animated :delay t))


(defn sequence
  "Starts a collection of `animations` in order.

  If the current animation is stopped, later animations will not be started."
  [animations]
  ($ Animated :sequence (js-array<- animations)))


(defn parallel
  "Starts a colleciton of `animations` parallel.

   By default, the animation stops with the first one stopping.
   One may optionally specify `stop-together` to be false to
   override this."
  [animations & {:keys [stop-together]}]
  (let [config? (if (some? stop-together)
                  #js {:stopTogether stop-together}
                  nil)]
    ($ Animated :parallel (js-array<- animations) config?)))


(defn stagger
  "Starts a collection of animations successively with a delay."
  [t animations]
  ($ Animated :stagger t (js-array<- animations)))


(defn loop
  "Loops a given animation indefinitely.

   Optional arguments: iterations, native?
   interactive?."
  [animation & {:keys [iterations]}]
  ($ Animated :loop animation
     #js {:iterations iterations}))


;;; Event handling

(defn trap-event
  "Follows a native event as in Animated.event.

   Note, the config is a clojurescript map. Keys can be spit or
   camelcase."
  [mapping & {:keys [native? listener]}]
  ($ Animated :event (clj->rejs mapping)
     #js {:useNativeDriver native?
          :listener listener}))


(defn scroll-x
  "A performant event handler that places the scroll offset x into `x`."
  [x & {:keys [native? listener]}]
  ($ Animated :event
     #js [#js {:nativeEvent #js {:contentOffset #js {:x x}}}]
     #js {:listener listener
          :useNativeDriver native?}))


(defn scroll-y
  "A performant event handler that places the scroll offset y into `y`."
  [y & {:keys [native? listener]}]
  ($ Animated :event
     #js [#js {:nativeEvent #js {:contentOffset #js {:y y}}}]
     #js {:listener listener
          :useNativeDriver native?}))


(defn scroll-xy
  "An event handler that places the scroll offset into an animated vector a pair `x` `y`."
  [x y & {:keys [native? listener]}]
  ($ Animated :event
     #js [#js {:nativeEvent #js {:contentOffset #js {:x x :y y}}}]
     #js {:listener listener
          :useNativeDriver native?}))


(defn pan-respoder
  "An event handler that places a pan-responder key into the corresponding var.

   Example: (pan-responder {:dx x} :native? true)"
  [{:keys [dx dy move-x move-y x0 y0 vx vy]}
   & {:keys [native? listener]}]
  ($ Animated :event
     #js [nil
          #js {:dx dx
               :dy dy
               :moveX move-x
               :moveY move-y
               :x0 x0
               :y0 y0
               :vx vx
               :vy vy}]
     #js {:listener listener
          :useNativeDriver native?}))
