# React Native, `reagent` and `shadow-cljs`

A super simple way to develop react native apps with clojurescript and
reagent, or add clojurescript modules to existing react-native
projects.

If you prefer `rum`, go [here](https://github.com/idokutela/rum-native).

## Why?

`re-natal` is awesome, but I always feels a little too magical for my
taste.  Also, it uses `lein`, and I want to use `shadow-cljs`.

## If I’m starting a new react native project.

Create the project:

    react-native init <project-name>

Install [`shadow-cljs`](https://github.com/thheller/shadow-cljs) if
you haven’t already. With `yarn`, that goes as follows:

    yarn add --dev shadow-cljs

Clone the repo:

    git clone https://github.com/idokutela/reagent-native.git
	
Make a folder to contain your own clojurescript source:

    mkdir src

Make a `shadow-cljs.edn` to suit your config. Be sure to include
`rum-native` and your source path in the source-paths.

A simple example (heavily annotated):

```clojure
{:source-paths
 ["src" "reagent-native"] ; Including rum-native makes sure rum works!

;; No need for exclusions, because shadow-cljs prefers local files
;; to deps. 0.7.0 also works
 :dependencies
 [[reagent "0.8.0-rc1"]]

 :builds
 {:app {:target :npm-module
        :output-dir "lib"}}}
```

Now, start coding! But see the guide below…

## If I already have a react native project

Do as above, except don’t create a project.

## Using `reagent`
One can use reagent exactly as in the browser. The module
`reagent-native.core` defines all the helpers one needs:

 - `register-component`: this takes the app name and the component to
   use as the main app component. This component should take no props
   or children.
 - wrapped React Native 0.55 components: their names are precisely as
in React Native, except that CamelCase becomes kebab-case, and IOS
becomes ios. Props are just clojure maps, with kebab-case keys
replacing the React camelCase keys.

Below is a complete app:

```clojure
;;; src/app/core.cljs
(ns app.core
  (:require [reagent-native.core :as rn]
            [reagent.core :as r])
		
(def styles 
  {:main {:flex 1
          :flex-direction :column
		  :justify-content :center
		  :align-items :center}
   :text {:font-size 20}}
   
(defn result
  [val]
  (rn/text {:style (:text styles)} 
    "You’ve clicked " val (if (= 1 val) " time." " times.")))

(defn app
  []
  (let [cnt (r/atom 0)]
    (fn []
	  [rn/view {:style (:main styles)}
	    [result @cnt]
		[rn/button 
		 {:on-press #(swap! cnt inc)
		  :title "Click me!"}]])))
    
(defn init [] rn/register-component "MyApp" app)
```

```clojure
;;; src/main.cljs
(ns main (:require app.core :refer [init]))

(init)
```

```js
// index.js

import "./lib/main";
```

### Hot reloading
To make hot-reloading work, one needs to tell reagent explicitly to
rerender on reload. The easiest way to do so is to `accept` the
`react-native.core/reload!` function in the `module.hot` hot reaload
hook (as described [here](https://facebook.github.io/react-native/blog/2016/03/24/introducing-hot-reloading.html):

```clojure
;;; main.cljs
(ns main
  (:require [app.core :refer [init]]
            [reagent-native.core :refer [reload!]]))
			
(when-some [hot (.-hot js/module)]
  (.accept hot reload!))
  
(init)
```

## Development workflow
Unfortunately, it’s 
[unlikely](https://github.com/thheller/shadow-cljs/issues/214) a repl
will be working any time soon.

As a consequence, I find it convenient to define (at least) two build
targets:

 - app, as above
 - dev, which builds a node module.
 
The latter can happily be loaded and used in repl based
development. Even better, the nature of reagent components is that
they are simply functions to vectors, so one can test them without a
heavy emulator.
   
## What doesn’t work
- I don't yet know how to get a working repl.
- Source maps are messed up. Unfortunately, there’s not much that will
  change here: react’s bundler ignores source maps when bundling js.
- This is deliberately not on clojars: it's completely experimental,
  and likely to change quite a bit.
- I have yet to see how well this builds to production.
  
## References

 - [`re-natal`](https://github.com/drapanjanas/re-natal): an excellent
   way to quickly get going with react native and
   clojurescript. `react-native/core` was very strongly inspired by
   their work.
 - [`shadow-cljs`](https://github.com/drapanjanas/re-natal): a lovely
   way to build clojurescript
 - [`reagent`](https://github.com/reagent-project/reagent): 
   a clean react cljs library.

## License

Public domain: you're free to do whatever you want with this. However,
I accept no liability for the use you put it to, nor make any claim
that it is fit for any purpose.

