# cupickle

![A Pickle in a Cubicle](http://i.imgur.com/NsBCjsQ.png)

A Clojure library designed to run cucucmber test-suites.

Write cucumber features with clojure step-definitions from your cubicle... with cupickle!

## Usage

Put `[au.com.auspost/cupickle "0.1.0-SNAPSHOT"]` into the `:dependencies` vector of your project.clj.

Put `(:require [cupickle.steps :as cps])` into your namespace that is going to define steps.

Put `(:require [cupickle.core :as cpc])` into your namespace that is going to trigger testing (not-required if you are using lein-cupickle).

Two namespaces are provided:

* cupickle.core
* cupickle.steps

Core is intended to be used to trigger test runs.

Steps is intended to make the definitions of cupickle-compatible functions easier.

## License

Copyright © 2014 Australia Post

Distributed under the Apache License v2.0

## TODO

* Print single dots for progress
* Show how to implement missing step-definitions
* More documentation
* Publish to clojars
* Annotation support in cupickle.steps
