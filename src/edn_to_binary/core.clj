(ns edn-to-binary.core
  (:import java.nio.ByteBuffer)
  (:import java.nio.ByteOrder)
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]))

