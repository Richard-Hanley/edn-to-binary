# edn-to-binary

In my experience converting human readable data to an arbitrary binary data is a difficult task to do well.  This library is intended to help support declaratively defining ad-hoc binary formats, and giving the developer tools for serializing and deserializing. This library is a work in progress, so let me know if you have any feature requests or comments on the API.

This library is deeply integrated with clojure.spec, and so it require clojure 1.9.0 or higher.  

## Encoding

The `encode` function is used to convert a piece of clojure data into a binary collection. `encode` takes two arguments; a special Codec used to specify the type, and the data to be transformed.  Below is an example of the number 125 being encoded as an 8, 16, and 32 bit integer respectively.
```
(require '[edn-to-binary.core :as e])
;;=>

(e/encode ::e/uint8 125)
;;=>[125]

(e/encode ::e/uint16 125)
;;=>[125 0]

(e/encode ::e/uint32 125)
;;=>[125 0 0 0]
```

Signed integers are also supported 

```
(require '[edn-to-binary.core :as e])
;;=>

(e/encode ::e/int8 -125)
;;=>[-125]

(e/encode ::e/int16 -125)
;;=>[-125 -1 -1]

(e/encode ::e/int32 -125)
;;=>[125 -1 -1 -1]

(e/encode ::e/int32 -125)
;;=>[-125 -1 -1 -1 -1 -1 -1 -1]
```

Both 32-bit and 64-bit floating point is also supported

```
(require '[edn-to-binary.core :as e])
;;=>

(e/encode ::e/float 125.895)
;;=>[61 -54 -5 66]

(e/encode ::e/double 125.895)
;;=>[-31 122 20 -82 71 121 95 64]
```

## Using Spec to Validate

In the previous section it was shown that each of the primitive types were defined as keywords.  These keywords are also registered as clojure specs.  This means we can validate data to make sure they are a in a reasonable range.  Below is an example of some bounds checking

```
(require '[clojure.spec.alpha :as s] '[edn-to-binary.core :as e])

(s/valid? ::e/uint8 152)
;;=>true

;;This call will fail because singed 8-bit values can't be higher than 127
(s/valid? ::e/int8 152)
;;=>false

;;Similar checks are done on signedness
(s/valid? ::e/int16 -257)
;;=>true

(s/valid? ::e/uint16 -257)
;;=>false

;;Integral numbers work as floats, but not the other way around
(s/valid? ::e/float 152)
;;=>true

(s/valid? ::e/uint32 152.25)
;;=>false

;;And non-integral types will also fail
(s/valid? ::e/uint32 "A string")
;;=>false
```


## Creating Composite Data

## Usage

FIXME

## License

Copyright © 2018 Richard Hanley

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
