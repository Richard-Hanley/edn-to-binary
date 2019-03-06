# edn-to-binary

In my experience converting human readable data to an arbitrary binary data is a difficult task to do well.  This library is intended to help support declaratively defining ad-hoc binary formats, and giving the developer tools for serializing and deserializing. This library is a work in progress, so let me know if you have any feature requests or comments on the API.

This library is deeply integrated with clojure.spec, and so it require clojure 1.9.0 or higher.  

## Encoding

The `encode` function is used to convert a piece of clojure data into a binary collection. `encode` takes two arguments; a special Codec used to specify the type, and the data to be transformed.  Below is an example of the number 125 being encoded as an 8, 16, and 32 bit integer respectively.
```
(require '[clojure.spec.alpha :as s] '[edn-to-binary.core :as e])
;;=>

(e/encode ::e/uint8 125)
;;=>[0x7D]

(e/encode ::e/uint16 125)
;;=>[0x7D 0x00]

(e/encode ::e/uint32 125)
;;=>[0x7D 0x00 0x00 0x00]
```

Signed integers are also supported 

```
(require '[clojure.spec.alpha :as s] '[edn-to-binary.core :as e])
;;=>

(e/encode ::e/int8 -125)
;;=>[0x83]

(e/encode ::e/int16 -125)
;;=>[0x83 0xff 0xff]

(e/encode ::e/int32 -125)
;;=>[0x83 0xff 0xff 0xff]

(e/encode ::e/int32 -125)
;;=>[0x83 0xff 0xff 0xff 0xff 0xff 0xff 0xff]
```



### Primitive Types
There are seven primitive types supported at the moment

| keyword        | Java Type           | Cool  |
| ------------- |:-------------:| -----:|
| col 3 is      | right-aligned | $1600 |
| col 2 is      | centered      |   $12 |
| zebra stripes | are neat      |    $1 |


## Usage

FIXME

## License

Copyright © 2018 Richard Hanley

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
