# edn-to-binary

In my experience converting human readable data to an arbitrary binary data is a difficult task to do well.  This library is intended to help support declaratively defining ad-hoc binary formats, and giving the developer tools for serializing and deserializing. This library is a work in progress, so let me know if you have any feature requests or comments on the API.

This library is deeply integrated with clojure.spec, and so it require clojure 1.9.0 or higher.  

## Overview

```
(require '[clojure.spec.alpha :as s] '[edn-to-binary.core :as e])

(e/encode ::e/uint8 125)
;;=>[125]

(e/encode ::e/uint16 125)
;;=>[125 0]

(e/encode ::e/uint32 125)
;;=>[125 0 0 0]
```


### Primitive Codecs

## Usage

FIXME

## License

Copyright © 2018 Richard Hanley

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
