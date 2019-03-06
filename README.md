# edn-to-binary

In my experience converting human readable data to an arbitrary binary format is a difficult task to do cleanly.  This library is intended to help support declaratively defining ad-hoc binary formats, and giving the developer tools for serializing and deserializing. This library is a work in progress, so let me know if you have any feature requests or API comments.

This library is deeply integrated with clojure.spec, and so it requires clojure 1.9.0 or higher.  

## Encoding

The `encode` function is used to convert a piece of clojure data into a binary collection. `encode` takes two arguments; a special Codec used to specify the type, and the data to be transformed.  Below is an example of the number 125 being encoded as an 8, 16, and 32 bit integer respectively.
```
(require '[edn-to-binary.core :as e])
;;=>

(e/encode ::e/uint8 125)
;;=>(125)

(e/encode ::e/uint16 125)
;;=>(125 0)

(e/encode ::e/uint32 125)
;;=>(125 0 0 0)
```

Signed integers are also supported 

```
(require '[edn-to-binary.core :as e])
;;=>

(e/encode ::e/int8 -125)
;;=>(-125)

(e/encode ::e/int16 -125)
;;=>(-125 -1 -1)

(e/encode ::e/int32 -125)
;;=>(125 -1 -1 -1)

(e/encode ::e/int32 -125)
;;=>(-125 -1 -1 -1 -1 -1 -1 -1)
```

Both 32-bit and 64-bit floating point is also supported

```
(require '[edn-to-binary.core :as e])
;;=>

(e/encode ::e/float 125.895)
;;=>(61 -54 -5 66)

(e/encode ::e/double 125.895)
;;=>(-31 122 20 -82 71 121 95 64)
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

### Custom Specs and Types
These codecs are fully featured specs, which support functions such as `s/conform` and `s/explain`.  This library also provides a wrapper over `s/def` and `s/and` that enables the creation of custom types and custom validators.

```
(e/def ::foo (e/and ::e/uint16
                    even?)
;;=>::foo

;;The keyword ::foo is now registered as a uint16 that must be even
(s/valid? ::foo 12)
;;=>true

(s/valid? ::foo -2)
;;=>false

(e/encode ::foo 12)
;;=>(12 0)
```

### Changing Byte Order and Alignment

By default all primitive types are encoded as little endian, and have alignment of 1 (more on alignment later in this guide).  The `e/primitive` macro can be used to create new encoding types.  This combined with `e/def` help to make these new primitives first class codecs.  

```
;;Registering new types as big endian
(e/def :big/int8 (e/primitive Byte ::e/order :big))
(e/def :big/int16 (e/primitive Short ::e/order :big))
(e/def :big/int32 (e/primitive Integer ::e/order :big))
(e/def :big/int64 (e/primitive Long ::e/order :big))
(e/def :big/uint8 (e/unsigned-primitive Byte ::e/order :big))
(e/def :big/uint16 (e/unsigned-primitive Short ::e/order :big))
(e/def :big/uint32 (e/unsigned-primitive Integer ::e/order :big))
(e/def :big/float (e/primitive Float ::e/order :big))
(e/def :big/double (e/primitive Double ::e/order :big))

;;Registering new types with a word size of 8 bytes
(e/def :aligned/int8 (e/primitive Byte ::e/word-size 8))
(e/def :aligned/int16 (e/primitive Short ::e/word-size 8))
(e/def :aligned/int32 (e/primitive Integer ::e/word-size 8))
(e/def :aligned/int64 (e/primitive Long ::e/word-size 8))
(e/def :aligned/uint8 (e/unsigned-primitive Byte ::e/word-size 8))
(e/def :aligned/uint16 (e/unsigned-primitive Short ::e/word-size 8))
(e/def :aligned/uint32 (e/unsigned-primitive Integer ::e/word-size 8))
(e/def :aligned/float (e/primitive Float ::e/word-size 8))
(e/def :aligned/double (e/primitive Double ::e/word-size 8)
```

It is important to note that the alignment of a particular type is the minimum of it's size in bytes and the word-size.  A uint32 would be 4 byte aligned if the word-size was either 4 or 8 bytes. 

## Creating Composite Data

There are three core composite types `arrays`, `tuples`, and `structs`.  These composites are also integrated with spec, so as new complex data types are defined there is support for data validation and exploration.

### Arrays
An array is a repeated collection of data with the same type.  The `e/array` macro is a wrapper over `s/coll-of`, and it takes all of the same arguments that `s/coll-of` takes. The return value of the `array` macro is both a spec and a codec  

```
(e/def ::arr (e/array ::e/uint16))
;;=>::arr

;;Each element of an array must be valid, for an array to be valid
(s/valid? ::arr [1 2 3 -4])
;;=>false

;;Arrays can use s/coll-of bounding arguments
(e/def ::fixed-arr (e/array ::e/uint16 :count 3)
;;=>::fixed-arr

(s/valid? ::fixed-arr [1 2 3])
;;=>true

(s/valid? ::fixed-arr [1 2])
;;=>false

(s/valid? ::fixed-arr [1 2 3 4])
;;=>false

(e/def ::bounded-arr (e/array ::e/uint16 :min-count 2 :max-count 4))
;;=>::bounded-arr

(s/valid? ::bounded-arr [1 2])
;;=>true

(s/valid? ::bounded-arr [1 2 3 4])
;;=>true

(s/valid? ::bounded-arr [1])
;;=>false

(s/valid? ::bounded-arr [1 2 3 4 5])
;;=>false
```

When encoding an array, the resulting collection will be a vector of binary sequences.  The library provides a custom function `e/flatten` that will take a complex binary collection and flatten it to a single binary sequence, while respecting each fields order and alignment.

```
(e/encode ::arr [1 2 3])
;;=>[(1 0) (2 0) (3 0)]

(e/flatten (e/encode ::arr [1 2 3]))
;;=>(1 0 2 0 3 0 4 0)
```

### Tuples

A tuple is a fixed length vector with each element being a fixed (usually different) type.  Spec provides a `s/tuple` macro, and this library provides an equivalent `e/tuple` wrapper

```
(e/def ::tup ::e/uint8 ::e/int16 ::e/uint32)

(s/valid? ::tup [2 -5 100])
;;=>true

(e/encode ::tup [2 -5 100])
;;=>[(2) (-5 0) (100 0 0 0)]

(e/flatten (e/encode ::tup [2 -5 100]))
;;=>(2 -5 0 100 0 0 0)
```

### Structs

Structs are ordered maps.  The `e/struct` macro wraps `s/keys` while maintaining the proper order when encoding.  When a struct is enocded the result is a map of binary collections.  `e/flatten` will respect both the alignment and key order of the binary collection maps.

*An important note about spec.* One of the design decisions of spec is that `s/keys` would only be used to specify existence.  This means that each filed must be seperately defined before creating the struct. 

```
(e/def ::foo ::e/uint8)
(e/def ::bar ::e/uint16)
(e/def ::baz ::e/int32)

(e/def ::st (e/struct ::foo
                      ::bar
                      :baz))
;;=>::st

;;Each field is validated through spec
(e/valid? ::st {::foo 12 ::bar 20 ::baz -12}
;;=>true

;;In this case ::bar should not be negative
(e/valid? ::st {::foo 12 ::bar -20 ::baz -12}
;;=>false

(e/encode ::st {::foo 12 ::bar 20 ::baz -12}))
;;=>{::foo (12) ::bar (20 0) ::baz (-12 -1 -1 -1)}

(e/flatten (e/encode ::st {::foo 12 ::bar 20 ::baz -12}))
;;=>(12 20 0 -12 -1 -1 -1)
```

## Decoding

Decoding is done through a similar method as encoding.  The decode function takes a codec, binary sequence, and some optional arguments.  It returns a tuple of decoded data and remaining bytes. The return value is very similar to the return of `split` or `split-with`.  This gives the programmer the ability to chain multiple decoding calls if desired.

```
(e/decode ::e/uint16 [14 0])
;;=>[14 ()]

(e/decode ::e/uint16 [14 0 13 0 12 0])
;;=>[14 (13 0 12 0)]
```

Decoding of structs and tuples are done seamlessly through the decode function
```
(e/decode ::tup [2 -5 0 100 0 0 0])
;;=>[[2 -5 100] []]

(e/decode ::tup [2 -5 0 100 0 0 0 8 9 10])
;;=>[[2 -5 100] [8 9 10]]

(e/decode ::st [12 20 0 -12 -1 -1 -1])
;;=>[{::foo 12 ::bar 20 ::baz -12} []]
```

Arrays have some special features, depending on how the array is defined.  If the `:count` is specified in the decleration, then the decoder will be know to decode only `:count` number of elements.  However, if there is no `:count`, then the array is unbounded and it will consume as much binary data as possible 

```
(e/def ::arr (e/array ::e/uint16))
;;=>::arr

(e/decode ::arr [1 0 2 0 3 0 4 0 5 0 6 0])
;;=>[[1 2 3 4 5 6] []]

(e/def ::fixed-arr (e/array ::e/uint16 :count 3)
;;=>::fixed-arr

(e/decode ::fixed-arr [1 0 2 0 3 0 4 0 5 0 6 0])
;;=>[[1 2 3] [4 0 5 0 6 0]]
```

To give some control at runtime to the decoder, there are optional arguments that can be used.  The most common one, is specifying `::e/count`.  This will force an unbounded array to consume only a fixed number of elements

```
(e/decode ::arr [1 0 2 0 3 0 4 0 5 0 6 0])
;;=>[[1 2 3 4 5 6] []]

;;By specifying the count, the decoder will only decode 2 elements
(e/decode ::arr [1 0 2 0 3 0 4 0 5 0 6 0] ::e/count 2)
;;=>[[1 2] [3 0 4 0 5 0 6 0]]
```

## Advanced Features

There are some advanced features to manage special encoding and decoding data

### Custom Alignment

Besides allowing for primitives to be redefined with new alignments, there is a special `e/align` macro that will force a codec to have a particular alignment.  The `e/flatten` function will always respect these alignments

```
(e/def ::aligned-tup ::e/uint8
                     ::e/uint16
                     (e/align ::e/uint32 8))

;; The return value from e/encode looks identical to the non-aligned version of this tuple
(e/encode ::aligned-tup [1 2 4])
;;=>[(1) (2 0) (4 0 0 0)]

;; The flattened version has padding added in to handle the 8 byte alignment of element 2
(e/faltten (e/encode ::aligned-tup [1 2 4]))
;;=>(1 2 0 0 0 0 0 0 4 0 0 0)
```

### Data Dependencies

One of the super powers of this library comes from the deep leverage of spec.  Because of this, you can create some powerful dependencies in your data, and have `s/conform` manage them.  There are some helper function for this.  For example, the `dependent-field` function creates a special conformer that can be used to specify a fields relation to another.

The prototypical example of this is a struct with the first element being the number of elements in the second field. The dependent field function takes a key or index, a function that takes the conformed data, and returns the new value of the passed key or index

```
(e/def ::data-count ::e/uint8)
(e/def ::data (e/array ::e/uint16))

(e/def ::tagged-array (e/and (e/struct ::data-count
                                       ::data)
                             (e/dependent-field ::data-count #(count (::data %)))))

;; The value of ::data-count will now be conformed to the length of ::data
(s/conform ::tagged-array {::data-count 0 ::data [1 2 3 4]})
;;=>{::data-count 4 ::data [1 2 3 4]}
```

### Implicit Decoders

Managing data depenencies is extremely powerful when encoding data, but it really can't do anything on the decoding side.  This is because we can't leverage any of spec's validators and conformers.  So to handle this in decoding, we need to be able to give the decoder some hints.

These hints are given through a special argument to the tuple and struct macros called `e/implicit-decoder`

An implict decoder takes a function of `(fn [data-so-far decoding-args] ....)` and returns the decoding args for that particular field.

```

(e/def ::data-count ::e/uint8)
(e/def ::data (e/array ::e/uint16))

(e/def ::tagged-array (e/and (e/struct ::data-count
                                       (e/implicit-decoder (fn [data _]
                                                              {::e/count (::data-count data})
                                                           ::data))
                             (e/dependent-field ::data-count #(count (::data %)))))
                             
;;The implicit decoder will now notice that the first byte is 2, and so it will only decode 2 elements in the array
(e/decode ::tagged-array [2 1 0 2 0 3 0 4 0 5 0])
;;=>[{::data-count 2 ::data [1 2]} [3 0 4 0 5 0]]
```
## TODO

Support for a string primitive needs to be added.  A string would have to support multiple encodings and word sizes

Support for union types should be supported.  This would be an extension of `s/or` types.  Enoding a union type should be very simple, but decoding may be really tricky, because there would need to be a way to tag unions in a binary collection.

Spec has support for mutlimethods, and that would be really nice to support.  A multi-codec would have the same issues as a union type, but it is also hard to create a useable API.  

Spec also has support for ad-hoc sequences and regexs.  This would be really nice to have because sometimes binary data comes in regex form.  Although I really don't know how that would be decoded

## License

Copyright © 2018 Richard Hanley

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
