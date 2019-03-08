# Design Walkthrough

## Repo Structure

```
+-- _src
|   +-- edn_to_binary
|       +-- core.clj
|       +-- deprecated.clj
|       +-- deprecated2.clj
|       +-- deprecated3.clj
|       +-- usb.clj
+-- test
|   +-- edn_to_binary
|       +-- core_test.clj
```

The two most important files are `core.clj` and `core_test.clj`.  The deprecated files are leftover from the first 3 attempts at getting this library right.  I have kept them around because they have some good lessons on what to do (and what not to do).  Once this library is finished, I will remove them.

The `usb.clj` file is a motivating example of what is trying to be accomplished with this library.  It is an attempt to declare a codec capable of encoding and decoding usb descriptors.  In my opinion usb descriptors are the ultimate test of binary serialization.  Aything that can handle those descriptors can probably handle most anything.

## The Codec Protocol

A Codec is defined as follows:

```
(defprotocol Codec
  (alignment* [this])
  (encode* [this data])
  (decode* [this binary args]))
```

These core function are used to enocde to binary, decode from binary, and get the alignment of the type when embedded into a composite object.  However users will usually call special functions, that for reasons that should soon become clear, are far more hygenic to use.  These functions are:

```
(defn encode [specified-codec data]) 
(defn decode [specified-codec binary & args]) 
(defn alignment [specified-codec]) 
```

You might notice that these functions take a `specified-codec` instead of a `codec` argument. What is a specifiec-codec?  Well the answer to that goes to the heart of how this library is built.  

### Merging Codecs and Specs

One of the design goals I had was that all Codecs should be able to act as Specs.  Spec is a very powerful tool for data validation and I figured that if I could properly extend Spec, then data valadation would become vastly easier.

However, this is a task that is easier said than done. Spec goes out of it's way to hide it's data, and the Spec protocol functions operate on arguments that have been heavily processed by some macros.  In the end I realized it would be much simpler to create a parallel system, and then attach everything together with some macros. But first, to handle this attachement system a few things needed to be done. 

#### Recreating the Registry

Spec provides a global registry that can be used to register a given spec with a namespace qualified keyword.  At first glance this seem to be fairly small feature, but it ends up being used extensively (in particular when trying to conform maps).  If all codecs can be registered in the same way that specs are, then a programmer can use keywords as a way to symbolically referenece the two without needing any particular code sharing between the libraries.

To accomplish this, the registry needed to be re-implemented.  The following functions were taken and modified from the Spec source code

```
(defonce ^:private registry-ref (atom {}))

(defn registry
  "returns the registry map, prefer 'get-codec' to lookup a codec by name"
  []
  @registry-ref)

(defn get-codec
  "Returns the codec registered with the fully qualified keyword"
  [k] (get @registry-ref k))

(defn register-codec [k c]
  (swap! registry-ref assoc k c))
  
(defn reg-resolve
  "returns the codec end of alias chain starting with k, nil if not found, k if k not Named"
  [k]
  (if (named? k)
    (let [reg @registry-ref]
      (loop [codec k]
        (if (named? codec)
          (recur (get reg codec))
          (when codec
            (with-name codec k)))))
    k))
```

The registry is a private atom.  There are two accessor functions `get-codec` and `registry` that can be used to get values from the registry at a particular time.  New codecs can be added using the `register-codec`.  

Finally, the `reg-resolve` function is used to follow a registered chain.  Consider a case where `::foo` is registered as `::bar`, which is then registered as `::baz`.  The `reg-resolve` function would be able to follow the chain, and eventually return the codec mapped by `::baz`

#### Implementing the Core Functions

Now that we have these core registry functions, we can take a look at the implementation of the core functions.  Here is the implementation of `encode`:

```
(defn encode 
  "Returns an encoded binary collection."
  [specified-codec data] 
   (let [codec (if (keyword? specified-codec) 
                 (reg-resolve specified-codec)
                 (extract-codec specified-codec))]
     (encode* codec data)))
```

This function takes a look at the passed codec to see if it is a keyword.  If the passed codec is a keyword, then it is assumed that it is in the registry, and it tries to resolve from the registry.  If the passed codec is unregistered, then it calls `extract-codec`, which will be explained in the next section.

Here is the code for `decode` and `alignment`.  They work almost the exact same way that `encode` works.  The only difference is that `decode` uses spec to validate the arguments that were passed to it.  The decode function also has a way to trim alignment padding.  How that works will also have to wait until later.

```
(defn decode [specified-codec bin & decoding-args] 
  "Given a binary sequence, this will return a tuple with the decoded value, and the
  rest of the binary that was unused"
  (let [args (s/conform (s/keys*) decoding-args)
        codec (if (keyword? specified-codec) 
                (reg-resolve specified-codec)
                (extract-codec specified-codec))]
    (if (s/invalid? args)
      (throw (ex-info "Invalid decoding args for decode" (s/explain-data (s/keys*) decoding-args)))
      (decode* codec 
               (trim-to-alignment (alignment* codec) (seq bin))
               args))))
               
(defn alignment 
  "Returns the alignment of the specified codec"
  [specified-codec] 
  (let [codec (if (keyword? specified-codec) 
                (reg-resolve specified-codec)
                (extract-codec specified-codec))]
    (alignment* codec)))
```

#### Metamagic (Dealing with Unregistered Codecs)

The registry alone doesn't do a whole lot.  To tie it all together we need a way to represent specs and codecs in the same object.  We also need to have a way to register a codec and spec together.  

This was accomplished through an intellegient use of metadata.  The return value of all spec macros is a Spec object, and most objects in Clojure can have arbitrary metadata attached to them.  By attaching a codec to the metadata of a spec, there is now a way to create objects that look and feel exactly like specs, but also have all of the extended features of a codec.  

```
(defn extract-codec 
  "Given a spec object, this will extract the annotated codec.  The spec must have 
  a codec embedded in the metadata.  This function will not work on keywords"
  [spec]
  (if (keyword? spec)
    spec
    (or (::codec (meta spec))
        (throw (ex-info "No codec defined for the given spec" {:spec spec :meta (meta spec)})))))

(defmacro def 
  "Given a namespace qualified keyword k, this will register the codec and assocaited
  spec to k.  The spec is assumed to be part of the metadata of the passed codec"
  [k specified-codec]
  `(do 
     (register-codec ~k (extract-codec ~specified-codec))
     (s/def ~k ~specified-codec)))
```

At this point we have a complete mechanism to integrating instances of the Codec protocol to the Spec protocol.  Codecs are to be attached to existing specs using metadata. The result of attachment can be called a specified-codec, and we now have a `def` macro that can take specified-codecs and register them both locally and in spec.  The rest of this document is going to be discussing the implmentation of different codecs.

## Defining Primitives

The primitive codecs and specs are relatively straightfoward.  However, to reduce the amount of boilerplate and there are a few macros that make it pretty easy to create new primitives.  In order to create a primitive, you need to create a spec, and then create an implementation of a codec.  Fortunately there are some macros that can help.

### Specifying Primitives

A primitive is considered valid so long as it is a valid numerical type, the value is within a valid range, and unsigned numbers are not negative

#### Unsigned Spec

```
(defmacro unsigned-primitive-spec [class]
  `(s/int-in 0 (bit-shift-left 1 (. ~class SIZE))))
```

The valid range of an unsigned type of bit n is `[0, 2^n)`.  Fortunately spec provides a built-in `s/int-in` which does this check for us.  Since this is a macro, we can extract the bit size directly from the Java class definition.

#### Signed Spec

```
(defmacro signed-primitive-spec [class]
  `#(and (int? %1)
         (or (zero? %1)
             (<= (. ~class MIN_VALUE) %1 (. ~class MAX_VALUE)))))
```

The valid range for a signed type of bit n is `[-2^(n-1), 2^(n-1))`.  This spec is a simple predicate that makes sure the input is a integral value and is between these ranges

#### Floating Spec

```
(defmacro floating-primitive-spec [class]
  `#(or (zero? %1)
        (<= (. ~class MIN_VALUE) %1 (. ~class MAX_VALUE))))
```

A floating point check works exactly like the signed spec, only it doesn't need to check if the input is integral.

### The Primitive Record

To implement the Codec protocol for primtives a special PrimitiveCodec record was created.  The primitive Codec has four fields as shown in this decleration:

```
(defrecord PrimitiveCodec [size get-buffer put-buffer coerce-from])
```

Since records are just maps, there are some keys that are used to configure the record.

- __Required__ `:size` The size in bytes of this primitive. Primitives are assumed to have a fixed size
- __Required__ `:get-buffer` A function that takes a java.nio.ByteBuffer and returns the primtive value
- __Required__ `:put-buffer` A 2-airity function that takes a java.nio.ByteBuffer, a primitive data, and puts the primitive into the buffer
- __Required__ `:coerce-from` A function that can modify a primitive value before being returned by `decode`.  For most primitives this is `identity`, but unsigned values should call `Long/toUnsignedLong` to make the return values more human readable
- __Optional__ `::e/word-size` The alignment of a primitive is the minimum value of word size and primitive size.
- __Optional__ `::e/force-alignment` The alignment can be forced to a specific value, which overrules the word-size argument.
- __Optional__ `::e/order` The byte order of the primitive can be `:little` `:big` `:native` and `:network`

Once these config functions are passed, the record can then implement the Codec protocol.

```
(defrecord PrimitiveCodec [size get-buffer put-buffer coerce-from]
  Codec
  (alignment* [this]
    (let [{:keys [::force-alignment ::word-size] 
           :or {force-alignment nil word-size 1}} this]
      (or force-alignment
          (min size word-size))))
  (encode* [this data]
    (let [buff (.order (ByteBuffer/allocate size)
                       (get order-map (::order this) default-order))
          _ (put-buffer buff (or data 0))]
      (make-binary (seq (.array buff))
                   :align (alignment* this))))
  (decode* [this bin _] 
    (let [[prim remaining] (split-at size bin)
          bytes (.order (ByteBuffer/wrap (byte-array prim))
                         (get order-map (::order this)))
          data (coerce-from (get-buffer bytes))]
      [data
       (indexed-binary (+ size (current-index bin))
                       remaining)])))
```

Alignment simply checks if the `::force-alignment` or `::word-size` key is set.  

Encoding first allocates a java.nio.ByteBuffer with the appropriate size and byte order.  The data is then encoded into the ByteBuffer, which is then returned as a BinaryCollection by making a call to `make-binary`.  The usage of a BinaryCollection is explained later, for now you can just treat the result as a seq with some metadata.

Decoding works by first splitting the passed binary up into a prim section and a remaining section.  This works because primitives are all fixed size. The prim section is then converted to a byte array and wrapped as a ByteBuffer.  Once the data has been read from the 
ByteBuffer it is corerced into a human readable number.

One of the downsides to this implementation is that primitives need to be individually allocated when encoding and decoding. This does affect performance. So far the performance cost has been worth the development cost of using Clojure sequences.

### Combining Spec and Codec

At this point we have a group of macros that can be used to generate specs, and we have a record that can be used to implement the Codec protocol.  Now we just have to tie them together.  First we need a mechanism to relate primitive classes to PrimitiveCodecs

```
(def primitive-codecs
  {Byte (map->PrimitiveCodec {:get-buffer #(.get %) :put-buffer #(.put %1 (unchecked-byte %2)) :size (Byte/BYTES) :coerce-from identity})
   Short (map->PrimitiveCodec {:get-buffer #(.getShort %) :put-buffer #(.putShort %1 (unchecked-short %2)) :size (Short/BYTES) :coerce-from identity})
   Integer (map->PrimitiveCodec {:get-buffer #(.getInt %) :put-buffer #(.putInt %1 (unchecked-int %2)) :size (Integer/BYTES) :coerce-from identity})
   Long (map->PrimitiveCodec {:get-buffer #(.getLong %) :put-buffer #(.putLong %1 (unchecked-long %2)) :size (Long/BYTES) :coerce-from identity})
   Float (map->PrimitiveCodec {:get-buffer #(.getFloat %) :put-buffer #(.putFloat %1 (unchecked-float %2)) :size (Float/BYTES) :coerce-from identity})
   Double (map->PrimitiveCodec {:get-buffer #(.getDouble %) :put-buffer #(.putDouble %1 (unchecked-double %2)) :size (Double/BYTES) :coerce-from identity})})
```

The primitive-codecs map has Java classes as keys, and PrimitiveCodecs as values.  So a call like `(get primitive-codecs Short)` would return the apporpriate record. This map is used in the primitive macro shown below:

```
(defmacro primitive [prim & encoding]
  (let [c `(get primitive-codecs ~prim)
        enc `(if (s/valid? (s/keys*) [~@encoding])
               (s/conform (s/keys*) [~@encoding])
               (throw (ex-info "Unable to conform encoding" (s/explain-data (s/keys*) [~@encoding]))))]
  `(with-meta (signed-primitive-spec ~prim)
              {::codec (merge ~c ~enc)})))
```

The primitive macro takes a Java primitive class, and gets the record from the primitive-codecs map.  It also validates all of the optional encoding arguments, which are merged with the record.  The result of this merge is still a record, which is a valid implementation of the Codec protocol.

The `with-meta` function merges the `signed-primitive-spec` with the primitive record.  The result of which is a specified codec.  

#### Registering the Specified Primitive

Now that we have a fully functioning primitive that can be freely used with `encode`, `decode`, and `alignment`; we need to register these locally and with spec. 

```
(edn-to-binary.core/def ::int8 (primitive Byte))
(edn-to-binary.core/def ::int16 (primitive Short))
(edn-to-binary.core/def ::int32 (primitive Integer))
(edn-to-binary.core/def ::int64 (primitive Long))

(edn-to-binary.core/def ::uint8 (unsigned-primitive Byte))
(edn-to-binary.core/def ::uint16 (unsigned-primitive Short))
(edn-to-binary.core/def ::uint32 (unsigned-primitive Integer))

(edn-to-binary.core/def ::float (floating-primitive Float))
(edn-to-binary.core/def ::double (floating-primitive Double))
```

With this we now have primitives fully functioning.  They are registered as keywords, and function as spec objects and codecs.  Things are pretty good right now, but all of this is for naught if we can't get composite types working.

## Composite Macros and Implicit Decoders

__WARNING: Dark Magic Ahead__

No seriously, things are about to get a little spooky.  Spec implements some very powerful macros to conform collections, tuples, and maps.  The goal is to create special codec macros `array`, `tuple`, and `struct` that wrap the spec macros.  It's a well known fact that Lisp macros can be difficult to write and debug.  However, that is nothing compared to Lisp macros that are trying to manipulate other Lisp macros.

To make things even more complicated, I wanted to be able to be able to provide runtime hints to the decoder.  One of the more common occurences in binary formats is to have a tagged array.  In these situations a custom parser would read the first token to get the array length, and then in a loop read the array.  I wanted to be able to specify this in a declaritive fashion.  This means that the composite macros must do some transformation on their input.

The code in this section is very dense. If you've made it this far into the documentation, then I'm going to assume you have some understanding of how Clojure macros are evaluated.  I welcome any comments or suggestions you may have to make these more hygenic.

### Thec Codecs

Before we get to the macros, let's understand the composite codecs. Each of the three composite types are functions that reify a Codec protocol

```
(defn array-impl 
  ([codec]
   (reify Codec
     (alignment* [_] ....)
     (encode* [this data] ...)
     (decode* [this bin decoding-args] ....))))
     

(defn tuple-impl 
  ([codecs implicit-decoders]
   (reify Codec
     (alignment* [_] ....)
     (encode* [this data] ...)
     (decode* [this bin decoding-args] ....))))
     

(defn struct-impl 
  ([key-codec-pairs implicit-decoders]
   (reify Codec
     (alignment* [_] ....)
     (encode* [this data] ...)
     (decode* [this bin decoding-args] ....))))
```

#### Encoding an Array

If we take a look at the implementation of the of the array Codec (sans decoding for now), you'll notice a call to a `raw-alignment` and `raw-encode`

```
(defn array-impl 
  ([codec]
   (reify Codec
     (alignment* [_] (raw-alignment codec))
     (encode* [this data] (make-binary (mapv (partial raw-encode codec) data)
                                       :align (alignment* this)))
     (decode* [this bin decoding-args] ....))))

(defn- raw-alignment [codec-form] 
  (let [codec (if (keyword? codec-form) 
                (reg-resolve codec-form)
                codec-form)]
    (alignment* codec)))

(defn- raw-encode 
  [codec-form data] 
   (let [codec (if (keyword? codec-form) 
                 (reg-resolve codec-form)
                 codec-form)]
     (encode* codec data)))
````

The raw functions work very similarly to the normal core functions, except they are working with either registered keywords or raw Codec objects, and never take Spec objects.

The alignment on an array is always just the alignment of it's base codec that is passed in.  And the encoding is simply done by mapping `raw-encode` to each element in the passed array.  The `make-binary` function just tags the resulting vector with some metadata.

#### Encoding a Tuple

The implementation of a tuple is similar to an array.  In this case however, a vector of codecs is passed instead of a single repeated codec

```
(defn tuple-impl [codecs implicit-decoders]
  (reify Codec
    (alignment* [_] (apply max (map raw-alignment codecs)))
    (encode* [this data] (make-binary (map raw-encode codecs data)
                                   :align (raw-alignment this)))
    (decode* [this bin decoding-args]  ...)))
```

In the case of a tuple each element in the passed data is a different type.  The alignment on such a tuple must be the maximum alignment of all the passed types.  The encoding can be done by mapping codecs to their data.

#### Encoding a Struct

A struct is like a tuple, except each element is gotten by key instead of index.  Instead of getting a list of codecs, the struct takes a list of key-codec pairs.  This gives a clear relationship to keys and order.

```
(defn struct-impl [key-codec-pairs implicit-decoders]
  (let [key-order (map first key-codec-pairs)
        codecs (map second key-codec-pairs)
        ordered-values (fn [coll]
                        (map #(get coll %) key-order))]
    (reify Codec
      (alignment* [this] (apply max (map raw-alignment codecs)))
      (encode* [this data] (make-binary (zipmap key-order
                                                (map raw-encode codecs (ordered-values data)))
                                        :align (alignment* this)
                                        :order key-order))
      (decode* [this bin decoding-args] ...))))
```

The first thing to do in a struct is to destructure the key-codec-pairs.  By mapping first to the pair list, you get the individual keys.  By mapping second, you get the codec.

The ordered-values function takes a map, and returns a list of values that are gotten from the key-order.  This is the function that effectively converts a map into a tuple.

After this processing is done, alignment and encoding look very similar to the implementation of tuple.  The main difference is that before calling `make-binary` the result of the encoding is zipped together with the key-order.  This reconstructs a map that is returns by the `encode*` function.

### Composite Decoders

The difficulty with encoding complex data types, is that there is some runtime information that is usually needed.  However, that runtime information has been stripped out of binary data.  For that reason decoders need to accept some decoding-args, which can be used to give hints to a given codec. The decoding arguments are a map, with each codec having specific keys that can be passed.

#### Decoding an Array

An array takes two special decoding args.  The first, `::e/count` is passed to tell the array how many elements are in the array.  If this is not passed, then the array will assume that all remaining data is part of the array, and it will consume all bytes when decoding.

The other special decoding arg is `::e/child-args`.  This field is a function of the form `(fn [decoding-args index])` and returns a map of decoding args.  This gives callers a way to create custom decoding args that are passed to specific indicies of the array.  IF this is not included, no decoding arguments will be passed to the child codecs.

```
(defn array-impl 
  ([codec]
   (reify Codec
     ....
     (decode* [this bin decoding-args]
       (let [count (::count decoding-args) 
             child-args (or (::child-args decoding-args) (constantly nil)) 
             elem-args (if (some? count)
                         (map #(child-args decoding-args %) (range count))
                         (map #(child-args decoding-args %) (range)))]
         (reduce (fn [[data-accum current-rem] arg]
                   (let [[new-data bin-rem] (raw-decode codec current-rem arg)
                         result [(conj data-accum new-data)
                                 bin-rem]]
                     (if (empty? bin-rem)
                       (reduced result)
                       result)))
                 [[] bin]
                 elem-args))))))
```

The first thing is to parse the `decoding-args` to see if there is a special count or child args.  Once that is sorted out, the `elem-args` list is created.  This is a list of all decoding arguments that will be passed on to the child codecs.

After that, there is a complicated reduce that is used to calculate the return value.  Remember, the return value of `decode*` is a tuple of decoded data and the remaining binary.  So let's think about this problem iteratively

1. At the start the entire binary is remaining, and there is no decoded data
2. For each `elem-arg` call the `raw-decode` function with the remaining binary
3. Append the decoded data to a list, and the remaining binary from `raw-decode` the new remaining binary
4. If there are no more `elem-args` or there is no more binary data, then finish.  Otherwise move on to the next `elem-arg`

In this reduction, there is a call to the `raw-decode` function seen below.

```
(defn- raw-decode [codec-form bin decoding-args] 
  (let [args (s/conform (s/keys) (or decoding-args {}))
        codec (if (keyword? codec-form) 
                (reg-resolve codec-form)
                codec-form)]
    (if (s/invalid? args)
      (throw (ex-info "Invalid decoding args for raw decode" (s/explain-data (s/keys) decoding-args)))
      (decode* codec 
               (trim-to-alignment (alignment* codec) bin)
               args))))
```

There are a couple of things being done in this function.  First, any decoding arg is being conformed using spec.  This gives us some safety with out decoding arguments, becuase any globally registered spec will be detected using this.  Then the raw codec object is resolved, if need be.  The binary is then trimmed to the proper alignment before being decoded by the raw codec.

#### Decoding a Tuple

When decoding a tuple, we need to introduce the concept of an implicit decoder.  An implicit decoder is a function of the form `fn [data decoding-args]` that returns a decoding argument.  This allows a user to write a custom decoding argument based on the data that has been decoded thus far.

The `tuple-impl` is passed a map of implicit decoders.  The keys to this map is a index.  That way, when decoding, the codec can easily get the implicit decoder.  If there is no implicit decoder for a given index, nil is used.

Tuples also support the `::e/child-args` argument, which works exactly like the array.

```
(defn tuple-impl [codecs implicit-decoders]
  (reify Codec
    ...
    (decode* [this bin decoding-args] 
      (let [child-args (or (::child-args decoding-args) (constantly nil))]
        (reduce (fn [[data-accum current-rem] [i codec]]
                  (let [implicit-fn (get implicit-decoders i (constantly nil))
                        args (merge 
                               (child-args decoding-args i)
                               (implicit-fn data-accum decoding-args))
                        [new-data bin-rem] (raw-decode codec current-rem args)]
                    [(conj data-accum new-data)
                     bin-rem]))
                [[] bin]
                (map-indexed vector codecs))))))
```

This function works like the array decoder, with the small difference that it uses the implicit decoder map on every element in the tuple.

#### Decoding a Struct

Struct decoding works almost exactly like the tuple decoding except all functions take keys instead of indicies, and the result must be assoc'ed together instead of conj'ed

```
(defn struct-impl [key-codec-pairs implicit-decoders]
  (let [key-order (map first key-codec-pairs)
        codecs (map second key-codec-pairs)
        ordered-values (fn [coll]
                        (map #(get coll %) key-order))]
    (reify Codec
      ...
      (decode* [this bin decoding-args] 
        (let [child-args (or (::child-args decoding-args) (constantly nil))]
          (reduce (fn [[data-accum current-rem] [k codec]]
                    (let [implicit-fn (get implicit-decoders k (constantly nil))
                          args (merge 
                                 (child-args decoding-args k)
                                 (implicit-fn data-accum decoding-args))
                          [new-data bin-rem] (raw-decode codec current-rem args)]
                      [(assoc data-accum k new-data)
                       bin-rem]))
                  [{} bin]
                  (map vector key-order codecs)))))))
```

### The Spec Macros

Now for the final piece to tie this entire library together.  

## The BinaryCollection Protocol

__TODO__ Add in some documentation for the Binary Collection Protocol the `make-binary` function, and the `flatten` function
