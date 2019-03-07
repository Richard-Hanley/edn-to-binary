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

However, this is a task that is easier said than done. Spec goes out of it's way to hide it's data, and the Spec protocol functions operate on arguments that have been heavily processed by some macros.  In the end I realized it would be much simpler to create a parallel system, and then attach everything together with some macros. To handle this attachement system a few things needed to be done. 

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

The primitive codecs and specs are relatively straightfoward.  However, to reduce the amount of boilerplate and there are a few macros that make it pretty easy to create new primitives.

### Specifying Primitives

A primitive is considered valid so long as it is a valid numerical type and within a valid range.

## Composite Macros and Implicit Decoders

## The BinaryCollection Protocol
