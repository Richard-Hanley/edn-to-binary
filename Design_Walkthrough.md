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

### Defining Primitives

## The BinaryCollection Protocol

## Composite Macros and Implicit Decoders
