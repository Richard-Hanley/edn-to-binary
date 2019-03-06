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

## The BinaryCollection Protocol

## Spec Macros and Implicit Decoders
