(ns edn-to-binary.usb
  (:require [clojure.spec.alpha :as s]
            [edn-to-binary.core :as e]))

(e/def ::bLength (e/nilable ::e/uint8))
(e/def ::bDescriptorType (e/nilable ::e/uint8))

(defn length-conformer [codec]
  (e/dependent-field ::bLength #(->> %
                                     (e/encode codec)
                                     (e/sizeof))))


(e/def ::bcdUSB ::e/uint16)
(e/def ::bDeviceClass ::e/uint8)
(e/def ::bDeviceSubClass ::e/uint8)
(e/def ::bDeviceProtocol ::e/uint8)
(e/def ::bMaxPacketSize ::e/uint8)
(e/def ::idVendor ::e/uint16)
(e/def ::idProduct ::e/uint16)
(e/def ::bcdDevice ::e/uint16)
(e/def ::iManufacturer ::e/uint8)
(e/def ::iProduct ::e/uint8)
(e/def ::iSerialNumber ::e/uint8)
(e/def ::bNumConfigurations ::e/uint8)

(e/def ::device (e/struct ::bLength
                          ::bDescriptorType
                          ::bDeviceClass
                          ::bDeviceSubClass
                          ::bDeviceProtocol
                          ::bMaxPacketSize
                          ::idVendor
                          ::idProduct
                          ::bcdDevice
                          ::iManufacturer
                          ::iProduct
                          ::iSerialNumber
                          ::bNumConfigurations))

(e/def ::wTotalLength ::e/uint16)
(e/def ::bNumInterfaces ::e/uint8)
(e/def ::bConfigurationValue ::e/uint8)
(e/def ::iConfiguration ::e/uint8)
(e/def ::bmAttributes ::e/uint8)
(e/def ::bMaxPower ::e/uint8)

(e/def ::config (e/struct ::bLength
                          ::bDescriptorType
                          ::wTotalLength
                          ::bNumInterfaces
                          ::bConfigurationValue
                          ::bmAttributes
                          ::bMaxPower))

(e/def ::bInterfaceNumber ::e/uint8)
(e/def ::bAlternateSetting ::e/uint8)
(e/def ::bNumEndpoints ::e/uint8)
(e/def ::bInterfaceClass ::e/uint8)
(e/def ::bInterfaceSubClass ::e/uint8)
(e/def ::bInterfaceProtocol ::e/uint8)
(e/def ::iInterface ::e/uint8)

(e/def ::interface (e/struct ::bLength
                             ::bDescriptorType
                             ::bInterfaceNumber
                             ::bAlternateSetting
                             ::bNumEndpoints
                             ::bInterfaceClass
                             ::bInterfaceSubClass
                             ::bInterfaceProtocol
                             ::iInterface))


(e/def ::bEndpointAddress ::e/uint8)
(e/def ::bmAttributes ::e/uint8)
(e/def ::wMaxPacketSize ::e/uint16)
(e/def ::bInterval ::e/uint8)

(e/def ::endpoint (e/struct ::bLength
                            ::bDescriptorType
                            ::bEndpointAddress
                            ::bmAttributes
                            ::wMaxPacketSize
                            ::bInterval))


(e/def ::bcdHID ::e/uint16)
(e/def ::bCountryCode ::e/uint8)
(e/def ::bNumDescriptors (e/nilable ::e/uint8))
(e/def ::wItemLength ::e/uint16)
(e/def ::reports (e/array (e/struct ::bDescriptorType
                                    ::wItemLength)))

(e/def ::hid 
  (e/and (e/struct ::bLength
                   ::bDescriptorType
                   ::bcdHID
                   ::bCountryCode
                   ::bNumDescriptors
                   ::reports)
         (e/dependent-field ::bNumDescriptors #(count (::reports %)))
         (e/constant-field ::bDescriptorType 0x21)
         (length-conformer ::hid)))

