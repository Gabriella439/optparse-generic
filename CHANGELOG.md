1.4.0

* BREAKING CHANGE: Add support for type-level default values
    * This is a breaking change because the various `parse*` typeclass methods
      now take an additional argument to support this feature

1.3.1

* Export `GenericParseRecord` and `getRecord{,PureWith}`

1.3.0

* BREAKING CHANGE: New `metavar` method for `ParseField` class
    * This field simplifies customizing `ParseField` instances
        * Now you usually only need to override `metavar` now or possibly also
          `readField`, whereas the default behavior for `parseField` should work
          more often
    * This is only a breaking change for data types that use the default
      implementation of `ParseField` but do not derive `Typeable`
    * You can migrate existing code that doesn't compile by just explicitly
      specifying what the `metavar` field should be
