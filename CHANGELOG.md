1.4.4

* `readIntegralBounded`: use `metavar` in error message

1.4.3

* Export internal `readIntegralBounded` utility
* Build against `optparse-applicative-0.16.0.0`

1.4.2

* New `unwrap` function
    * This is the underlying utility that powers
      `unwrap{Record,RecordPure,WithHelp}`

1.4.1

* Fix broken haddocks

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
