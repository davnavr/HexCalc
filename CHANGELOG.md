# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.5.1] - 2020-12-23
### Fixed
- Fix bug where digits would disappear for hexadecimal and binary numbers.

## [0.5.0] - 2020-11-20
### Added
- Update to .NET 5
- Error message that is printed when a division by zero occurs

## [0.4.0] - 2020-09-26
### Added
- `ans` keyword that refers to the value of the previous evaluated expression
- `listvars` command that lists all variables assigned with a non-zero value
- Assignment of variables (variable names must start with an uppercase letter)
- Usage of variables in expressions

## [0.3.0] - 2020-09-22
### Added
- `BigInteger` is now used as underlying integer type instead of Int64, allowing for a larger range of possible values
- Hexadecimal and Binary integers are no longer negative when the sign bit is set
- Negative hexadecimal and binary integers no longer set the sign bit in the output
- Added conversion functions that change the base of integers
- Added `pow` function to raise integers to a power
- Added `abs` function to get the absolute value of integers

## [0.2.0] - 2020-09-09
### Added
- Help subcommand can now list all available operators or display a description for each

## [0.1.0] - 2020-09-06
### Added
- CHANGELOG file for determining version when publishing using the GitHub Action workflow
- Basic calculator features
- GitHub Action workflow for publishing releases to NuGet
