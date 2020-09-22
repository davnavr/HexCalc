# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.0] - 2020-09-19
### Added
- `BigInteger` is now used as underlying integer type instead of Int64, allowing for a larger range of possible values
- Hexadecimal and Binary integers are no longer negative when the sign bit is set
- Negative hexadecimal and binary integers no longer set the sign bit in the output
- Added conversion functions that change the base of integers
- Added `pow` function to raise integers to a power

## [0.2.0] - 2020-09-09
### Added
- Help subcommand can now list all available operators or display a description for each

## [0.1.0] - 2020-09-06
### Added
- CHANGELOG file for determining version when publishing using the GitHub Action workflow
- Basic calculator features
- GitHub Action workflow for publishing releases to NuGet
