# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-10-18

### Added
- First open source release of kiss_cache
- Simple ETS-based cache with timed expiry
- Basic put/get/delete operations with minimal overhead
- Lazy cleanup without background processes
- Serialized fetch operations to prevent thundering herd problems
- Zero external dependencies (uses only Erlang/OTP stdlib)
- Comprehensive documentation and examples
- Unit tests
- MIT License

### Notes
- This library has been used in production at CrankWheel since 2021
- Battle tested in high-traffic, high-contention scenarios
- Now available as an open source package on Hex.pm
