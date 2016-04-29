# ocplib-concur

A wrapper on top of Lwt and (partially) Async to implement basic
concurrent applications.

Currently, the library provides an easy way to:
* create a client-server application
* manage spawned processes
* start timers

## Build and Install

You will need:
* ocp-build
* lwt
* async

All of them can be installed using:
```
make opam-deps
```

You can then proceed as usually:

```
./configure
make
make install
```

## Usage

This package creates two libraries:
* ocplib-concur-lwt
* ocplib-concur-async (only if async is installed)

Both libraries implement a module `Concur`. Examples of using
`Concur` are available in the `tests` directory.
