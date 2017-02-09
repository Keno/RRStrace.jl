# RRStrace

Like `strace`, but operate on an [rr](https://github.com/mozilla/rr) recording.

# Usage

At the moment, this package relies on unreleased, dependencies (sorry).
You can find those in my GitHub, but I wouldn't recommend using this at this stage.
If you do manage to install the dependencies, use:

`julia -e 'Pkg.dir("RRStrace","bin","stracelatest.jl")'`

to strace the most recent recording.
