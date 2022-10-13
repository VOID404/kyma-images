# kyma-images

App for bumping kyma images before release

# Usage

Run from sources:

    make run
    # aka sbcl --load run.lisp

choose your lisp:

    LISP=ccl make run

or build and run the binary:

```
$ make build
$ ./kyma-images [path-to-cloned-kyma]
```

## Roswell integration

Roswell is an implementation manager and [script launcher](https://github.com/roswell/roswell/wiki/Roswell-as-a-Scripting-Environment).

A POC script is in the roswell/ directory.

Your users can install the script with `void404/kyma-images`.
