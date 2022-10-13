
## How to use Roswell to build and share binaries

From the project root:

Run as a script:

    chmod +x roswell/kyma-images.ros
    ./roswell/kyma-images.ros

Build a binary:

    ros build roswell/kyma-images.ros

and run it:

    ./roswell/kyma-images

Or install it in ~/.roswell/bin:

    ros install roswell/kyma-images.ros

It creates the binary in ~/.roswell/bin/
Run it:

    ~/.roswell/bin/kyma-images [name]~&

Your users can install the script with ros install void404/kyma-images

Use `+Q` if you don't have Quicklisp dependencies to save startup time.
Use `ros build --disable-compression` to save on startup time and loose on application size.


## See

- https://github.com/roswell/roswell/wiki/
- https://github.com/roswell/roswell/wiki/Reducing-Startup-Time
