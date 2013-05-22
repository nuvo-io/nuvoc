nuvoc
=====

nuvoc ia compiler and serialisation/deserialization framework. It
allows you to specify your types directly using Scala case classes and
then generates very efficient serializers and deserializers.

More information can be found at http://nuvo.io/nuvoc.html

Compile
=======

$ sbt compile


Package
=======
To package nuvoc into an executable jar simple do the following:

$ sbt assembly


Install
=======
The simplest way to install nuvoc is to generate the assembly and then to add nuvoc/bin to your executable search path (usually, the PATH environment variable).

Once you have done this, from your home directory do:

$ nuvoc
USAGE nuvoc <scala-source-file> [out-dir]

And you should see the nuvoc help line
