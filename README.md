giferly - a GIF 89a decoder
===========================

by Avik Das

Overview
--------

giferly is a standalone graphical program that decodes a GIF file in pure
Erlang, then displays the image using the Simple DirectMedia Layer (SDL).

Rationale
---------

This is my attempt to learn both the GIF 89a format and Erlang. The project
started by my encountering an article describing the GIF format, in essence as
a summary of some of the more fundamental parts of the official specification
of the format. While this task would have been fairly straightforward in Ruby,
C or some other language I am familiar with, I decided to implement the decoder
in Erlang in order to learn the language.

The entire parser is implemented from scratch in Erlang. While Erlang may not
necessarily be the ideal choice for this task, it is an interesting exercise to
attempt this in a functional style, and taking advantage of Erlang's binary
data manipulation features.

Quick Start
-----------

```sh
# Install the necessary dependencies (example given for Ubuntu)
sudo apt-get install erlang erlang-esdl-dev

# build the application
make

# run the application with the demo input file
make run

# run the application with a different input file
# note that there are many bugs, so many files won't work
make run IN=gfx/rgb-stripes.gif
```

User Interaction
----------------

Pressing '`=`' (that is, '`+`' without pressing the Shift key) zooms into the
image, while pressing '`-`' zooms out. You cannot zoom out past 100%, and a
maximum zoom level is also set.

Pressing '`n`' or `SPACE` is currently set to move to the next frame of the
animation if relevant, but given the fact that I am unable to load many images,
this feature is untested.

Pressing '`q`' or `ESC` will quit the application.

Dependencies
------------

<dt>
  <dt>[Erlang](http://www.erlang.org/download.html)</dt>
  <dd>In particular, the compiler and virtual machine are needed to build and run this application. As I am using Ubuntu 11.04 in developing this application, I have developed against Erlang R13B03, and as such have avoided any features available only in later releases.</dd>

  <dt>[Esdl (Erlang SDL bindings)](http://esdl.sourceforge.net/)</dt>
  <dd>Used to render the decoded GIF onto the screen. The Makefile assumes the library files live inside `/usr/lib/esdl`, which is where Ubuntu's `erlang-esdl` package (and presumably Debian's) installs the library. Change the Makefile if you install it elsewhere.</dd>
</dl>

Current Status
--------------

There are many bugs remaining in the application. Given that I created this
application to understand the GIF 89a format and learn some Erlang, I feel that
I've accomplished my goal. Thus, I *may* incrementally add a feature or fix a
bug in the future, but please feel free to fork this project and update it.

Some of the problems include:

* Inability to decode GIF files. The bug might be in the LZW decompression
  routines, but given the amount of data that needs to be processed, trying to
  step through the execution of these routines is quite tedious.

* Untested features: local color table support, whether an animated gif can
  even be parsed.

* No support for interlacing.

Additionally:

* Inability to decode GIF 87a files. This is by design.

* Inability to display animations. Again, this is by design, so far.
  Technically, the frames of an animation *should* be navigable by using the
  '`n`' or `SPACE` key, but this feature is untested.

References
----------

<dl>
  <dt>[What's In A GIF](http://matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp)</dt>
  <dd>the article that piqued my interest. Provides a very comprehensive, but approachable, guide to the GIF 89a format.</dd>

  <dt>[Graphics Interchange Format Version 89a](http://www.w3.org/Graphics/GIF/spec-gif89a.txt)</dt>
  <dd>The official specification, dated 31 July 1990.</dd>

  <dt>[Learn You Some Erlang For Great Good!](http://learnyousomeerlang.com/)</dt>
  <dd>The book I'm using to learn Erlang.</dd>
</dl>
