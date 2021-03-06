giferly - a GIF 89a decoder
===========================

by Avik Das

Overview
--------

giferly is a standalone graphical program that decodes a GIF file in pure
Erlang, then displays the image using the Simple DirectMedia Layer (SDL).

![](https://raw.githubusercontent.com/avik-das/giferly/master/screenshot-stripes.png)
![](https://raw.githubusercontent.com/avik-das/giferly/master/screenshot-tomatos.png)

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
sudo apt install erlang erlang-esdl-dev

# Build the application
make

# Run the application with the demo input file
make run

# Run the application with a different input file. While many files have been
# tested, it's likely some file will expose a bug in the program.
make run IN=gfx/rgb-stripes.gif

# run the tests
make test
```

User Interaction
----------------

Pressing '`=`' (that is, '`+`' without pressing the Shift key) zooms into the
image, while pressing '`-`' zooms out. You cannot zoom out past 100%, and a
maximum zoom level is also set.

Pressing '`n`' or `SPACE` will move to the next frame of the animation if
relevant. Not all animation-related functionality is supported yet, so the
animation may have visual bugs, such as not clearing the image before moving to
the next frame.

Pressing '`q`' or `ESC` will quit the application.

Dependencies
------------

<dl>
  <dt><a href="http://www.erlang.org/download.html">Erlang</a></dt>
  <dd>In particular, the compiler and virtual machine are needed to build and run this application. As I am using Ubuntu 11.04 in developing this application, I have developed against Erlang R13B03, and as such have avoided any features available only in later releases. I have since tested on Ubuntu 18.04, Erlang/OTP 20.</dd>

  <dt><a href="http://esdl.sourceforge.net/">Esdl (Erlang SDL bindings)</a></dt>
  <dd>Used to render the decoded GIF onto the screen.</dd>
</dl>

Current Status
--------------

Given that I created this application to understand the GIF 89a format and
learn some Erlang, I feel that I've accomplished my goal. Thus, I *may*
incrementally add a feature or fix a bug in the future, but please feel free to
fork this project and update it.

Some of the problems include:

* Inability to automatically play animations. This is by design, so far. The
  frames of an animation are navigable by using the '`n`' or `SPACE` key.

* No support for advanced animation functionality: disposal methods,
  inter-frame delays, and looping. The latter two fall under the inability to
  automatically play animations.

* No support for interlacing.

* Inability to decode GIF 87a files. This is by design.

References
----------

<dl>
  <dt><a href="http://matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp">What's In A GIF</a></dt>
  <dd>the article that piqued my interest. Provides a very comprehensive, but approachable, guide to the GIF 89a format.</dd>

  <dt><a href="http://www.w3.org/Graphics/GIF/spec-gif89a.txt">Graphics Interchange Format Version 89a</a></dt>
  <dd>The official specification, dated 31 July 1990.</dd>

  <dt><a href="http://learnyousomeerlang.com/">Learn You Some Erlang For Great Good!</a></dt>
  <dd>The book I'm using to learn Erlang.</dd>
</dl>
