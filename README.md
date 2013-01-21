This repository is my (Sukant Hajra's) personal fork of the source code for the
book [_Functional Programming in Scala_](http://manning.com/bjarnason/).  The
[original/official repository](https://github.com/pchiusano/fpinscala-exercises)
is hosted on GitHub.  Please have a look at the original repository too because
you may find it helpful in ways this one is not.


Using This Repository
---------------------

This code repository helps you work through the exercises in Manning's
[_Functional Programming in Scala_](http://manning.com/bjarnason/) book.
Without this book, the repository is much less instructive.  The book comes
highly recommended, so if you find the problems in this code base at all
interesting, please consider buying the book.

This code base builds with a Scala-specialized tool called
[Simple Build Tool (SBT)](http://www.scala-sbt.org).  You shouldn't have to
learn too much about SBT to dive into working with the book's exercises.

To use this code base, you'll need:

* a recent version of a Java runtime
* an online internet connection.

You don't even need to have a Scala installed on your computer.  SBT will pull
everything it needs from the network connection.

If on Window, run the following command to build the code for the first time:

    $ .\sbt.cmd

If on Mac or GNU/Linux:

    $ chmod a+x ./sbt
    $ ./sbt

This will download dependencies and launch SBT.  Once it is finished, you'll
get a prompt for an interactive shell from which you can issue commands to work
with your code.  You can exit from this shell with the `exit` command:

    > exit

There are two subprojects provided:

* "exercises", which has stubs for you to do your exercises in
* "answers", which has an example solution set.

The answers also have a number of inline comments to supplement the main text
of the book.

You can switch between projects with the `project` command.  And you compile
your code with the `compile` command.  SBT has an extremely useful feature to
run a command repeatedly each time a change is sensed on the filesystem.  You
do this by prefixing a command with "~".

So a typical way to use this code base is to change into the "exercises"
project and ask SBT to recompile the project upon every change:

    > project exercises
    > ~compile

If you keep the SBT terminal in view along side your editor of choice, you can
take advantage of rather fast compiler feedback for the work you're doing.

If you want to play around with your code in a Scala REPL, you can use the
`console` command from SBT:

    > console

This is like running a Scala REPL manually, except for you don't have to deal
with setting up the classpath with your compiled artifacts.  This classpath is
set up based upon the project you're in, so remember to perform a
`project exercises` or `project answers` command if you're not already in the
right project.


Why Such A Severe Fork?
-----------------------

This repository is independent from my GitHub fork of the official repository.
There's a lot of heavy changes in this repository, and I do not expect many of
them are appropriate candidates to push back to the original repository.

My intent this this repository is two-fold:

1. facilitate my own learning as I go through the _FP in Scala_ book
2. develop training material for others going through the book.

Regarding the first point, I'm actually doing solutions myself (the problems
are very well designed), and building my own answer set.  Then I'm merging good
ideas and observations from the official answer set back into my own.

The second point may be a bit of hubris on my part; there's a possibility the
original repository is still better.  My changes include some of the following:

* streamline the content to just exercises, answers, and some minimal SBT
* make exercises easier to follow from the book with more clear stubs
* provide more answers relevant to my own interests (like CPS and Scalaz usage)
* reformat code to help with readability
* stylize code in a way I'd advocate.

Some of the usability improvements are candidates for pushing up to the
official repository.  However a lot of the other the changes may be more
debatable, many stylistic.  I plan on using this repository professionally to
help newer Scala developers I'll be working with, so this is my way of sharing
my preferred style while they learn more FP in Scala.


-Sukant
