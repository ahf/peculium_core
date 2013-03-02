Peculium - A Bit of Money
=========================

Peculium is a Bitcoin client implemented in the Erlang programming language.

Getting Started
---------------

Make sure you have a working Erlang environment running on your machine. If you
are a Mac OS X user using Homebrew, you can install the Erlang distribution using:

    $ brew install erlang

We are also depending on Basho's Rebar build-utility. You should be able to
manually install it using:

    $ git clone git://github.com/basho/rebar.git
    $ cd rebar
    $ make

Then copy the `rebar` executable into somewhere in your UNIX `$PATH`.

You should now be able to download Peculium and build it using:

    $ git clone git://github.com/ahf/peculium.git
    $ cd peculium

Download our dependencies:

    $ make get-deps

And finally, build Peculium itself:

    $ make
