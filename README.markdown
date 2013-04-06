Peculium - A Bit of Money
=========================

Peculium is an experimental Bitcoin client implemented in the Erlang programming language.

Warning
-------

Peculium is **beta quality software** and needs proper testing. Do not move your
entire Bitcoin fortune from your current client to Peculium.

We will remove this warning once we believe this software is of good enough
quality to be a replacement for the Satoshi client.

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
