

# Peculium - An Erlang Bitcoin Client #

Copyright (c) 2013 Alexander Færøy


__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

Peculium is an experimental Bitcoin client implemented in the Erlang programming language.


### <a name="Warning">Warning</a> ###

Peculium is **beta quality software** and needs proper testing. Do not move your
entire Bitcoin fortune from your current client to Peculium.

We will remove this warning once we believe this software is of good enough
quality to be a replacement for the Satoshi client.


### <a name="Getting_Started">Getting Started</a> ###

Make sure you have a working Erlang environment running on your machine. If you
are a Mac OS X user using Homebrew, you can install the Erlang distribution using:

```
$ brew install erlang
```

We are also depending on Basho's Rebar build-utility. You should be able to
manually install it using:

```
$ git clone git://github.com/basho/rebar.git
$ cd rebar
$ make
```

Then copy the rebar executable into somewhere in your UNIX path.

You should now be able to download Peculium and build it using:

```
$ git clone git://github.com/ahf/peculium.git
$ cd peculium
```

Download our dependencies:

```
$ make get-deps
```

Compile our dependencies and Peculium itself:

```
$ make build-deps
```

From now on, you can (re)build Peculium using:

```
$ make
```

To start Peculium and attach an Erlang shell:

```
$ make console
```


### <a name="Testing">Testing</a> ###

Peculium uses `EUnit` and `triq` for unit and property-based testing. To run
the test suites use:

```
$ make test
```


### <a name="Documentation">Documentation</a> ###

Every function in the Peculium source code must have a type-specification and a
documentation entry. Peculium uses `edoc` for documentation and generates
Github friendly markdown based on the output. To generate the documentation
use:

```
$ make doc
```


### <a name="Community">Community</a> ###

All Peculium hackers and users are welcome to join `#peculium` on the Freenode
IRC network.


### <a name="Contributors">Contributors</a> ###

Anyone who has contributed to Peculium should add themselves here. No change is
too little to be put on this list.

- [Alexander Færøy](https://github.com/ahf).
- [Jesper Louis Andersen](https://github.com/jlouis).


### <a name="Quirks">Quirks</a> ###

This secion will keep you updated on various quirks that you should take into
account when using Peculium.


#### <a name="Use_NTP">Use NTP</a> ####

Like a lot of other software out there, the Bitcoin system heavily depends upon
having access to a correct time source. The Satoshi client tries to use the
Bitcoin network to calculate an adjusted time whereas Peculium don't. Peculium
requires that you keep your clock synchronized.

If you do not keep your clock in sync strange things might happen. For
instance, Peculium might start thinking that certain perfectly valid blocks are
invalid because of this.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core.md" class="module">peculium_core</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_app.md" class="module">peculium_core_app</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_base58.md" class="module">peculium_core_base58</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_block.md" class="module">peculium_core_block</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_block_header.md" class="module">peculium_core_block_header</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_block_index.md" class="module">peculium_core_block_index</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_block_index_entry.md" class="module">peculium_core_block_index_entry</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_block_locator.md" class="module">peculium_core_block_locator</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_block_store.md" class="module">peculium_core_block_store</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_config.md" class="module">peculium_core_config</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_crypto.md" class="module">peculium_core_crypto</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_difficulty.md" class="module">peculium_core_difficulty</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_inv.md" class="module">peculium_core_inv</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_leveldb.md" class="module">peculium_core_leveldb</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_merkle_tree.md" class="module">peculium_core_merkle_tree</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_messages.md" class="module">peculium_core_messages</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_network.md" class="module">peculium_core_network</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_network_address.md" class="module">peculium_core_network_address</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_peer.md" class="module">peculium_core_peer</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_protocol.md" class="module">peculium_core_protocol</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_protocol_types.md" class="module">peculium_core_protocol_types</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_protocol_utilities.md" class="module">peculium_core_protocol_utilities</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_script.md" class="module">peculium_core_script</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_sup.md" class="module">peculium_core_sup</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_transaction.md" class="module">peculium_core_transaction</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_transaction_input.md" class="module">peculium_core_transaction_input</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_transaction_outpoint.md" class="module">peculium_core_transaction_outpoint</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_transaction_output.md" class="module">peculium_core_transaction_output</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_triq.md" class="module">peculium_core_triq</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_types.md" class="module">peculium_core_types</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_units.md" class="module">peculium_core_units</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_utilities.md" class="module">peculium_core_utilities</a></td></tr>
<tr><td><a href="https://github.com/ahf/peculium_core/blob/master/doc/peculium_core_version.md" class="module">peculium_core_version</a></td></tr></table>

