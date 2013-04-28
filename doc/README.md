

# Peculium - An Erlang Bitcoin Client #

Copyright (c) 2013 Fearless Hamster Solutions


__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

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

And finally, build Peculium itself:

```
$ make
```

To start Peculium and attach an Erlang shell:

```
$ make console
```

Testing
-------

Peculium uses `EUnit` and `triq` for unit and property-based testing. To run
the test suites use:

```
$ make test
$ make qc
```

Documentation
-------------

Every function in the Peculium source code must have a type-specification and a
documentation entry. Peculium uses `edoc` for documentation and generates
Github friendly markdown based on the output. To generate the documentation
use:

```
$ make doc
```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="peculium.md" class="module">peculium</a></td></tr>
<tr><td><a href="peculium_app.md" class="module">peculium_app</a></td></tr>
<tr><td><a href="peculium_base58.md" class="module">peculium_base58</a></td></tr>
<tr><td><a href="peculium_block.md" class="module">peculium_block</a></td></tr>
<tr><td><a href="peculium_block_header.md" class="module">peculium_block_header</a></td></tr>
<tr><td><a href="peculium_block_index.md" class="module">peculium_block_index</a></td></tr>
<tr><td><a href="peculium_block_index_entry.md" class="module">peculium_block_index_entry</a></td></tr>
<tr><td><a href="peculium_block_locator.md" class="module">peculium_block_locator</a></td></tr>
<tr><td><a href="peculium_block_store.md" class="module">peculium_block_store</a></td></tr>
<tr><td><a href="peculium_config.md" class="module">peculium_config</a></td></tr>
<tr><td><a href="peculium_crypto.md" class="module">peculium_crypto</a></td></tr>
<tr><td><a href="peculium_difficulty.md" class="module">peculium_difficulty</a></td></tr>
<tr><td><a href="peculium_inv.md" class="module">peculium_inv</a></td></tr>
<tr><td><a href="peculium_json.md" class="module">peculium_json</a></td></tr>
<tr><td><a href="peculium_leveldb.md" class="module">peculium_leveldb</a></td></tr>
<tr><td><a href="peculium_merkle_tree.md" class="module">peculium_merkle_tree</a></td></tr>
<tr><td><a href="peculium_messages.md" class="module">peculium_messages</a></td></tr>
<tr><td><a href="peculium_network.md" class="module">peculium_network</a></td></tr>
<tr><td><a href="peculium_network_address.md" class="module">peculium_network_address</a></td></tr>
<tr><td><a href="peculium_peer.md" class="module">peculium_peer</a></td></tr>
<tr><td><a href="peculium_protocol.md" class="module">peculium_protocol</a></td></tr>
<tr><td><a href="peculium_protocol_types.md" class="module">peculium_protocol_types</a></td></tr>
<tr><td><a href="peculium_protocol_utilities.md" class="module">peculium_protocol_utilities</a></td></tr>
<tr><td><a href="peculium_script.md" class="module">peculium_script</a></td></tr>
<tr><td><a href="peculium_sup.md" class="module">peculium_sup</a></td></tr>
<tr><td><a href="peculium_transaction.md" class="module">peculium_transaction</a></td></tr>
<tr><td><a href="peculium_transaction_input.md" class="module">peculium_transaction_input</a></td></tr>
<tr><td><a href="peculium_transaction_outpoint.md" class="module">peculium_transaction_outpoint</a></td></tr>
<tr><td><a href="peculium_transaction_output.md" class="module">peculium_transaction_output</a></td></tr>
<tr><td><a href="peculium_triq_domains.md" class="module">peculium_triq_domains</a></td></tr>
<tr><td><a href="peculium_units.md" class="module">peculium_units</a></td></tr>
<tr><td><a href="peculium_utilities.md" class="module">peculium_utilities</a></td></tr>
<tr><td><a href="peculium_version.md" class="module">peculium_version</a></td></tr></table>

