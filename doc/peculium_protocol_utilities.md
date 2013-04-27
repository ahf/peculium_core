

# Module peculium_protocol_utilities #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Bitcoin Protocol Utilities.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#atom_to_inv-1">atom_to_inv/1</a></td><td></td></tr><tr><td valign="top"><a href="#checksum-1">checksum/1</a></td><td></td></tr><tr><td valign="top"><a href="#command_to_atom-1">command_to_atom/1</a></td><td></td></tr><tr><td valign="top"><a href="#inv_to_atom-1">inv_to_atom/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="atom_to_inv-1"></a>

### atom_to_inv/1 ###


<pre><code>
atom_to_inv(X::<a href="#type-bitcoin_inv_atom">bitcoin_inv_atom()</a>) -&gt; {ok, <a href="#type-bitcoin_inv_integer">bitcoin_inv_integer()</a>} | {error, {invalid_inv_atom, any()}}
</code></pre>

<br></br>



<a name="checksum-1"></a>

### checksum/1 ###


<pre><code>
checksum(X::iolist()) -&gt; <a href="#type-bitcoin_checksum">bitcoin_checksum()</a>
</code></pre>

<br></br>



<a name="command_to_atom-1"></a>

### command_to_atom/1 ###


<pre><code>
command_to_atom(Command::binary()) -&gt; {ok, <a href="#type-bitcoin_command_atom">bitcoin_command_atom()</a>} | {error, {invalid_command_atom, any()}}
</code></pre>

<br></br>



<a name="inv_to_atom-1"></a>

### inv_to_atom/1 ###


<pre><code>
inv_to_atom(X::integer()) -&gt; {ok, <a href="#type-bitcoin_inv_atom">bitcoin_inv_atom()</a>} | {error, {invalid_inv_integer, any()}}
</code></pre>

<br></br>



