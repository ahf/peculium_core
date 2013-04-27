

# Module peculium_utilities #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Various Peculium Utilities.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bin2hex-1">bin2hex/1</a></td><td></td></tr><tr><td valign="top"><a href="#find_last-2">find_last/2</a></td><td>Returns the last element of a given list that matches the given predicate.</td></tr><tr><td valign="top"><a href="#hex2bin-1">hex2bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#parallel_map-2">parallel_map/2</a></td><td></td></tr><tr><td valign="top"><a href="#reverse-1">reverse/1</a></td><td></td></tr><tr><td valign="top"><a href="#strip-2">strip/2</a></td><td></td></tr><tr><td valign="top"><a href="#timestamp-0">timestamp/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bin2hex-1"></a>

### bin2hex/1 ###


<pre><code>
bin2hex(Bin::binary()) -&gt; binary()
</code></pre>

<br></br>



<a name="find_last-2"></a>

### find_last/2 ###


<pre><code>
find_last(Pred::fun((X::term()) -&gt; boolean()), List::[term()]) -&gt; term() | not_found
</code></pre>

<br></br>


Returns the last element of a given list that matches the given predicate.
<a name="hex2bin-1"></a>

### hex2bin/1 ###


<pre><code>
hex2bin(Rest::string()) -&gt; binary()
</code></pre>

<br></br>



<a name="parallel_map-2"></a>

### parallel_map/2 ###

`parallel_map(Fun, List) -> any()`


<a name="reverse-1"></a>

### reverse/1 ###


<pre><code>
reverse(Binary::binary()) -&gt; binary()
</code></pre>

<br></br>



<a name="strip-2"></a>

### strip/2 ###


<pre><code>
strip(Subject::binary(), Pattern::binary()) -&gt; binary()
</code></pre>

<br></br>



<a name="timestamp-0"></a>

### timestamp/0 ###


<pre><code>
timestamp() -&gt; non_neg_integer()
</code></pre>

<br></br>



