

# Module peculium_core_utilities #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Peculium Utilities
This module contains various utilities used in Peculium.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bin2hex-1">bin2hex/1</a></td><td>Convert binary data to hex.</td></tr><tr><td valign="top"><a href="#expand_homedir-1">expand_homedir/1</a></td><td>Returns the path with the homedir expanded.</td></tr><tr><td valign="top"><a href="#find_last-2">find_last/2</a></td><td>Returns the last element of a given list that matches the given predicate.</td></tr><tr><td valign="top"><a href="#hex2bin-1">hex2bin/1</a></td><td>Convert hex to binary.</td></tr><tr><td valign="top"><a href="#parallel_map-2">parallel_map/2</a></td><td>Applies the function, Fun, to each element of List in parallel and
returns the result.</td></tr><tr><td valign="top"><a href="#repeat-2">repeat/2</a></td><td>Repeat a function N times and return the list of return values.</td></tr><tr><td valign="top"><a href="#reverse-1">reverse/1</a></td><td>Reverse a given binary.</td></tr><tr><td valign="top"><a href="#strip-2">strip/2</a></td><td>Strip the pattern, Pattern, from the given Subject.</td></tr><tr><td valign="top"><a href="#timestamp-0">timestamp/0</a></td><td>Returns current UNIX epoch timestamp.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bin2hex-1"></a>

### bin2hex/1 ###


<pre><code>
bin2hex(Data::binary()) -&gt; binary()
</code></pre>

<br></br>


Convert binary data to hex.
<a name="expand_homedir-1"></a>

### expand_homedir/1 ###


<pre><code>
expand_homedir(Path::string()) -&gt; string()
</code></pre>

<br></br>


Returns the path with the homedir expanded.
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
hex2bin(Data::string()) -&gt; binary()
</code></pre>

<br></br>


Convert hex to binary.
<a name="parallel_map-2"></a>

### parallel_map/2 ###


<pre><code>
parallel_map(Fun::fun((X::any()) -&gt; any()), List::[term()]) -&gt; [term()]
</code></pre>

<br></br>


Applies the function, Fun, to each element of List in parallel and
returns the result. The result shares the same order as the input list.
<a name="repeat-2"></a>

### repeat/2 ###


<pre><code>
repeat(N::non_neg_integer(), Fun::fun(() -&gt; term())) -&gt; [term()]
</code></pre>

<br></br>


Repeat a function N times and return the list of return values.
<a name="reverse-1"></a>

### reverse/1 ###


<pre><code>
reverse(Data::binary()) -&gt; binary()
</code></pre>

<br></br>


Reverse a given binary.
<a name="strip-2"></a>

### strip/2 ###


<pre><code>
strip(Subject::binary(), Pattern::binary()) -&gt; binary()
</code></pre>

<br></br>


Strip the pattern, Pattern, from the given Subject.
<a name="timestamp-0"></a>

### timestamp/0 ###


<pre><code>
timestamp() -&gt; non_neg_integer()
</code></pre>

<br></br>


Returns current UNIX epoch timestamp.
