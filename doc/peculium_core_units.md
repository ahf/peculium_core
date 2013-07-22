

# Module peculium_core_units #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Unit Utilities.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities to help converting to and from various
Bitcoin unit types.
<a name="types"></a>

## Data Types ##




### <a name="type-unit_type">unit_type()</a> ###



<pre><code>
unit_type() = <a href="peculium_core_types.md#type-unit_type">peculium_core_types:unit_type()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#convert-3">convert/3</a></td><td>Convert a given number from the input unit to the output unit.</td></tr><tr><td valign="top"><a href="#factor-1">factor/1</a></td><td>Returns the factor of a given unit.</td></tr><tr><td valign="top"><a href="#stringify-1">stringify/1</a></td><td>Convert a unit to a binary.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="convert-3"></a>

### convert/3 ###


<pre><code>
convert(Value::float(), InputUnit::<a href="#type-unit_type">unit_type()</a>, OutputUnit::<a href="#type-unit_type">unit_type()</a>) -&gt; float()
</code></pre>

<br></br>


Convert a given number from the input unit to the output unit.
<a name="factor-1"></a>

### factor/1 ###


<pre><code>
factor(Unit::<a href="#type-unit_type">unit_type()</a>) -&gt; float()
</code></pre>

<br></br>


Returns the factor of a given unit.
<a name="stringify-1"></a>

### stringify/1 ###


<pre><code>
stringify(Unit::<a href="#type-unit_type">unit_type()</a>) -&gt; binary()
</code></pre>

<br></br>


Convert a unit to a binary.
