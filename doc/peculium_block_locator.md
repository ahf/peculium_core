

# Module peculium_block_locator #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


       Bitcoin Block Locator Utilities.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------
<a name="types"></a>

## Data Types ##




### <a name="type-block_locator">block_locator()</a> ###



<pre><code>
block_locator() = [binary()]
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_best_block-0">from_best_block/0</a></td><td>Create block locator from the best block.</td></tr><tr><td valign="top"><a href="#from_height-1">from_height/1</a></td><td>Create block locator object from a given height.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_best_block-0"></a>

### from_best_block/0 ###


<pre><code>
from_best_block() -&gt; <a href="#type-block_locator">block_locator()</a>
</code></pre>

<br></br>


Create block locator from the best block.
<a name="from_height-1"></a>

### from_height/1 ###


<pre><code>
from_height(Height::non_neg_integer()) -&gt; {ok, <a href="#type-block_locator">block_locator()</a>} | {error, any()}
</code></pre>

<br></br>


Create block locator object from a given height.
