

# Module peculium_block_index #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Bitcoin Block Index Server.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#best_block_height-0">best_block_height/0</a></td><td>Get best block's height.</td></tr><tr><td valign="top"><a href="#best_block_index-0">best_block_index/0</a></td><td>Get best block index entry.</td></tr><tr><td valign="top"><a href="#exists-1">exists/1</a></td><td>Check if a given hash exists in the index.</td></tr><tr><td valign="top"><a href="#height_to_hash-1">height_to_hash/1</a></td><td>Get block hash from height.</td></tr><tr><td valign="top"><a href="#insert-1">insert/1</a></td><td>Insert block into the block index.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Start Block Index Server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="best_block_height-0"></a>

### best_block_height/0 ###


<pre><code>
best_block_height() -&gt; non_neg_integer()
</code></pre>

<br></br>


Get best block's height.
<a name="best_block_index-0"></a>

### best_block_index/0 ###

`best_block_index() -> any()`

Get best block index entry.
<a name="exists-1"></a>

### exists/1 ###


<pre><code>
exists(Hash::binary()) -&gt; boolean()
</code></pre>

<br></br>


Check if a given hash exists in the index.
<a name="height_to_hash-1"></a>

### height_to_hash/1 ###


<pre><code>
height_to_hash(Height::non_neg_integer()) -&gt; {ok, binary()} | {error, any()}
</code></pre>

<br></br>


Get block hash from height.
<a name="insert-1"></a>

### insert/1 ###

`insert(Block) -> any()`

Insert block into the block index.
<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

Start Block Index Server.
