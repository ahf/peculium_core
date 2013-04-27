

# Module peculium_leveldb #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Peculium LevelDB Utilities.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Close LevelDB database.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>Delete element.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Get value from a given key.</td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td>Open LevelDB database.</td></tr><tr><td valign="top"><a href="#open_options-1">open_options/1</a></td><td>Get default database opening options.</td></tr><tr><td valign="top"><a href="#put-3">put/3</a></td><td>Insert element.</td></tr><tr><td valign="top"><a href="#read_options-0">read_options/0</a></td><td>Get default database read options.</td></tr><tr><td valign="top"><a href="#write_options-0">write_options/0</a></td><td>Get default database write options.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###


<pre><code>
close(Database::<a href="eleveldb.md#type-db_ref">eleveldb:db_ref()</a>) -&gt; ok | {error, any()}
</code></pre>

<br></br>


Close LevelDB database.
<a name="delete-2"></a>

### delete/2 ###


<pre><code>
delete(Database::<a href="eleveldb.md#type-db_ref">eleveldb:db_ref()</a>, Key::binary()) -&gt; ok | {error, any()}
</code></pre>

<br></br>


Delete element.
<a name="get-2"></a>

### get/2 ###


<pre><code>
get(Database::<a href="eleveldb.md#type-db_ref">eleveldb:db_ref()</a>, Key::binary()) -&gt; {ok, binary()} | not_found | {error, any()}
</code></pre>

<br></br>


Get value from a given key.
<a name="open-1"></a>

### open/1 ###


<pre><code>
open(Path::string()) -&gt; {ok, <a href="eleveldb.md#type-db_ref">eleveldb:db_ref()</a>} | {error, any()}
</code></pre>

<br></br>


Open LevelDB database.
<a name="open_options-1"></a>

### open_options/1 ###


<pre><code>
open_options(CacheSize::non_neg_integer()) -&gt; <a href="eleveldb.md#type-open_options">eleveldb:open_options()</a>
</code></pre>

<br></br>


Get default database opening options.
<a name="put-3"></a>

### put/3 ###


<pre><code>
put(Database::<a href="eleveldb.md#type-db_ref">eleveldb:db_ref()</a>, Key::binary(), Value::binary()) -&gt; ok | {error, any()}
</code></pre>

<br></br>


Insert element.
<a name="read_options-0"></a>

### read_options/0 ###


<pre><code>
read_options() -&gt; <a href="eleveldb.md#type-read_options">eleveldb:read_options()</a>
</code></pre>

<br></br>


Get default database read options.
<a name="write_options-0"></a>

### write_options/0 ###


<pre><code>
write_options() -&gt; <a href="eleveldb.md#type-write_options">eleveldb:write_options()</a>
</code></pre>

<br></br>


Get default database write options.
