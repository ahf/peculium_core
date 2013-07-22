

# Module peculium_core_peer #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Peer Server.
Copyright (c)  2013 Alexander Færøy

__Behaviours:__ [`gen_server`](gen_server.md), [`ranch_protocol`](ranch_protocol.md).

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##

   This module contains a `gen_server` for representing a peer in the Bitcoin
peer-to-peer network.


We are using a single server to represent both incoming and outgoing
peers.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#send_message-3">send_message/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#test_connect-0">test_connect/0</a></td><td></td></tr><tr><td valign="top"><a href="#test_connect-1">test_connect/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVersion, State, Extra) -> any()`


<a name="connect-3"></a>

### connect/3 ###

`connect(Peer, Address, Port) -> any()`


<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`


<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Message, State) -> any()`


<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`


<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`


<a name="send_message-3"></a>

### send_message/3 ###

`send_message(Peer, Message, Arguments) -> any()`


<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`


<a name="start_link-4"></a>

### start_link/4 ###

`start_link(ListenerPid, Socket, Transport, Options) -> any()`


<a name="stop-1"></a>

### stop/1 ###

`stop(Peer) -> any()`


<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


<a name="test_connect-0"></a>

### test_connect/0 ###

`test_connect() -> any()`


<a name="test_connect-1"></a>

### test_connect/1 ###

`test_connect(Address) -> any()`


