Long Term Research Tasks
========================

These tasks all requires a fair share of knowledge of the codebase and requires
a lot of consideration and planning.

- Look into a protocol replacement and make Peculium act like a bridge between
  the current Bitcoin protocol network and this new protocol network.

  Some of the design requirements includes:

  - The protocol should not use variable width integers and variable length
    strings. We should define the max length of a string in the protocol.
    Sending a few more null-bytes on the wire to get a more simple protocol is
    worth it. This should also make it easier for other hackers to implement
    their own, much better, clients. We should encourage that.

  - Use a one-byte opcode to represent the message type. Using a 12 byte,
    null-padded, string is not something we want. Having access to 256
    different opcodes should be more than enough for everyone. Otherwise, we
    will have to do a Peculium protocol version 6 :-)

  - Timestamps shouldn't be a mixture between 32-bit and 64-bit values. We
    should use 64-bit values for timestamps everywhere.

  - It shouldn't be necessary for a client to know its own external IP address
    to perform a handshake with another client. The Satoshi client tries to
    detect its own IP by performing an HTTP request to a server which will reply
    with the IP. If, for some reason, we must know our IP, we should make the
    handshake response contain the IP.

  - Write a formal protocol specification that wont require other client
    implementers to look up code in competing implementations. The current
    specification for the Bitcoin protocol found on a Wiki page is rather ill
    written. Everything in the Peculium protocol should be formalized. It would
    be neat if we could get someone else to try to do an independent
    implementation.

Other interesting research sub-projects:

  - Is it worth compressing the data in the protocol?

  - Is it worth encrypting peer connections using SSL?
