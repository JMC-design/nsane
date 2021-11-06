# nsane
Common Lisp implementation of the SANE network protocol

This package is NOT meant to be :USEd
This is not a frontend, it is just an implementation of the protocol in common lisp so anybody can write a frontend using it.
There is one additional function besides the protocol, RETRIEVE-SCAN that will retrieve and return a scan as vector. 

nsane:\*socket\* needs to be bound by a socket returned by nsane:init before any other protocol functions are used.

Functions follow the naming convention of the protocol with SANE_NET_ dropped from the function and then kebobbed, e.g. SANE_NET_INIT becomes nsane:init .
