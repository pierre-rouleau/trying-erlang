===============================
Writing a TCP-based RPC Service
===============================

Here I implement the TCP-based RPC service described in chapter 3 of the
book `Erlang and OTP in Action`_.  For code related to this book, I'm putting
all Erlang source code inside a common root.  I'm implementing code that is
from the book, but with the tools I have and making any modifications that
reflect what I have learned elsewhere.

Most of the code here is based on the code presented in the book
(which is located in the `GitHub Erlang and OTP in Action`_ repository.)


.. _Erlang and OTP in Action:                     https://www.manning.com/books/erlang-and-otp-in-action
.. _GitHub Erlang and OTP in Action:              https://github.com/erlware/Erlang-and-OTP-in-Action-Source


Commit History
==============

First version
-------------

I created the first version of the file, a gen-server behaviour empty
template, by using Emacs Erlang gen-server skeleton, which I invoked in my
`PEL Emacs package support for Erlang`_ by typing ``<f12> <f12> M-g``.
Then I added an attribution note.
The template places a copyright with my name on it although I didn't do much
at this point.  I'll leave it there, at least for the moment. At least I put
an attribution note.  I'll have to check if EDoc supports attribution in the
markup.

So I'll need to read:

- `EDoc User's Guide`_, for the notation.
- `EDoc man page`_ to see the commands used to invoke it.

I'll also check if it's possible to parse and verify Edoc syntax directly
within Emacs.

At first glance, we can already see that the generated code uses ``-spec``
declarations instead of the Edoc ``@spec`` statements that were used in the
book example.  This is better: Edoc can now extract `type specifications`_ from
Erlang source code.  No need to duplicate this information in Edoc-markup comments!


.. _EDoc man page:     https://erlang.org/doc/apps/edoc/chapter.html
.. _EDoc User's Guide: https://erlang.org/doc/apps/edoc/users_guide.html
.. _type specifications: https://erlang.org/doc/reference_manual/typespec.html
.. _PEL Emacs package support for Erlang: https://github.com/pierre-rouleau/pel/blob/master/doc/pdf/pl-erlang.pdf


-----------------------------------------------------------------------------
