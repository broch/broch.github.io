---
title: Using CPRG Random Number Generators in a Web Application
tags: haskell,crypto
date: 2015-02-06
thumb: placeholder.png
author: Luke
---

When I started writing the `jose-jwt` library I quickly ran into the issue of how best to manage random number generators (RNGs) in Haskell programs. In a language like Java, `SecureRandom` is always available from anywhere in your program and you can just grab some more bytes from it whenever you choose. In Haskell, the RNG is of course immutable and you get a new one back when when you request data from it. Accidentally use the same one twice and you'll get the same data again. We need to thread the RNG values through our function calls and out the other end and so RNGs are often used to illustrate how to use the state monad, which takes care of this for you. In a simple example, there is usually a call to initialize the generator, then a few calls to use it, then we're done. However in a multi-threaded program, such as a web application, the RNG still has to "live" somewhere when it isn't being called. At the time I wasn't sure how to do this and asked [this Stackoverflow question][so-rng-question].

[so-rng-question]: http://stackoverflow.com/questions/16024461/managing-cryptographic-random-number-generators-in-a-haskell-web-application

Motivated by original question on stack overflow. Measure time taken, run tests and see if a pool is faster.
