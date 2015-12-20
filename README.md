Gonito platform
===============

[Gonito](http://gonito.net) (pronounced _ɡɔ̃ˈɲitɔ_) is a Kaggle-like
platform for machine learning competitions (disclaimer: Gonito is
neither affiliated with nor endorsed by [Kaggle](https://www.kaggle.com)).


What's so special about Gonito:

  * free & open-source (AGPL), you can use it your own, in your
    company, at your university, etc.
  * git-based (challenges and solutions are submitted only with git).

See the home page (and an instance of Gonito) at http://gonito.net .

Installation
------------

[Gonito](http://gonito.net) is written in [Haskell](https://www.haskell.org) and uses
[Yesod Web Framework](http://www.yesodweb.com/), but all you need is
just [the Stack tool](https://github.com/commercialhaskell/stack). See https://github.com/commercialhaskell/stack
for instruction how to install Stack on your computer.

By default, Gonito uses [Postgresql](http://www.postgresql.org/), so it needs to be installed and running at your computer.

After installing Stack:

    createdb -E utf8 gonito
    git clone git://gonito.net/geval
    git clone git://gonito.net/gonito
    cd gonito
    stack setup
    stack build
    stack exec yesod devel

The last command will start the Web server with Gonito (go to
http://127.0.0.1:3000 in your browser).

Authors
-------

* Filip Graliński
