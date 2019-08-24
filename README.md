Gonito platform
===============

[Gonito](https://gonito.net) (pronounced _ɡɔ̃ˈɲitɔ_) is a Kaggle-like
platform for machine learning competitions (disclaimer: Gonito is
neither affiliated with nor endorsed by [Kaggle](https://www.kaggle.com)).


What's so special about Gonito:

  * free & open-source (AGPL), you can use it your own, in your
    company, at your university, etc.
  * git-based (challenges and solutions are submitted only with git).

See the home page (and an instance of Gonito) at https://gonito.net .

Installation
------------

[Gonito](https://gonito.net) is written in [Haskell](https://www.haskell.org) and uses
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

References
----------

    @inproceedings{gralinski:2016:gonito,
      title="{Gonito.net - Open Platform for Research Competition, Cooperation and Reproducibility}",
      author={Grali{\'n}ski, Filip and Jaworski, Rafa{\l} and Borchmann, {\L}ukasz and Wierzcho{\'n}, Piotr},
      booktitle="{Branco, Ant{\'o}nio and Nicoletta Calzolari and Khalid Choukri (eds.), Proceedings of the 4REAL Workshop: Workshop on Research Results Reproducibility and Resources Citation in Science and Technology of Language}",
      pages={13--20},
      year=2016,
      url="http://4real.di.fc.ul.pt/wp-content/uploads/2016/04/4REALWorkshopProceedings.pdf"
    }
