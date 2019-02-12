Code
====

_The following is all a bit outdated but if you are interested in learning
how I used to do research 3-4 years ago, read on!_

Other than about mathematics and physics, I am very passionate about
the tools I use to carry on my research.

My workflow roughly goes as follows: I write my Monte Carlo simulation code in
C++. C++ is an horrible language for academic research because it's _very_
complex and there are too many ways to shoot yourself in the foot, e.g. by
stepping on [undefined behavior](http://blog.regehr.org/archives/213). Despite
this, I spent quite a long time learning C++ and, along with Python, is one of
the languages I am most productive in. I have recently fall in love with
[Rust](http://www.rust-lang.org) and but I find [Scala](http://www.scala-lang.org)
quite attractive.

My simulations usualy end up writing down a big
[HDF5](http://www.hdfgroup.org/HDF5/) ready for data analysis.

I do all the analysis on [Jupyter
notebooks](http://ipython.org/notebook.html) using a combination of
Python libraries ([numpy](http://numpy.org),
[matplotlib](http://matplotlib.org), [PyTables](http://pytables.github.io),
[Pandas](http://pandas.pydata.org), etc) and code I have written of my
own that automatise the most repetitive tasks.

If you never used Jupyter notebooks for your research you should, your
notes will looks like [this](http://nbviewer.jupyter.org/). You can
play with Jupyter notebooks simply by visiting [http://tmpnb.org](http://tmpnb.org).

When an open-source tool that I use is missing an important feature, I do my
best to contribute to its development. This was the case with PyTables, to
which I have contributed the
[code](https://github.com/PyTables/PyTables/pulls?q=is%3Apr+author%3Aandreabedini)
to support extended precision datasets on both 32 and 64 bits arches (where
extended precision is 12 and 16 bytes respectively). My contribution to the
project is now lagging behind and I do feel pretty bad about it.

