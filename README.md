Manuscript, code, and data for "Compression and Conditional Effects." [[Paper](http://www.carlislerainey.com/files/compress.pdf)]

> Previous research in political methodology argues that researchers do not need to include a product term in a logistic regression model to test for interaction if they suspect interaction due to compression alone. I disagree with this claim and offer analytical arguments and simulation evidence that when researchers incorrectly theorize interaction due to compression, models without a product term bias the researcher, sometimes heavily, toward finding interaction. However, simulation studies also show that models with a product term fit a broad range of non-interactive relationships surprisingly well, enabling analysts to remove most of the bias toward finding interaction by simply including a product term.

If you have any comments or suggestion, please [open an issue](https://github.com/carlislerainey/compress/issues) or just [e-mail](mailto:carlislerainey@gmail.com) me.

To replicate all the results, simply [download](https://github.com/carlislerainey/compress/archive/master.zip) the directory, and run the files

* `do-all.R`, which performs all the Monte Carlo simulations, creates the plots, and does the Oneal and Russet replication.

Note that the simulation take a very long time to run. If you like, you can tradeoff the speed and precision of the simulations by changing the lines

    # Set simulation parameters
    n.sims1 <- 2000
    n.sims2 <- 2000

in the file `do-all.R`. `n.sims1` controls the number of times to compute each confidence interval to compute the probability of covering the true value. `n.sims2` controls the number of simulations used to construct each confidence interval. Also, in the file `sims-generic.R`, the parameter `niter` controls the number of random DGPs to compute.

Note that I did some post-editing on the figures in order to clearly annotate relevant information. I did this in [Inkscape](http://www.inkscape.org/en/).
