Just for fun, we can even use these little planets as a cool backdrop
for a circular phylogeny.

    library(jpeg)
    library(treebase)
    library(ape)
    library(wesanderson)
    ## load little planet image
    backimg <- readJPEG("D:/Users/Dinnage/Projects/MEPHEO-blog/Little-Planets/HerdsmanLake_edit_small_centred4.jpg")
    ## grab a tree for waterfowl
    tree <- search_treebase("3269","id.tree","tree",TRUE)

    ## http://purl.org/phylo/treebase/phylows/tree/find?query=tb.identifier.tree==3269&format=rss1&recordSchema=tree
    ## Query resolved, looking at each matching resource...
    ## 1 resources found matching query
    ## Attempting try 1
    ## Looking for nexus files...
    ## Tree read in successfully
    ## dropped 0 objects

    par(bg="gray")
    ## setup the tree plot
    tt<-plot(tree[[1]], type="fan", plot=FALSE, no.margin=TRUE)
    ## plot the background image on the blank plot
    rasterImage(backimg, tt$x.lim[1], tt$y.lim[1], tt$x.lim[2], tt$y.lim[2])
    ## draw tree with thick green branches
    plot.phylo.add(tree[[1]], type="fan",cex=0.75, edge.color=wes.palette(4, "Rushmore")[3],edge.width=6, label.offset=0.1,
                   node.depth=2)
    ## redraw tree with thinner white branches, to give an outline effect
    plot.phylo.add(tree[[1]], type="fan",cex=0.75, edge.color="white",edge.width=4, label.offset=0.1,
                   node.depth=2)

![plot of chunk
littleplanet](Little_planets_files/figure-markdown_strict/littleplanet.png)

This could probably use some tweaking in a graphics program. As it is,
it looks a bit busy. But it was fun to try this in R. By the way, in
case you are wondering, the function `plot.phylo.add()` is just the
`plot.phylo()` function from the package `ape`, slightly modified so it
will plot over something that has already been plotted. I simple
commented out this line:

    plot.default(0, type = "n", xlim = x.lim, ylim = y.lim, xlab = "", 
                 ylab = "", axes = FALSE, asp = asp, ...)

That is the line where the function sets up the plot before it draws the
tree. Since the plot had already been setup, this line isn't needed, and
if it is still there, it results in the plot being redrawn and losing
the background image.
