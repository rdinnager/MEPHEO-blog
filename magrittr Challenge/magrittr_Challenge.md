magrittr Challenge!
-------------------

![A gif a giant nerd made](http://i.imgur.com/VjOYVpH.gif)

My good friend
[Andrew](https://www.zoology.ubc.ca/~macdonald/curious_interactions/)
recently posted this
[gist](https://gist.github.com/aammd/ba73669e90ea27b8af2f), purporting
to show how to solve a simple programming exercise know as **FizzBuzz**,
but clearly just showing off his already amazing proficiency in the very
new R package [`magrittr`](https://github.com/smbache/magrittr). Those
were some *piping* hot coding skillz (or should I say (*not-a-*)piping
hot?

![A picture suitable for a fancy pipe
enthusiast](http://media.giphy.com/media/7fLGzTYkX3hO8/giphy.gif)

Clearly this was a challenge to see who can come up with the most
ridiculously long and complicated expression that uses only one line of
code (which is made possible by the magic of `%>%`). In that spirit, I
have decided to try and replicate one of the more complicated analyses
and figures in my [first
paper](http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0007071#pone-0007071-g005)—a
phylogenetic ordination—using only one line of code and lots of
`%>%`-ing. Actually, I am going to use a different method for the
phylogenetic ordination from the paper, and I think this one is better
(I will post more on this in my [blog](http://mepheoscience.com) later).

I download the data directly from the supplemental material on my paper,
so you should be able to run this code on your computer if you want (as
long as you have an internet connection). This requires the following
packages:

-   `magrittr`
-   `ape`
-   `plyr`
-   `dplyr`
-   `vegan`
-   `ggplot2`

<!-- -->

    library(ape)
    library(plyr)
    library(dplyr)
    library(magrittr)
    library(vegan)
    library(ggplot2)
    dat<-scan("http://www.plosone.org/article/fetchSingleRepresentation.action?uri=info:doi/10.1371/journal.pone.0007071.s002",what=character(0)) %>% #download data
      read.tree(text=.) %>% #make it a phylo object
      cophenetic %>% # turn it into a phylogenetic distance matrix
      l(.,x -> cmdscale(x,nrow(x)-1)) %>% # get phylogenetic principle components
      l(.,x -> x[order(rownames(x)), ]) %>% # order by rownames
      aaply("http://www.plosone.org/article/fetchSingleRepresentation.action?uri=info:doi/10.1371/journal.pone.0007071.s001" %>% 
                              read.csv %>% # read-in community data
                              l(x -> x %>% set_rownames(paste(rownames(x),x[,2],sep="_"))) %>% #save treatment names for later in the rownames
                              extract(c(-1,-2)) %>% # toss non-species data
                              l(x -> x[,order(colnames(x))]) %>% # order by column names
                              as.matrix, # make it a matrix
            1,function(x, y) (y*x) %>% colSums, y=.) %>% #make a function to pipe in 
      # community data as x, and phylo data (.) as y, then multiple each community abundance
      # by phylo principle components. now we have a phylogenetic feature vector for each
      # site!
      metaMDS("euclidean") %>% # use non-metric multidimensional scaling on phylogenetic features
      extract("points") %>% # pull out the ordinated points
      data.frame %>% # make it  data.frame
      l(., x -> mutate(x,treatment=rownames(x) %>%
                      strsplit("_") %>% 
                        laply(function(y) y[2]))) %>% #extract treatment names from rownames
      ggplot(.,aes(x=points.MDS1,y=points.MDS2)) + geom_point(aes(color=factor(treatment)),size=5) #make a ggplot

    ## Run 0 stress 0.1055 
    ## Run 1 stress 0.1055 
    ## ... procrustes: rmse 0.01079  max resid 0.05315 
    ## Run 2 stress 0.1199 
    ## Run 3 stress 0.1198 
    ## Run 4 stress 0.1057 
    ## ... procrustes: rmse 0.006186  max resid 0.03488 
    ## Run 5 stress 0.1055 
    ## ... procrustes: rmse 0.01088  max resid 0.05316 
    ## Run 6 stress 0.121 
    ## Run 7 stress 0.1408 
    ## Run 8 stress 0.1055 
    ## ... procrustes: rmse 0.01081  max resid 0.05317 
    ## Run 9 stress 0.1055 
    ## ... New best solution
    ## ... procrustes: rmse 0.000166  max resid 0.0007816 
    ## *** Solution reached

    dat #plot ordination!!

![plot of chunk phylordinate](http://i.imgur.com/shwE6ON.png)

We can see that the main difference between the disturbed and
undisturbed sites is that there is a much larger variance in
phylogenetic composition between disturbed sites. This has interesting
implications which has inspired me to do a follow-up to my old paper, on
my blog. I will post this there as well, once I have switched to my new
platform, which supports markdown.

So that's a phylogenetic ordination, done in a single line of code
(well, it would be a single line if I removed all the linebreaks). Not a
single intermediate variable was used. Note that if you do try this
code, you will get some warnings, they don't affect the outcome.

Did I mention that I love
[`magrittr`](https://github.com/smbache/magrittr) (you could probably
tell since I made that gif at the beginning of this post).

Happy piping!

Russell
