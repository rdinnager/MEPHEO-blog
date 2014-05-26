One of the most important things

First a quick function to extract the colours from an online or local
image. This code is based on the
[`extract_colours()`](https://github.com/woobe/rPlotter/blob/master/R/extract_colours.R)
function in the [`rPlotter`](https://github.com/woobe/rPlotter) package
written by [Jo-fai Chow](https://github.com/woobe). It uses the
[`EBImage`](http://www.bioconductor.org/packages/release/bioc/html/EBImage.html)
package to load the image. It also requires the `reshape2` package.

    colour_extract <- function(img_file = "http://developer.r-project.org/Logo/Rlogo-1.png", rsize=100) {
      
      require(EBImage)
      require(reshape2)
      
      ## Read Image
      img <- readImage(img_file)
        
      ## Resize Image (make it smaller so the remaining tasks run faster)  
      if (max(dim(img)[1:2]) > rsize) {
        if (dim(img)[1] > dim(img)[2]) {
          img <- resize(img, w = rsize)
        } else {
          img <- resize(img, h = rsize)
        }
      }
      
      ## Melt
      img_melt <- melt(img)
      
      ## Reshape
      img_rgb <- reshape(img_melt, timevar = "Var3", idvar = c("Var1", "Var2"), direction = "wide")
      
      just_cols<-unique(img_rgb[,3:5])
      colnames(just_cols)<-c("Red","Green","Blue")
      
      return(just_cols)
    }

First thing, it would be cool to visualize the distribution of colours
in an image to get an idea of where the major colour gradients are. I
will use an image of one of my favourite we images of one of my
favourite animals:

![A posing Peacock
Spider](http://upload.wikimedia.org/wikipedia/commons/5/53/MalePeacockSpider.jpg)

The plot requires the package `lattice`.

    library(EBImage)
    library(reshape2)
    library(lattice)
    ## extract the colours!
    spider.cols<-colour_extract("http://upload.wikimedia.org/wikipedia/commons/5/53/MalePeacockSpider.jpg",rsize=100)

    ## plot a spinning 3D plot of the RGB values

    for (i in seq(0,360,by=10)) print(cloud(Blue~Red*Green,spider.cols,pch=19,col=rgb(spider.cols), screen=list(z=i,x=-60)))

<video   controls="controls" loop="loop">
<source src="R_Colours_files/figure-markdown_strict/animate_colours.ogg" />video
of chunk animate colours
</video>
