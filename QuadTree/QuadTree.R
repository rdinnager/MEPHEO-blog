library(EBImage)
library(colorspace)
library(tidyr)
library(dplyr)
library(magrittr)

img <- readImage("http://upload.wikimedia.org/wikipedia/commons/5/53/MalePeacockSpider.jpg")

test <- as.raster(img)
width <- ncol(test)
height <- nrow(test)
xycoords <- as.matrix(test) %>% data.frame(stringsAsFactors = FALSE) %>% set_colnames(width %>% seq_len) %>% 
  mutate(Y = height %>% seq_len) %>% gather(X, colour, width %>% seq_len) %>% tbl_df
xycoords$X <- xycoords$X %>% as.character %>% as.integer
## convert to LAB space
img_tree <- xycoords$colour %>% hex2RGB(gamma=TRUE) %>% as("LAB") %>% coords %>% data.frame %>% 
  cbind(xycoords %>% select(X, Y)) %>% gather(element, value, L, A, B) %>% 
  cbind(colour = as.vector(test), stringsAsFactors = FALSE) %>% tbl_df %>% mutate(split = 1)
rownames(img_tree) <- seq_len(nrow(img_tree))

sub_rows <- c(seq_len(nrow(img_tree)))

ifelse(img_tree$X < width, ifelse(img_tree$Y < height, div_facts[1], div_facts[3]), ifelse(img_tree$Y < height, div_facts[2], div_facts[4]))

iters <- 5
last_fact <- 0

i <- 1

for (i in 1:iters) {
  step_name <- paste("Step_", i, sep = "")
  ## divide image
  div_facts <- (last_fact + 1):(last_fact + 4)
  last_facts <- last_fact + 4
  div_x <- floor(width / 2)
  div_y <- floor(height / 2)
  img_tree$split <- ifelse(img_tree$X < div_x, ifelse(img_tree$Y < div_y, div_facts[1], div_facts[3]), ifelse(img_tree$Y < div_y, div_facts[2], div_facts[4]))
  
  #div_k <- kronecker(matrix(div_facts, 2, byrow = TRUE), matrix(1, div_y, div_x))
  #if ((width %% 2) != 0) div_k <- div_k %>% cbind(c(rep(2, div_y), rep(4, div_y + 1)))
  #if ((height %% 2) != 0) div_k <- div_k %>% rbind(c(rep(1, floor(ncol(div_k) / 2)), rep(2, ncol(div_k) / 2)))
  #div_k <- div_k %>% as.vector %>% rep(3)
  ## add dividing factor to dataframe
  #img_tree$split[sub_rows] <- div_k  
  img_tree2 <- img_tree %>% group_by(element, split)
  ## calculate mean distance from centroid for each split
  split_crit <- img_tree2 %>% mutate(std = (value - mean(value))^2) %>% ungroup %>% 
    select(X, Y, split, element, std) %>% spread(element, std) %>% mutate(errs = sqrt(L + A + B)) %>%
    group_by(split) %>% summarise(error = mean(errs))
  best <- split_crit$split[split_crit$error %>% which.max]
  sub_img <- img_tree2 %>% filter(split == best) 
  sub_rows <- rownames(sub_img)
  width <- max(sub_img$X) - min(sub_img$X) + 1
  height <- max(sub_img$Y) - min(sub_img$Y) + 1
}


if ((nrow(test)%%2)!=0) test <- test[-nrow(test),]
if ((ncol(test)%%2)!=0) test <- test[,-ncol(test)]
xsize <- ncol(test) / 2
ysize <- nrow(test) / 2
k <- kronecker(matrix(1:4, 2, byrow = TRUE), matrix(1, xsize, ysize))
tt <- lapply(split(test, k), matrix, nrow=ysize)