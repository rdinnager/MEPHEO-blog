library(EBImage)
library(colorspace)
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)

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
  cbind(colour = as.vector(test), stringsAsFactors = FALSE) %>% tbl_df %>% mutate(split = 1, row_n = seq_len(nrow(img_tree))) %>%
  group_by(element) %>% mutate(mean.col = mean(value))  %>% ungroup
#rownames(img_tree) <- seq_len(nrow(img_tree))

sub_img <- img_tree[c(seq_len(nrow(img_tree))),]
sub_rows <- sub_img$row_n

#ifelse(img_tree$X < width, ifelse(img_tree$Y < height, div_facts[1], div_facts[3]), ifelse(img_tree$Y < height, div_facts[2], div_facts[4]))

iters <- 1000
last_fact <- 0

i <- 1

for (i in 1:iters) {
  ## convert img_tree dataframe into a series of plottable rectangles
  img_rect <- img_tree %>% group_by(split) %>% summarise(xmin = min(X), xmax = max(X), ymin = min(Y), ymax = max(Y))
  img_cols <- img_tree %>% select(X, Y, split, element, mean.col) %>% group_by(element, split) %>% 
    summarise(mean.col = mean(mean.col)) %>% spread(element, mean.col) %>% mutate(hex.col = LAB(L, A, B) %>% hex(fixup = TRUE)) %>%
    select(split, hex.col)  
  img_rect <- left_join(img_rect, img_cols)
  p <- ggplot(img_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = factor(split))) + geom_rect() + 
    scale_fill_manual(values=img_rect$hex.col) + theme(legend.position="none")
  print(p)
  Sys.sleep(0)
  step_name <- paste("Step_", i, sep = "")
  width <- max(sub_img$X) - min(sub_img$X) + 1
  height <- max(sub_img$Y) - min(sub_img$Y) + 1
  ## divide image
  div_facts <- (last_fact + 1):(last_fact + 4)
  last_fact <- last_fact + 4
  div_x <- width / 2
  div_y <- height / 2
  sub_img <- sub_img %>% mutate(X2 = X - min(X) + 1, Y2 = Y - min(Y) + 1, split = ifelse(X2 < div_x, ifelse(Y2 < div_y, div_facts[1], div_facts[3]), 
                                                     ifelse(Y2 < div_y, div_facts[2], div_facts[4]))) %>% select(-X2, -Y2)    
  img_tree[sub_rows,] <- sub_img 
  img_tree <- img_tree %>% group_by(element, split) %>% mutate(mean.col = mean(value))
  ## calculate mean distance from centroid for each split
  split_crit <- img_tree %>% mutate(std = (value - median(value))^2) %>% ungroup %>% 
    select(X, Y, split, element, std) %>% spread(element, std) %>% mutate(errs = sqrt(L + A + B)) %>%
    group_by(split) %>% summarise(error = sum(errs))
  best <- split_crit$split[split_crit$error %>% which.max]
  sub_img <- img_tree %>% filter(split == best) %>% ungroup
  sub_rows <- sub_img$row_n
  print(i)
}

img_rect <- img_tree %>% group_by(split) %>% summarise(xmin = min(X), xmax = max(X), ymin = min(Y), ymax = max(Y))
img_cols <- img_tree %>% select(X, Y, split, element, mean.col) %>% group_by(element, split) %>% 
  summarise(mean.col = mean(mean.col)) %>% spread(element, mean.col) %>% mutate(hex.col = LAB(L, A, B) %>% hex(fixup = TRUE)) %>%
  select(split, hex.col)  
img_rect <- left_join(img_rect, img_cols)
p <- ggplot(img_rect, aes(xmin = xmin - 1, xmax = xmax, ymin = ymin - 1 , ymax = ymax, fill = factor(split))) + geom_rect() + 
  scale_fill_manual(values=img_rect$hex.col) + theme(legend.position="none")
p
