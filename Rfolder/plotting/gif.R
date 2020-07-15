# This function still needs a bunch of work

gif.plot <- function(sldf, detects, dir = "D:/Jordy/myplots/plot", darken=2.5, col_by_fish=F){

  n_flights <- detects$

  files <- rep(NA,n_flights)
  for (i in 1:n_flights){
    make.plot(sldf, viterbi, flight=i,open_maps=T, type="bing", darken=2.5, col_by_fish=F)
    file <- paste(dir, i, ".png", sep="")
    files[i] <- file
    dev.print(png, file = file, width = 1024, height = 768)
    dev.off()
  }
  images <- map(files, image_read)
  images <- image_join(images)
  animation <- image_animate(images, fps = 0.5)
  image_write(animation, paste(dir, "gif.gif", sep=""))
}

library(magick)
n_flights <- 8

