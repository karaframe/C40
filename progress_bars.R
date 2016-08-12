
library(tcltk)


total <- 10
for(i in 1:total){
  print(i)
  Sys.sleep(0.1)
}

##############################################

total <- 20
for(i in 1:total){
  Sys.sleep(0.1)
  print(i)
  # update GUI console
  flush.console()                          
}

##############################################

total <- 20
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in 1:total){
  Sys.sleep(0.1)
  # update progress bar
  setTxtProgressBar(pb, i)
}
close(pb)

##############################################

total <- 20
# create progress bar
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = total, width = 300)

for(i in 1:total){
  Sys.sleep(0.1)
  setTkProgressBar(pb, i, label=paste( round(i/total*100, 0),
                                       "% done"))
}
close(pb)


#############################################

# create progress bar
pb <- winProgressBar(title = "progress bar", min = 0,
                     max = total, width = 300)

for(i in 1:total){
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, title=paste( round(i/total*100, 0),
                                        "% done"))
}
close(pb)

