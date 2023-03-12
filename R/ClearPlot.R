#write a program to clear the graphics output if a plot is displayed
ClearPlot<-function(){
  if (names(dev.cur()) !='null device'){
    dev.off()
  }
  #return NULL
}

