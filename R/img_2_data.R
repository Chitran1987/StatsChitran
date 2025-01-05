#write a function to input a png image and then convert it into a matrix, also plot it in grayscale if the pl is set to true(default)
img_2_data <- function(source.png, x.lim, y.lim, pl = T, bgrnd = 1){
  ##call libraries
  library(png)

  ##error checking
  #check whether source.png is a single character
  if(  (!(is.character(source.png)) | !(length(source.png) == 1)) ){
    stop('source.png has to be a single character value')
  }
  #check whether x.lim and y.lim are two element numeric vectors
  if( (!(is.numeric(x.lim)) | !(length(x.lim) == 2)) ){
    stop('x.lim has to be a numeric vector of length = 2')
  }
  if( (!(is.numeric(y.lim)) | !(length(y.lim) == 2)) ){
    stop('y.lim has to be a numeric vector of length = 2')
  }
  #check whether pl is a one element boolean
  if( !(is.logical(pl)) | !(length(pl) == 1)){
    stop('pl has to be a boolean scalar')
  }
  #check bgrnd constraints
  if( !(is.numeric(bgrnd)) | !(length(bgrnd) == 1) | !(bgrnd >= 0) | !(bgrnd <= 1) ){
    stop('bgrnd has to be a numeric scalar between 0 and 1')
  }
  ##core
  #build an array from the png file
  img_data <- readPNG(source.png)
  if(get.strct(img_data) == 'Matrix' ){
    #This is grayscale data
    M <- img_data
    L <- list(gray.val = M)
    L$data <- M
  }else if(get.strct(img_data) == 'Array'){
    if(dim(img_data)[3] == 2){
      #This is a GA image
      G <- img_data[,,1]
      A <- img_data[,,2]
      M <- GA_2_gray(gray.ch = G, alp.ch = A, bg = bgrnd)
      L <- list(gray.val = M)
      L$data <- list(gray = G, alp = A)
    }
    if(dim(img_data)[3] == 3){
      #This is RGB data
      red.ch <- img_data[,,1]
      green.ch <- img_data[,,2]
      blue.ch <- img_data[,,3]
      M <- RGBA_2_gray(red.ch, green.ch, blue.ch, alp = F)
      L <- list(gray.val = M)
      L$data <- list(red = red.ch, green = green.ch, blue = blue.ch)
    }
    if(dim(img_data)[3] == 4){
      #This is RGBA data
      red.ch <- img_data[,,1]
      green.ch <- img_data[,,2]
      blue.ch <- img_data[,,3]
      alp.ch <- img_data[,,4]
      M <- RGBA_2_gray(red.ch, green.ch, blue.ch, alp.ch, alp = T, bg = bgrnd)
      L <- list(gray.val = M)
      L$data <- list(red = red.ch, green = green.ch, blue = blue.ch, alp = alp.ch)
    }
  }else{
    stop("Image can't be converted, because conversion leads to it being neither a matrix, nor an array")
  }
  #calculate the co-ordinate values and return it with the list as well
  xmin <- x.lim[1]
  xmax <- x.lim[2]
  ymin <- y.lim[1]
  ymax <- y.lim[2]
  x.seq <- seq(from = xmin, to = xmax, length.out = ncol(M))
  y.seq <- seq(from = ymin, to = ymax, length.out = nrow(M))
  L$data$X <- x.seq
  L$data$Y <- y.seq
  if(pl == T){
    plot2D.mat(X = L$data$X, Y = L$data$Y, Z = M, x.lab = 'X values', y.lab = 'Y Values')
  }
  #if pl == T, output the matrix using plot2D.mat by constructing the X and Y co-ordinates
  #return a list with list$dat, list$X and list$Y
  return(L)
}
