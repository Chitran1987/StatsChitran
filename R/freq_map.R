freq_map <- function(X,Y, w_int, nbin, xbox, color, plt=F, plt.leg=F){
  #w_int has to be a 2 row data frame. Defines the min and max values of the k-space within whose bounds you integrate. name your columns for the plot legends
  #nbin has to be a vector such that "length(nbin) == dim(w_int)[2]". No. of single units of periodicity to be mapped for each freq component.
  #xbox has to be a single number: denotes the length of the bin vector in real space(Use only if you want same bin size for all vectors, else use nbin)
  #xbox and nbin can't be simultaneously defined
  ###error handling for simultaneous xbox and nbin usage#######################################
  if(xor(missing(nbin), missing(xbox)) == F){
    stop('Either both xbox and nbin are both assigned, or are both missing. Check') ##check if both nbin and xbox are assigned
  }
  #############################################################################################
  ###error handling for w_int here#############################################################
  if(is.data.frame(w_int) == F){
    stop('w_int has to be a dataframe (of type list)') ##check for data frame
  }
  if(dim(w_int)[1] != 2){
    stop('w_int has to be a two row data frame') ##check for two rows(dimensions)
  }
  if(all(sapply(w_int, is.numeric)) == F){
    stop('w_int needs every element to be numeric') ##check if the dataframe is numeric
  }
  if(sum(is.na(w_int)) != 0){
    stop("The w_int dataframe can't contain NAs or NANs") ##check to see if there are no NAs
  }

  ####################################################################################
  ###error handling for nbin here#####################################################
  if(missing(nbin) == F){
    if(is.vector(nbin) == F){
      stop('nbin has to be a vector') ##check for vector
    }
    if(length(nbin) != dim(w_int)[2]){
      stop('length of nbin and no. of columns in w_int have to be equal') ##check for dimensions
    }
    if(is.numeric(nbin) == F){
      stop('nbin has to be numeric')  ##check if nbin is numeric
    }
    if(sum(is.na(nbin)) != 0){
      stop('nbin cannot contain NAs') ##check for NAs
    }
  }

  ###################################################################################
  ##error handling for color vector here#############################################
  if(plt == T){
    if(length(color) != dim(w_int)[2]){
      stop('length of color vector and no. of columns in w_int have to be equal')
    }
  }
  ###################################################################################
  ##error handling for xbox here#####################################################
  if(missing(xbox) == F){
    if(is.numeric(xbox) == F){
      stop('xbox has to be a numeric') ##check if xbox is a numeric
    }
    if(is.null(dim(xbox)) == F){
      stop('xbox should have a dimension of NULL') ##check if xbox is a simple number
    }
    w_trial <- as.numeric(unlist(w_int)) ##force all integration limits into a vector, w_trial
    w_min_abs <- min(w_trial)  ##identify the smallest element in the vector, w_trial, as this corresponds to the largest possible wavelength
    if(xbox <= 2*pi/w_min_abs){
      stop('xbox size too small for all wavelengths applied. Please suggest a larger size of xbox') #check if xbox can accommodate the largest wave limit suggested by w_int
    }
  }
  ###################################################################################
  ##define the sampling##############################################################
  xsamp <- mean(diff(X))
  ##define the integration bounds####################################################
  win_min <- vector(mode = 'numeric', length = dim(w_int)[2])
  win_max <- vector(mode = 'numeric', length = dim(w_int)[2])
  for (i in 1:dim(w_int)[2]) {
    win_min[i] <- min(w_int[,i])
    win_max[i] <- max(w_int[,i])
  }
  ##define nbin from xbox if nbin is not separately assigned#########################
  if(missing(xbox) == F){
    xp <- 2*pi/win_min  #max possible periodicity of the waves suggested by each window of the w_int data frame
    nbin <- xbox/xp     #once nbin is defined from xbox, the rest of the algorithm could function independently
  }
  ##no. of points available in a single unit of periodicity##########################
  p <- 2*pi/(win_min*xsamp) #no. of data points needed for a single unit of periodicity in each wave
  N <- ceiling(nbin*p) #no. of data points needed for nbin units of periodicity/for a single bin
  n_dark_field_bins <- floor(length(X)/N) #no. of bins created by this dark field binning over entire data set

  ###error handling here################################################################






  ######################################################################################

  ##define a dark field list############################################################
  dark_list <- vector(mode = 'list', length = dim(w_int)[2])
  ##define a bin_vec list###############################################################
  bin_vec <- vector(mode = 'list', length = dim(w_int)[2])
  for (i in 1:dim(w_int)[2]) {
    bin_vec[[i]] <- seq(1, n_dark_field_bins[i]) #define a bin vector and store it in a list
  }
  ##start the dark field binning
  for (i in 1:dim(w_int)[2]) {
    dfld <- NULL #define a dark field vector
    d_orig <- data.frame(X,Y)
    for (r in bin_vec[[i]]) {
      d_tmp <- d_orig[((r-1)*N[i]+1):(r*N[i]),]  #subset your datatframe according to the bin
      dft_tmp_abs <- ft(d_tmp$X, d_tmp$Y, w=T)  #fourier transform it
      dft_tmp_abs$fy <- abs(dft_tmp_abs$fy) #convert the fourier coefficients to their magnitude
      int_val <- num_integrate(dft_tmp_abs$w, dft_tmp_abs$fy, xmin = win_min[i], xmax = win_max[i]) #integrate between said (w values) k-vectors in k-space
      dfld <- c(dfld, int_val)
    }
    dark_list[[i]] <- data.frame(bin_vec[[i]], dfld) #convert it into a data frame and store it in a list
  }



  ###recreating an envelope on the original dataset
  ##defining the envelope function(f_data) list
  f_data <- vector(mode = 'list', length = dim(w_int)[2])
  for (i in 1:dim(w_int)[2]) {
    f_data_X <- NULL
    f_data_Y <- NULL
    for (r in bin_vec[[i]]) {
      f_data_X_tmp <- X[((r-1)*N[i]+1):(r*N[i])]
      f_data_X <- c(f_data_X, f_data_X_tmp)
      f_data_Y_tmp <- rep(dark_list[[i]]$dfld[r], N[i])
      f_data_Y <- c(f_data_Y, f_data_Y_tmp)
    }
    #f_data_Y <- nrm(f_data_Y)
    f_data[[i]] <- data.frame(f_data_X, f_data_Y)
  }



  if(plt == T){
    ##normalizing the dataframes in f_data#########################################
    f_data_norm <- f_data
    f_data_ratios <- vector(mode = 'list', length = dim(w_int)[2])
    for (i in 1:dim(w_int)[2]) {
      f_data_norm[[i]]$f_data_Y <- nrm(f_data_norm[[i]]$f_data_Y)
      f_data_ratios[[i]] <- c(max(f_data[[i]]$f_data_Y), min(f_data[[i]]$f_data_Y))
    }
    ##scaling f_data to look good for plotting
    ##place f_data_ratios in a vector(color_ratio_vec), normalize with min=mean(signal), max=(2/3)(max peak of sinal from mean)
    ##then place back in f_data_ratios list
    color_ratio_vec <- NULL
    for (i in 1:dim(w_int)[2]) {
      color_ratio_vec <- c(color_ratio_vec, f_data_ratios[[i]]) #place in color_ratio_vec
    }
    color_ratio_vec <- nrm(color_ratio_vec, min = mean(Y), max = 1*(max(Y) - mean(Y))/2 + mean(Y)) #normalize
    for (i in 1:dim(w_int)[2]) {
      f_data_ratios[[i]] <- c(color_ratio_vec[2*i-1], color_ratio_vec[2*i]) #place back in f_data_ratios
    }
    #scale f_data_norm in accordance to f_data_ratios
    for (i in 1:dim(w_int)[2]) {
      f_data_norm[[i]]$f_data_Y <- nrm(f_data_norm[[i]]$f_data_Y, min = f_data_ratios[[i]][2], max = f_data_ratios[[i]][1])
    }

    ##plotting the data
    #ClearPlot()
    plot(X,Y, col=rgb(0,0,1,0.25), type = 'l')
    for (i in 1:dim(w_int)[2]) {
      lines(f_data_norm[[i]]$f_data_X, f_data_norm[[i]]$f_data_Y, col=color[i])
    }
    if(plt.leg ==T){
      legend('topright', legend = names(w_int), lty = 1, col = color, horiz = F, bg = 'transparent', bty = 'n', lwd = 3)
    }
  }
  return(f_data)
}
