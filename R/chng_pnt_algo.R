chng_pnt_algo<-function(v,dat){
  names(dat)<-c('V1', 'V2')
  L<-length(v)
  Bp<-v[seq(1,(2*(L-1)/3))]####number of breakpoints
  H<-v[seq(((2*L+1)/3),L)] ####number of plateau heights

  ###########################################################
  #######creating the breakpoint height dataframe############
  Ht<-vector(mode = 'numeric', length=length(Bp))
  #######creating the Htvalues for the breakpoints##########
  k<-2
  for (i in 1:length(Bp)) {
    if(i==1){
      Ht[i]<-H[i]
    }
    else if(i==length(Bp)){
      Ht[i]<-H[length(H)]
    }
    else{
      if(i%%2==0){
        Ht[i] <- H[k]
        k <- k+1
      }
      else{
        Ht[i]<-Ht[i-1]
      }
    }
  }
  ###########################################################

  #######creating the dataframe##############################
  Bp_H_df<-data.frame(matrix(c(Bp,Ht), byrow = F, ncol = 2))
  names(Bp_H_df)<-c('V1','V2')
  ###########################################################

  ###########################################################
  ###########################################################

  #######
  #######create a function for accepting the profile that you want to create
  prof_struct<-function(v,dat){
    tgt<-dat
    L<-length(v)
    Bp<-v[seq(1,(2*(L-1)/3))]####number of breakpoints
    H<-v[seq(((2*L+1)/3),L)] ####number of plateau heights
    for (i in 1:length(Bp)) {
      if(i==1){
        tgt$V2[tgt$V1<=Bp[i]]<-H[i]
      }#if close
      else if(i==length(Bp)){
        tgt$V2[tgt$V1>=Bp[i]]<-H[length(H)]
        tgt$V2[tgt$V1>=Bp[i-1] & tgt$V1<=Bp[i]]<-((H[(i/2)+1]-H[i/2])/(Bp[i]-Bp[i-1]))*(tgt$V1[tgt$V1>=Bp[i-1] & tgt$V1<=Bp[i]]-Bp[i-1])+H[i/2]
      }#else if close
      else{
        if(i%%2==0 ){
          tgt$V2[tgt$V1>=Bp[i-1] & tgt$V1<=Bp[i]]<-((H[(i/2)+1]-H[i/2])/(Bp[i]-Bp[i-1]))*(tgt$V1[tgt$V1>=Bp[i-1] & tgt$V1<=Bp[i]]-Bp[i-1])+H[i/2]
        }#if close
        else{
          tgt$V2[tgt$V1>=Bp[i-1] & tgt$V1<=Bp[i]]<-H[(i+1)/2]
        }#small else close
      }#bigelse close
    }#forloopclose
    return(tgt)
  }   #function close
  ####################################################

  ########define the plot limits######################
  xmin <- min(dat$V1)-(1/4)*(max(dat$V1)-min(dat$V1))
  xmax <- max(dat$V1)+(1/4)*(max(dat$V1)-min(dat$V1))
  xl <- c(xmin,xmax)

  ymin <- min(dat$V2)
  ymax <- max(dat$V2)+(1/4)*(max(dat$V2)-min(dat$V2))
  yl <- c(ymin, ymax)
  ####################################################

  #######first plot###################################
  plot(dat$V1, dat$V2,col=rgb(0,0.5,0, 0.5), pch=19)
  dat1<-prof_struct(v,dat)
  dat1<-rbind(dat1, Bp_H_df)
  dat1<-dat1[order(dat1$V1),]
  lines(dat1$V1,dat1$V2, col='red')


  ######setup the constraint matrix and the vector
  ###number of points#####number of constraints=n-1(for breakpoints)+2*((length(v)+2)/3)(for the plateau heights)
  n<-2*(length(v)-1)/3  ####number of breakpoints
  #######constraint matrix
  m<-matrix(0,nrow = 2*n+1, ncol = length(v))
  #####loop for the n-1 breakpoint constraints
  for (i in 1:n-1) {
    m[i,i]<--1
    m[i,i+1]<-1
  }
  #####loop for the 2*((length(v)+2)/3) plateau height constraints
  for(i in n:(n+((n/2)+1)-1)){
    m[i,i+1]<-1
  }
  for(i in ((n+((n/2)+1)-1)+1):(2*n+1) ){
    m[i,i-((n/2)+1)+1]<--1
  }
  ###constraint vector
  #####length of constraint vector is equal to no. of constraints
  b<-rep(0L, 2*n+1)
  #####use zeros for the breakpoint constraints
  for(i in 1:n-1){
    b[i]<-0
  }
  for (i in n:(n+((n/2)+1)-1)) {
    b[i]<-min(dat$V2)
  }
  for (i in ((n+((n/2)+1)-1)+1):(2*n+1)) {
    b[i]<--max(dat$V2)
  }
  #######write down the constraint function#####
  f<-function(v){
    dat2<-dat
    tgt2<-prof_struct(v,dat2)
    e<-sum((tgt2$V2-dat$V2)^2)
    return(e)
  }
  r<-constrOptim(v,f,NULL,m,b)

  ######calculating the return values#####
  v<-r$par
  L<-length(v)
  Bp<-v[seq(1,2*(L-1)/3)]
  H<-v[seq((2*L+1)/3, L)]
  ########################################
  ###########################################################
  #######creating the breakpoint height dataframe############
  Ht<-vector(mode = 'numeric', length=length(Bp))
  #######creating the Htvalues for the breakpoints##########
  k<-2
  for (i in 1:length(Bp)) {
    if(i==1){
      Ht[i]<-H[i]
    }
    else if(i==length(Bp)){
      Ht[i]<-H[length(H)]
    }
    else{
      if(i%%2==0){
        Ht[i] <- H[k]
        k <- k+1
      }
      else{
        Ht[i]<-Ht[i-1]
      }
    }
  }
  ###########################################################

  #######creating the dataframe##############################
  Bp_H_df<-data.frame(matrix(c(Bp,Ht), byrow = F, ncol = 2))
  names(Bp_H_df)<-c('V1','V2')
  ###########################################################
  ######plotting the optimized function#####
  tgt<-prof_struct(v,dat)
  tgt<-rbind(tgt, Bp_H_df)
  tgt<-tgt[order(tgt$V1),]
  lines(tgt$V1,tgt$V2, col='blue')
  legend('topleft', legend = c('approx', 'optim'), col = c('red', 'blue'), lty=1:1)

  #######convergence factor##########
  con<-vector(mode = 'character', length = 1)
  if(r$convergence==0){
    con<-'solution has converged'
  }
  else{
    con<-'solution has not converged'
  }
  ##################################

  ######result of final convergence#####
  final<-list(return=r,Optimized_Vector=v, BreakPoints=Bp, Heights=H, Convergence=con)
  return(final)
}

