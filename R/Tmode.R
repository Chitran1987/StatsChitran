Tmode<-function(v, bt=T){
  df<-data.frame(matrix(NA, ncol = 2))
  colnames(df)<-c('val','fr')
  for (i in 1:length(v)) {
    
    
    
    if(i==1){
      df$val[i]<-v[i]
      df$fr[i]<-1
    }
    else{
      if(sum(df$val==v[i])!=0){
        vc<-df$val==v[i]
        for (j in 1:length(vc)) {
          if(vc[j]==T){
            df$fr[j]<-df$fr[j]+1
          }
        }
      }
      else{
        #df$val<-c(df$val,v[i])
        #df$fr<-c(df$fr,1)
        df_temp<-data.frame(matrix(c(v[i],1), byrow = T, ncol = 2, nrow = 1))
        colnames(df_temp)<-colnames(df)
        df<-rbind(df,df_temp)
      }
    }
    
    
    
    
    
    
  }
  df_new<-subset(df, df$fr==max(df$fr))
  if(bt==T){
    return(df_new)
  }
  else{
    L<-list(df_new, df)
    return(L)
}
}