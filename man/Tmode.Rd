\name{Tmode}
\alias{Tmode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the statistical mode of a distribution of numbers listed in a vector
}
\description{
The function calculates the mode of na vector and returns (only the mode when bt=T) or the mode and the frequency table (when bt=F)
}
\usage{
Tmode(v,bt=T)
}
\arguments{
  \item{v}{
The vector whose mode/frequency table you want to find
}
  \item{bt}{
The value of bt decides the return type. If bt is set to TRUE(default), it returns only the mode and its(the mode's) frequency as a dataframe, but if bt is set to FALSE, it returns a list whose first item is the mode(modes) of the distribution (in case of multimodal distributions) and the second item of the list is a two column dataframe, such that the variables in question are the first column of the dataframe and the frequency of occurrence of the variables are the second column of the data frame
}
}
\details{
The function returns either a dataframe with the max value(mode value) and the frequency or a list with two objects
1.The data frame of the mode and its frequency
2.The data frame comprising every element with its occurring frequency
}
\value{
The function returns either a dataframe with the max value(mode value) and the frequency or a list with two objects
1.The data frame of the mode and its frequency
2.The data frame comprising every element with its occurring frequency
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Chitran Ghosal
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
rm(list=ls())
v<-c(89,84,87,81,89,87,86,91,90,78,89,87,99,83,89)
Tmode(v, bt=F)

[[1]]
  val fr
1  89  4

[[2]]
   val fr
1   89  4
2   84  1
3   87  3
4   81  1
5   86  1
6   91  1
7   90  1
8   78  1
9   99  1
10  83  1



## The function is currently defined as
function (v, bt=T)
{
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Mode }% use one of  RShowDoc("KEYWORDS")
\keyword{ Frequency Table }% __ONLY ONE__ keyword per line
