# ------------------------------------------------------------------------------
# This R-script will draw the Mandelbrot set
# Inspired by this Numberphile video featuring
# Dr Holly Krieger: 
# https://www.youtube.com/watch?v=NGMRB4O922I
# Authored by Leon Eyrich Jessen, December 2016
# https://github.com/leonjessen/Draw_Mandelbrot
# ------------------------------------------------------------------------------

# Clear workspace
# ------------------------------------------------------------------------------
rm(list=ls())

# Define functions
# ------------------------------------------------------------------------------
countMandelbrot = function(z=complex(r=0,i=0),c,lim=100,n=0){
  n = n + 1
  if( n > lim ){
    return(n-1)
  }
  z = z * z + c
  if( abs(z) > 2 ){
    return(n)
  }
  countMandelbrot(z=z,c=c,lim=lim,n=n)
}
drawMandelbrot = function(res=500,lim=100,alim=c(-2,1),blim=c(-1.5,1.5),plot=TRUE){
  A = seq(alim[1],alim[2],length.out=res)
  B = seq(blim[1],blim[2],length.out=res)
  m = sapply(seq(1,length(A)),function(i){
        sapply(seq(1,length(B)),function(j){
          a = A[i]
          b = B[j]
          n = countMandelbrot(c=complex(r=a,i=b),lim=lim)
          return(c(a,b,n))
        })
      })
  X = as.vector(m[seq(1,nrow(m),3),])
  Y = as.vector(m[seq(2,nrow(m),3),])
  n = as.vector(m[seq(3,nrow(m),3),])
  if( plot ){
    p = c("darkblue","lightblue","white","yellow","orange","black")
    c = colorRampPalette(p)(n=lim)
    par(pty="s")
    plot(X,Y,xlim=alim,ylim=blim,pch=20,cex=0.2,col=c[n],
         xaxt='n',yaxt='n',bty='n',ann=FALSE)
  }
  return(cbind(X,Y,n))
}

# Draw the Mandelbrot set
# ------------------------------------------------------------------------------
m = drawMandelbrot()
