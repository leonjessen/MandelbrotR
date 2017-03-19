# ---------------------------------------------------------------------------- #
# This R-script will draw the Mandelbrot set                                   #
#                                                                              #
# Inspired by this Numberphile video featuring Dr. Holly Krieger:              #
# https://www.youtube.com/watch?v=NGMRB4O922I                                  #
# Authored by Leon Eyrich Jessen, December 2016, update March 2017             #
# https://github.com/leonjessen/MandelbrotR                                    #
# ---------------------------------------------------------------------------- #

# Clear workspace
# ------------------------------------------------------------------------------
rm(list=ls())

# Define functions
# ------------------------------------------------------------------------------
Mandelbrot_count = function(z = complex(r=0,i=0), c, lim = 100, n = 0){
  n = n + 1
  z = z * z + c
  if( abs(z) > 2 | n > lim ){
    return(n)
  }
  # Recurse untill return criterea are met
  Mandelbrot_count(z = z, c = c, lim = lim, n = n)
}
Mandelbrot_matrix = function(res=512,lim=512){
  A  = seq(-2,1,len=res)           # Values along the complex a-axis
  B  = seq(-1.5,1.5,len=res)       # Values along the complex b-axis
  mb = matrix(0,nrow=res,ncol=res) # Prebuild mandelbrot count matrix
  
  # Each pixel in the final picture is a count, generate counts
  for( i in 1:res ){
    for( j in 1:res ){
      mb[i,j] = Mandelbrot_count(c = complex(r=A[i],i=B[j]), lim = lim)
    }
  }
  return(mb)
}

# Generate Mandelbrot set count
# ------------------------------------------------------------------------------
#  - res [Pixel resolution res x res]
#  - lim [Bail out limit for max number of iterations]
# Should a high bail out limit, the internal R
# max number of allowed recursion may be reached
# Set to e.g. options( expressions = 10000 )
res = 512
lim = 512
mb  = Mandelbrot_matrix(res=res, lim=lim)


# Plot the Mandelbrot set
# ------------------------------------------------------------------------------
# Set colour palette
palette  = c("darkblue","lightblue","white","yellow","orange","black")
colours  = colorRampPalette(palette)(n = lim)

# Plot to png
png_file = paste0('mandelbrot_png_res_',res,'_lim_',lim,'.png')
png(filename = png_file, width = res, height = res)
par(pty="s")
image(mb, col = colours, xaxt = 'n', yaxt = 'n')
dev.off()

# Save Mandelbrot count matrix to file
# ------------------------------------------------------------------------------
mat_file = paste0('mandelbrot_matrix_res_',res,'_lim_',lim,'.txt')
write.table(mb, file=mat_file, row.names=FALSE, col.names=FALSE, sep=' ')
