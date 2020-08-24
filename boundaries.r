###############################################################################
## Applies boundary conditions to a 1d or 2d object. Implemented in R        ##
## version 3.4.3.                                                            ##
###############################################################################

boundaries <- function(indata, ypadding, xpadding, choice){
  indata <- as.matrix(indata)
  ny <- dim(indata)[1]
  nx <- dim(indata)[2]
  
  if(ypadding == 0){ # Fixes ypadding in case of mixup using 1d indata
    ypadding <- xpadding
  }
  
  if(xpadding >= nx){ # Reduces size of padding if equal to or larger than object
    xpadding <- xpadding-1
  }
  
  if(ypadding >= ny){ # Reduces size of padding if equal to or larger than object
    ypadding <- ypadding-1
  }
    
  switch(choice, # Type of boundary condition
         ignore={
           outdata <- as.numeric(indata) # Returns indata unaltered for further processing
           
           return(outdata)
         },
         
         zeros={ # Zero padding
           outdata <- rbind( # Pads rows on both sides of indata
             matrix(data = 0,
                    nrow = ypadding,
                    ncol = nx+2*xpadding),
             indata,
             matrix(data = 0,
                    nrow = ypadding, 
                    ncol = nx+2*xpadding))
           
           if(nx > 1){ # Only pads columns if indata is 2d
             outdata <- cbind( # Pads columns on both sides of the row padded outdata
               matrix(data = 0,
                      nrow = ny,
                      ncol = xpadding),
               outdata,
               matrix(data = 0,
                      nrow = ny,
                      ncol = xpadding)
             )
           }
           
           return(outdata)
         },
         
         cpad={ # Padding with boundary constant value
           outdata <- rbind(# Pads rows
             matrix(data = indata[1,],
                    nrow = ypadding,
                    ncol = nx,
                    byrow = T),
             indata,
             matrix(data = indata[ny,],
                    nrow = ypadding,
                    ncol = nx,
                    byrow = T)
           )
           
           if(nx > 1){
             outdata <- cbind( # Pads columns
               matrix(data = outdata[,1],
                      nrow = ny+2*ypadding,
                      ncol = xpadding),
               outdata,
               matrix(data = outdata[,nx],
                      nrow = ny+2*ypadding,
                      ncol = xpadding)
             )
           }
           
           return(outdata)
         },
           
         circular={ # Repeating entire signal
           outdata <- rbind( # Pads rows
             indata[(ny-ypadding+1):ny,],
             indata,
             indata[1:ypadding,]
           )
             
           if(nx > 1){
             outdata <- cbind( # Pads columns
               outdata[,(nx-xpadding+1):nx],
               outdata,
               outdata[,1:xpadding]
             )
           }
             
           return(outdata)
         },
         
         mirror={  # Symmetric replication
           outdata <- rbind( # Pads rows
             indata[ypadding:1,],
             indata,
             indata[ny:(ny-ypadding+1),]
           )
             
           if(nx > 1){
              outdata <- cbind( # Pads columns
               outdata[,xpadding:1], # Inverts signal at the boundary
               outdata,
               outdata[,nx:(nx-xpadding+1)]
             )
           }
       
           return(outdata)
         },
         
         stop("error: invalid choice of boundary condition")
        )
}
