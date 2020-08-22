###############################################################################
## Applies boundary conditions to a 1d or 2d object. Implemented in R        ##
## version 3.4.3.                                                            ##
###############################################################################

boundaries <- function(indata, ypadding, xpadding, choice){
  indata <- as.matrix(indata)
  ny <- dim(indata)[1]
  nx <- dim(indata)[2]
 
  if(xpadding >= nx){ # Reduces size of padding if equal to or larger than object
    xpadding <- xpadding-1
  }
  
  if(xpadding >= nx){ # Reduces size of padding if equal to or larger than object
    xpadding <- xpadding-1
  }
  
  switch(choice, # Type of boundary condition
         ignore={
           outdata <- as.numeric(indata) # Returns indata unaltered for further processing
           return(outdata)
         },
         
         zeros={ # Zero padding
           outdata <- cbind( # Pads columns
             matrix(data = 0,
                    nrow = ny,
                    ncol = xpadding),
             indata,
             matrix(data = 0,
                    nrow = ny,
                    ncol = xpadding)
           )

           outdata <- rbind( # Pads rows
             matrix(data = 0,
                    nrow = ypadding,
                    ncol = nx+2*xpadding),
             outdata,
             matrix(data = 0,
                    nrow = ypadding, 
                    ncol = nx+2*xpadding))
           
           return(outdata)
         },
         
         cpad={ # Padding with boundary constant value
           outdata <- cbind( # Pads columns
             matrix(data = indata[,1],
                    nrow = ny,
                    ncol = xpadding),
             indata,
             matrix(data = indata[,nx],
                    nrow = ny,
                    ncol = xpadding)
           )
           
           outdata <- rbind(# Pads rows
             matrix(data = outdata[1,],
                    nrow = ypadding,
                    ncol = nx+2*xpadding,
                    byrow = T),
             outdata,
             matrix(data = outdata[ny,],
                    nrow = ypadding,
                    ncol = nx+2*xpadding,
                    byrow = T)
           )
           
           return(outdata)
         },
           
           circular={ # Repeating entire signal
             outdata <- cbind( # Pads columns
               indata[,(nx-xpadding+1):nx],
               indata,
               indata[,1:xpadding]
             )

             outdata <- rbind( # Pads rows
               outdata[(ny-ypadding+1):ny,],
               outdata,
               outdata[1:ypadding,]
             )
         
             return(outdata)
           },
         
           mirror={  # Symmetric replication
             outdata <- cbind( # Pads columns
               indata[,xpadding:1], # Inverts signal at the boundary
               indata,
               indata[,nx:(nx-xpadding+1)]
             )
   
             outdata <- rbind( # Pads rows
               outdata[ypadding:1,],
               outdata,
               outdata[ny:(ny-ypadding+1),]
             )
             
             return(outdata)
           },
         
           stop("error: invalid choice of filter")
        )
}
