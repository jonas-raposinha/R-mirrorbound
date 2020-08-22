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
             matrix(0,ny,xpadding),
             indata,
             matrix(0,ny,xpadding)
           )

           outdata <- rbind( # Pads rows
             matrix(0,ypadding, nx+2*xpadding),
             outdata,
             matrix(0,ypadding, nx+2*xpadding))
           
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
               outdata[(ny-ypadding+1):nx,],
               outdata,
               outdata[1:ypadding,]
             )
         
             return(outdata)
           },
         
           mirror={  # Symmetric replication conditions
             outdata <- matrix(0, ny+2*ypadding, nx+2*xpadding)
             sum <- 0
             yiter <- 0
             xiter <- 0
             kloop <- c((-ypadding):(ny+ypadding))[-(ypadding+1)] # Loop vectors excluding "0" position
             lloop <- c((-xpadding):(nx+xpadding))[-(xpadding+1)]
           
             for(k in kloop){ # Loops through y
               yout <- k # Position in the original matrix
               if(k < 0){
                 yout <- -k
               }
               if(k > ny){
                 yout <- 2*ny-k+1 
               }
               yiter <- yiter+1
             
               for(l in lloop){ # Loops through x
                 xout <- l # Position in the original matrix
                 if(l < 0){
                   xout <- -l
                 }
                 if(l > nx){
                   xout <- 2*nx-l+1
                 }
                 xiter <- xiter+1
                 outdata[yiter, xiter] <- indata[yout, xout] # Prints the padded matrix
               } 
               xiter <- 0
             }
             if(xpadding == 0){
               outdata <- as.numeric(outdata)
             }
             
             return(outdata)
             },
         
             stop("error: invalid choice of filter")
        )
}
