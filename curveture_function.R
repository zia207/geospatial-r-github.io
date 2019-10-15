curv <- function(dat, calc = "both"){
  # Function to calculate plan and profile curvatures from a DEM.
  # Author: Katriina O'Kane, UBC Geography, Vancouver
  # dat = DEM in raster form
  # calc = what calculation you want to perform, either "plan" for plan curvature, "profile" for profile curvature, or "both" to calculate both plan and profile curvature at the same time.
  
  #Define required vetors
  ans.pl <- c()
  ans.pr <- c()
  ans.x <- c()
  ans.y <- c()
  
  #Calculate
  for (i in 1:length(dat@data@values)){
    
    a <- adjacent(dat, cells=i, directions=8, pairs=TRUE)
    a <- a[,2]
    
    #Define cells
    C = res(dat)[1]
    Z0 = as.numeric(dat[i])
    Z1 = as.numeric(dat[a[1]])
    Z2 = as.numeric(dat[a[7]])
    Z3 = as.numeric(dat[a[4]])
    Z4 = as.numeric(dat[a[2]])
    Z5 = as.numeric(dat[a[5]])
    Z6 = as.numeric(dat[a[3]])
    Z7 = as.numeric(dat[a[8]])
    Z8 = as.numeric(dat[a[6]])
    
    #Define values
    D = ((Z4 + Z5)/2 - Z0)/C^2
    E = ((Z2 + Z7)/2 - Z0)/C^2
    f = (Z3 - Z1 + Z6 - Z8)/(4*C^2)
    G = (Z5 - Z4) / (2*C)
    H = (Z2 - Z7) / (2*C)
    
    #Calculate curvatures
    if (calc == "plan") {
      ans.pl = c(ans.pl, (2*(D*H^2 + E*G^2 - f*G*H)) / (G^2 + H^2))
      ans.x <- c(ans.x, as.numeric(coordinates(dat)[i,1]))
      ans.y <- c(ans.y, as.numeric(coordinates(dat)[i,2]))
    } else if (calc == "profile") {
      ans.pr = c(ans.pr, (-2*(D*G^2 + E*H^2 + f*G*H)) / (G^2 + H^2))
      ans.x <- c(ans.x, as.numeric(coordinates(dat)[i,1]))
      ans.y <- c(ans.y, as.numeric(coordinates(dat)[i,2]))
    } else if (calc == "both") {
      ans.pl <- c(ans.pl, (2*(D*H^2 + E*G^2 - f*G*H)) / (G^2 + H^2))
      ans.pr <- c(ans.pr, (-2*(D*G^2 + E*H^2 + f*G*H)) / (G^2 + H^2))
      ans.x <- c(ans.x, as.numeric(coordinates(dat)[i,1]))
      ans.y <- c(ans.y, as.numeric(coordinates(dat)[i,2]))
    } else {
      stop(return ("Error, did not specify calc properly"))
    }
  }
  
  #Combine into data frams
  if (length(ans.pl) > 0 & length(ans.pr) == 0) {
    ans <- data.frame("x" = ans.x, "y" = ans.y, "plan" = ans.pl)
  } else if (length(ans.pl) == 0 & length(ans.pr) > 0) {
    ans <- data.frame("x" = ans.x, "y" = ans.y, "profile" = ans.pr)
  } else if (length(ans.pl) > 0 & length(ans.pr) > 0) {
    ans <- data.frame("x" = ans.x, "y" = ans.y, "plan" = ans.pl, "profile" = ans.pr)
  }
  
  #Return answer
  return(ans)
}