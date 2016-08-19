library(ggplot2)
library(plyr)
library(dplyr)
library(numDeriv)
library(reshape2)
library(ggvis)
library(foreach)

rotateSlice <- function(slice,angle){
  
  angleRad = -1*angle*pi/180.0
  
  newSlice <- data.frame(slice$x,slice$y,slice$z)
  
  newSlice$x = slice$x * cos(angleRad) + slice$y * sin(angleRad)
  newSlice$y = slice$y * cos(angleRad) - slice$x * sin(angleRad)
  newSlice$z = slice$z
  
  return(newSlice)
}


tranSlice <- function(circ,airfoil,chord,angle,radius,map,index){
  
  angleRad = -1*angle*pi/180.0
  
  newSlice <- data.frame(x=airfoil$x,y=airfoil$y,z=airfoil$z)
  
  lastCyl = map$d[4]
  firstAir = map$d[8]
  airAng = (map$tw[8] + 3.0)
  
  fraAir = (radius - lastCyl)/(firstAir - lastCyl)
  fraCyl = (firstAir - radius)/(firstAir - lastCyl)
  
  #airfoil = rotateSlice(airfoil,-1*airAng)
  
  
  
  newSlice$x = airfoil$x*fraAir + circ$x*fraCyl 
  newSlice$y = airfoil$y*fraAir + circ$y*fraCyl
  newSlice$z = rep(radius,times=length(newSlice$x))
  
  #nSQ = (max(newSlice$x) - min(newSlice$x))/4
  #offset = abs(newSlice$x[34]) - nSQ
  #newSlice$x = newSlice$x - offset
  
  #outSlice = rotateSlice(newSlice,0)
  #print(outSlice)
  
  return(newSlice)
}


setwd("C:/Users/ndeve/Dropbox/Research/Data/Aifoil_Coordinates/ibl_blade_write")
originalSlice <- read.csv(file="s809_C1_xyz_30percentZero.csv",sep = ",",head = TRUE)
originalCirc <- read.csv(file="circle1_nrelphase6.csv",sep = ",",head = TRUE)
bladeMap <- read.csv(file="blade_map_withtip.csv",sep = ",",head = TRUE)

coordN = length(originalSlice$x)
pitchAngle = 3.0
circDeg = seq(0,360,length=coordN)
circRad = circDeg*(pi/180)

n = length(bladeMap$d)
np = length(originalSlice$x)

bladeDef <- list()

# Cylinder and blade sections
for(i in 1:n){
  if(bladeMap$sh[i] == "s809"){
    tempSlice = rotateSlice(originalSlice,bladeMap$tw[i]+pitchAngle)
    bladeDef[[i]] <- data.frame(x=tempSlice$x*bladeMap$c[i],y=tempSlice$y*bladeMap$c[i],z=rep(bladeMap$d[i],times=length(tempSlice$x)))
  }
  if(bladeMap$sh[i] == "Cylinder"){
    tempCirc = rotateSlice(originalCirc,bladeMap$tw[i]+pitchAngle)
    bladeDef[[i]] <- data.frame(x=tempCirc$x*bladeMap$c[i]*0.5,y=tempCirc$y*bladeMap$c[i]*0.5,z=rep(bladeMap$d[i],times=length(tempCirc$x)))
  }
}

# Transitional sections
for(i in 1:n){
  if(bladeMap$sh[i] == "Transition"){
    bladeDef[[i]] <- tranSlice(bladeDef[[4]],bladeDef[[8]],bladeMap$c[i],bladeMap$tw[i]+pitchAngle,bladeMap$d[i],bladeMap,i)
  }
}

# Baffle Line to be added later
#for(i in 1:n){
#  baffleLine <- data.frame(p1=bladeDef[[i]][1,],p2=calcGradPoint())
#}


outfile = "bladesurface.ibl"
if (file.exists(outfile)) file.remove(outfile)

write("Open Arclength",file=outfile,append=TRUE)

cdef = 34
lup = 10
ldown = 11

for(i in 1:(n-1)){
 secText = paste("Begin section ! ",i,sep="")
 write(secText,file=outfile,append=TRUE)

 if(i<n-1){
   write("Begin curve ! 0",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][1:(cdef-lup),],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   write("Begin curve ! 1",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][(cdef-lup):(cdef+ldown),],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   write("Begin curve ! 2",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][(cdef+ldown):np,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)

   write("Begin curve ! 3",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][cdef+ldown,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   write.table(bladeDef[[i+1]][cdef+ldown,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   write("Begin curve ! 4",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][cdef-lup,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   write.table(bladeDef[[i+1]][cdef-lup,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   write("Begin curve ! 5",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][1,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   write.table(bladeDef[[i+1]][1,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
 }
 
 if(i==(n-1)){
   write("Begin curve ! 0",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][1:(cdef-lup),],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   write("Begin curve ! 1",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][(cdef-lup):(cdef),],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)

   write("Begin curve ! 2",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][(cdef):(cdef+ldown),],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   write("Begin curve ! 3",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][(cdef+ldown):np,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   write("Begin curve ! 4",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][1,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   write.table(bladeDef[[i+1]][1,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)

    lePoint = bladeDef[[n-1]][cdef,]
    leTop = bladeDef[[n-1]][cdef-lup,]
    leBot = bladeDef[[n-1]][cdef+ldown,]
    dleZ = bladeDef[[n]]$z[1]-bladeDef[[n-1]]$z[1]
    leX = (leTop$x + leBot$x)/2
    leXbot = (leX + leBot$x)/2
    leXtop = (leX + leTop$x)/2
    leDiam = leTop$y - leBot$y 
    tipP <- data.frame(x=leX, y=0.0,z=bladeDef[[n]]$z[1])
    cP1 <- data.frame(x=leXtop, y=leTop$y*sin(45*pi/180),z=bladeDef[[n-1]]$z[1]+dleZ*sin(45*pi/180)+0.005) 
    cP1off <- data.frame(x=leX, y=leTop$y - 0.0005, z=leTop$z + 0.01) 
    cP2 <- data.frame(x=leXbot, y=leBot$y*sin(45*pi/180),z=bladeDef[[n-1]]$z[1]+dleZ*sin(45*pi/180)+0.005)
    cP2off <- data.frame(x=leX, y=leBot$y + 0.0005, z=leBot$z + 0.01)
    capMid <- data.frame(x=lePoint$x*sin(45*pi/180), y=lePoint$y+0.005,z=bladeDef[[n-1]]$z[1]+dleZ*sin(45*pi/180)+0.0073) 
    lPoff <- data.frame(x=lePoint$x+0.0005, y=lePoint$y,z=lePoint$z+0.005)
    
    write("Begin curve ! 5",file=outfile,append=TRUE)
    write.table(bladeDef[[n]][1,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
    write.table(tipP,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
    

 }
#  if(i==(n)){
#    write("Begin curve ! 0",file=outfile,append=TRUE)
#    write.table(tipP,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
#    write.table(cP2,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
#    write.table(cP2off,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
#    write.table(leBot,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
#    
#    write("Begin curve ! 1",file=outfile,append=TRUE)
#    write.table(tipP,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
#    write.table(cP1,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
#    write.table(cP1off,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
#    write.table(leTop,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
#    
#    write("Begin curve ! 2",file=outfile,append=TRUE)
#    write.table(tipP,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
#    write.table(capMid,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
#    write.table(lPoff,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
#    write.table(lePoint,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
#    
#  }

 

}

plot(originalSlice$x,originalSlice$y,ylim=c(-0.4,0.4),asp=1)
#par(new=TRUE)
#plot(outSlice$x,outSlice$y,col="red",ylim=c(-0.4,0.4),asp=1,axes=FALSE)

