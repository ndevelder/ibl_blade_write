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
  airAng = (map$tw[8]+pitchAngle)
  
  fraAir = (radius - lastCyl)/(firstAir - lastCyl)
  fraCyl = (firstAir - radius)/(firstAir - lastCyl)
  
  newSlice$x = airfoil$x*fraAir + circ$x*fraCyl 
  newSlice$y = airfoil$y*fraAir + circ$y*fraCyl
  newSlice$z = rep(radius,times=length(newSlice$x))
  
  return(newSlice)
}


tranSliceCirc <- function(circ,airfoil,chord,angle,radius,map,index){
  
  angleRad = -1*angle*pi/180.0
  
  newSlice <- data.frame(x=airfoil$x,y=airfoil$y,z=airfoil$z)
  zeroAir <- rotateSlice(airfoil,-map$tw[8] - pitchAngle)
  
  lastCyl = map$d[4]
  firstAir = map$d[8]
  
  fraAir = (radius - lastCyl)/(firstAir - lastCyl)
  fraCyl = (firstAir - radius)/(firstAir - lastCyl)
  
  newSlice$x = zeroAir$x*fraAir + circ$x*fraCyl 
  newSlice$y = zeroAir$y*fraAir + circ$y*fraCyl
  newSlice$z = rep(radius,times=length(newSlice$x))
  
  newSlice <- rotateSlice(newSlice,angle)
  
  return(newSlice)
}

writeSec <- function(outfile,s){
  secText = paste("Begin section ! ",s,sep="")
  write(secText,file=outfile,append=TRUE)
  s = s+1
  return(s)
}


setwd("C:/Users/ndeve/Dropbox/Research/Data/Aifoil_Coordinates/ibl_blade_write")
originalSlice <- read.csv(file="s809_C1_xyz_30percentZero_addedTail.csv",sep = ",",head = TRUE)
originalCirc <- read.csv(file="circle1_nrelphase6_2.csv",sep = ",",head = TRUE)
originalCircBlend <- read.csv(file="circle1_nrelphase6.csv",sep = ",",head = TRUE)
bladeMap <- read.csv(file="blade_map_withtip.csv",sep = ",",head = TRUE)

coordN = length(originalSlice$x)
pitchAngle = 0.0
circDeg = seq(0,360,length=coordN)
circRad = circDeg*(pi/180)

n = length(bladeMap$d)
np = length(originalSlice$x)

bladeDef <- list()
bladeDefBlend <- list()

# Cylinder and blade sections
for(i in 1:n){
  if(bladeMap$sh[i] == "s809"){
    tempSlice = rotateSlice(originalSlice,bladeMap$tw[i]+pitchAngle)
    bladeDef[[i]] <- data.frame(x=tempSlice$x*bladeMap$c[i],y=tempSlice$y*bladeMap$c[i],z=rep(bladeMap$d[i],times=length(tempSlice$x)))
    bladeDefBlend[[i]] <- data.frame(x=tempSlice$x*bladeMap$c[i],y=tempSlice$y*bladeMap$c[i],z=rep(bladeMap$d[i],times=length(tempSlice$x)))
  }
  if(bladeMap$sh[i] == "Cylinder"){
    tempCirc = rotateSlice(originalCirc,bladeMap$tw[i]-8)
    bladeDef[[i]] <- data.frame(x=tempCirc$x*bladeMap$c[i]*0.5,y=tempCirc$y*bladeMap$c[i]*0.5,z=rep(bladeMap$d[i],times=length(tempCirc$x)))
    tempCirc = rotateSlice(originalCircBlend,bladeMap$tw[i]+23)
    bladeDefBlend[[i]] <- data.frame(x=tempCirc$x*bladeMap$th[i]/2,y=tempCirc$y*bladeMap$th[i]/2,z=rep(bladeMap$d[i],times=length(tempCirc$x)))
  }
}

# Transitional sections
for(i in 1:n){
  if(bladeMap$sh[i] == "Transition"){
    bladeDef[[i]] <- tranSlice(bladeDefBlend[[4]],bladeDefBlend[[8]],bladeMap$c[i],bladeMap$tw[i]+pitchAngle,bladeMap$d[i],bladeMap,i)
    plot(bladeDef[[i]]$x,bladeDef[[i]]$y,asp=1)
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

s=1

for(i in 1:(n-1)){

 if(i < 4){

   s1 = 21
   s2 = 41
   s3 = 61
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][1:s1,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][s1:s2,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][s2:s3,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][s3,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   write.table(bladeDef[[i+1]][s3,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][s2,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   write.table(bladeDef[[i+1]][s2,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][s1,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   write.table(bladeDef[[i+1]][s1,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
 }
 
 if(i == 4){
   
   s1 = 21
   s2 = 41
   s3 = 61
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][1:s1,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][s1:s2,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][s2:s3,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][s3,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   write.table(bladeDef[[i+1]][cdef+ldown,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][s2,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   write.table(bladeDef[[i+1]][cdef-lup,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][s1,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   write.table(bladeDef[[i+1]][1,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
 }
 
 
 if(i>4 && i<n-1){
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][1:(cdef-lup),],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][(cdef-lup):(cdef+ldown),],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][(cdef+ldown):np,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][cdef+ldown,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   write.table(bladeDef[[i+1]][cdef+ldown,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][cdef-lup,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   write.table(bladeDef[[i+1]][cdef-lup,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][1,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   write.table(bladeDef[[i+1]][1,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
 }
 
 if(i==(n-1)){
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][1:(cdef-lup),],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
#    s = writeSec(outfile,s)
#    write("Begin curve",file=outfile,append=TRUE)
#    write.table(bladeDef[[i]][(cdef-lup):(cdef),],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
#    
#    s = writeSec(outfile,s)
#    write("Begin curve",file=outfile,append=TRUE)
#    write.table(bladeDef[[i]][(cdef):(cdef+ldown),],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][(cdef-lup):(cdef+ldown),],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
   write.table(bladeDef[[i]][(cdef+ldown):np,],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
   
   s = writeSec(outfile,s)
   write("Begin curve",file=outfile,append=TRUE)
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
    
    s = writeSec(outfile,s)
    write("Begin curve",file=outfile,append=TRUE)
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

baffleTailP <- list()
baffleBafP <- list()
baffleDef <- list()

#Baffle Definition Loop
for(i in 1:(n-1)){
  
  if(bladeMap$sh[i]=="Cylinder"){
    bladeAngle = (bladeMap$tw[i])*pi/180.0
  }
  
  if(bladeMap$sh[i]=="Transition"){
    bladeAngle = (bladeMap$tw[i]+pitchAngle)*pi/180.0
  }
  
  if(bladeMap$sh[i]=="s809"){
    bladeAngle = (bladeMap$tw[i]+pitchAngle-3.0)*pi/180.0
  }
  
  bafLen = bladeMap$c[i]/16
  
  if(i<5){
    tailP = bladeDef[[i]][21,]
    bafP = tailP
    bafP[1] = bafP[1] + cos(bladeAngle)*bafLen
    bafP[2] = bafP[2] + sin(bladeAngle)*bafLen
  }
  
  if(i>4){
    tailP = bladeDef[[i]][1,]
    bafP = tailP
    bafP[1] = bafP[1] + cos(bladeAngle)*bafLen
    bafP[2] = bafP[2] + sin(bladeAngle)*bafLen
  }
  
  baffleDef[[i]] <- data.frame(angle=bladeAngle,length=bafLen)
  baffleTailP[[i]] <- tailP
  baffleBafP[[i]] <- bafP
    
}

# Baffle write loop
for(i in 1:(n-1)){

  s = writeSec(outfile,s)
  write("Begin curve",file=outfile,append=TRUE)
  write.table(baffleTailP[[i]],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
  write.table(baffleBafP[[i]],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
  
  if(i<(n-1)){
  s = writeSec(outfile,s)
  write("Begin curve",file=outfile,append=TRUE)
  write.table(baffleBafP[[i]],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
  write.table(baffleBafP[[i+1]],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
  }
  
}

# Baffle tip
bladeAngle = (bladeMap$tw[n]+pitchAngle)*pi/180.0
bafLen = bladeMap$c[n]/16
tailP = bladeDef[[n]][1,]
bafP = tailP
bafP[1] = bafP[1] + cos(bladeAngle)*bafLen
bafP[2] = bafP[2] + sin(bladeAngle)*bafLen
bafP[3] = bafP[3] + bafLen

bafPadd = tipP
bafPadd[1] = bafPadd[1]+bafLen*8
bafPadd[3] = bafPadd[3]+bafLen

bafPadd2 = tipP
bafPadd2[1] = bafPadd2[1]+bafLen*4
bafPadd2[3] = bafPadd2[3]+bafLen

bafPadd3 = tipP
bafPadd3[1] = bafPadd3[1]+bafLen*0.5
bafPadd3[3] = bafPadd3[3]+bafLen*0.67

s = writeSec(outfile,s)
write("Begin curve",file=outfile,append=TRUE)
write.table(tailP,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
write.table(bafP,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)

s = writeSec(outfile,s)
write("Begin curve",file=outfile,append=TRUE)
write.table(bafP,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
write.table(bafPadd3,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)

s = writeSec(outfile,s)
write("Begin curve",file=outfile,append=TRUE)
write.table(bafPadd3,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
write.table(tipP,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)

s = writeSec(outfile,s)
write("Begin curve",file=outfile,append=TRUE)
write.table(baffleBafP[[n-1]],outfile,row.names=FALSE,col.names=FALSE,append=TRUE)
write.table(bafP,outfile,row.names=FALSE,col.names=FALSE,append=TRUE)

plot(originalSlice$x,originalSlice$y,ylim=c(-0.4,0.4),asp=1)
#par(new=TRUE)
#plot(outSlice$x,outSlice$y,col="red",ylim=c(-0.4,0.4),asp=1,axes=FALSE)

