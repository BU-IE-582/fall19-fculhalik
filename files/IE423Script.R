library(jpeg)
library(fitdistrplus)
library(graphics)
library(grDevices)

img <- readJPEG("C:/Users/Abdulsamed/Downloads/mutfakduvari.jpg")
imggray<-readJPEG("C:/Users/Abdulsamed/Downloads/grimutfakduvari.jpeg")
str(img)
dim(img)
class(img)

if(exists("rasterImage")){
  plot(1:2, type='n', xlab = "", ylab = "")
  rasterImage(img,1,1,2,2,interpolate = FALSE)}
  #Colorful
  par(cex=0.5, mai=c(0.2,0.2,0.2,0.2))
  par(mar=rep(1,4))
  par(fig=c(0,0.33,0.33,0.66))
  image(img[,,1],col=hcl.colors(12, "Reds"),main="R Channel")
  par(fig=c(0.33,0.66,0.33,0.66), new=TRUE)
  image(img[,,2],col=hcl.colors(12, "Greens"),main="G Channel")
  par(fig=c(0.66,0.99,0.33,0.66), new=TRUE)
  image(img[,,3],col=hcl.colors(12, "Blues"),main="B Channel")
  #Grayscale
  par(cex=0.5, mai=c(0.2,0.2,0.2,0.2))
  par(cex=0.5, mai=c(0.2,0.2,0.2,0.2))
  par(fig=c(0,0.33,0.33,0.66))
  image(img[,,1],col=gray.colors(20),main="R Channel")
  par(fig=c(0.33,0.66,0.33,0.66), new=TRUE)
  image(img[,,2],col=gray.colors(20),main="G Channel")
  par(fig=c(0.66,0.99,0.33,0.66), new=TRUE)
  image(img[,,3],col=gray.colors(20),main="B Channel")
  
  par(cex=1, mai=c(0.2,0.2,0.2,0.2))
  par(fig=c(0.1,0.9,0.1,0.9))
  x1 <- 0
  y1<- 0
  x2 <- 0
  y2<- 0
  x3 <- 0
  y3<- 0
  for (i in 1:512){
    x1[i] <-i
    y1[i] <-mean(img[,i,1])
  }
  plot (x1,y1,type="l" , col ="red",main="Average of Columns for Each Channel", ylim=c(0.2,0.52), xlim=c(1,512))  
  for (i in 1:512){
    x2[i]<-i
    y2[i]<-mean(img[,i,2])
  }
  lines (x2,y2,type="l" , col ="green")  
  for (i in 1:512){
    x3[i]<-i
    y3[i]<-mean(img[,i,3])
  }
  lines (x3,y3,type="l" , col ="blue") 
  legend("topleft", c("R Channel","G Channel","B Channel"),fill=c("red","green","blue"))
  
  imgnew <- array(0, dim = c(256,512,3))
  imgnew[,,1]<-abs(img[1:256,,1]-img[257:512,,1])
  imgnew[,,2]<-abs(img[1:256,,2]-img[257:512,,2])
  imgnew[,,3]<-abs(img[1:256,,3]-img[257:512,,3])
  if(exists("rasterImage")){
    plot(1:2, type='n', xlab = "", ylab = "")
    rasterImage(imgnew,1,1,2,2,interpolate = FALSE)}
  
  imgnew2 <-array(0,dim = c(512,512,3))
  
  temporary <- array()
  
  for(i in 3:510){
    for(m in 3:510){
      for(j in 1:3){
        for(k in -2:2){
          for(l in -2:2){
            temporary <- c(temporary,img[i+k,m+l,j])
          }
        }
        imgnew2[i,m,j] = median(temporary,na.rm=TRUE) 
        temporary<-array()
      }
    }
  }
  if(exists("rasterImage")){
    plot(1:2, type='n', xlab = "", ylab = "")
    rasterImage(imgnew2,1,1,2,2,interpolate = FALSE)
  }
  
  imgnew3 <-array(0,dim = c(512,512,3))
  temporary <- array()
  
  for(i in 6:507){
    for(m in 6:507){
      for(j in 1:3){
        for(k in -5:5){
          for(l in -5:5){
            temporary <- c(temporary,img[i+k,m+l,j])
          }
        }
        imgnew3[i,m,j] = median(temporary,na.rm=TRUE) 
        temporary<-array()
      }
    }
  }
  
  if(exists("rasterImage")){
    plot(1:2, type='n', xlab = "", ylab = "")
    rasterImage(imgnew3,1,1,2,2,interpolate = FALSE)
  }
  
  imgnew4 <-array(0,dim = c(512,512,3))
  temporary <- array()
  
  for(i in 16:497){
    for(m in 16:497){
      for(j in 1:3){
        for(k in -15:15){
          for(l in -15:15){
            temporary <- c(temporary,img[i+k,m+l,j])
          }
        }
        imgnew4[i,m,j] = median(temporary,na.rm=TRUE) 
        temporary<-array()
      }
    }
  }
  
  if(exists("rasterImage")){
    plot(1:2, type='n', xlab = "", ylab = "")
    rasterImage(imgnew4,1,1,2,2,interpolate = FALSE)
  }
  
  hist(imggray[,,1],breaks=20,main="Pixel Value Distribution",xlab="Pixel Values", ylab="Density",freq=FALSE)
  fitdistr(imggray[,,1],"normal")
  newz <-array(0,dim = c(512,512))
  newz[,] <- imggray[,,1]
  
  lowerlimit<-qnorm(0.0005, mean=0.3589119257, sd= 0.1784200553)
  upperlimit<-qnorm(0.9995, mean=0.3589119257, sd= 0.1784200553)
  for(x in 1:512){
    for(y in 1:512){
      
      if((newz[x,y]<lowerlimit)||(newz[x,y]>upperlimit))
      {newz[x,y] = 0}
    }
  }
  
  if(exists("rasterImage")){
    plot(x=c(0,2.1),y=c(0,1), type='n', xlab = "", ylab = "")
    rasterImage(newz,1.1,0,2.1,1,interpolate = FALSE)
    rasterImage(imggray[,,1],0,0,1,1,interpolate=FALSE) }
  
  i=array(1:512)
  m=array(1:512)
  j=array(1:3)
  k=array(1:51)
  l=array(1:51)
  patch<- array(dim = c(51,51))
  newimage<-array(imggray[,,1],dim = c(512,512))
  
  for (i in seq(26,487,by=51)) {
    for (m in seq(26,487,by=51)) {
      for (k in -25:25) {
        for (l in -25:25) {
          patch[k+26,l+26]=imggray[i+k,m+l,1]
        }
      }
      
      patchmean<-mean(patch[,])
      patchsd<-sd(patch[,])
      lowerlimit<-qnorm(0.0005, mean=patchmean, sd= patchsd)
      upperlimit<-qnorm(0.9995, mean=patchmean, sd= patchsd)
      
      for (n in -25:25) {
        for (p in -25:25) {
          if((patch[n+26,p+26]>upperlimit) | (patch[n+26,p+26]<lowerlimit)){
            newimage[i+n,m+p]=0
          }
        }
      }
    }
  }
  
  if(exists("rasterImage")){
    plot(1:2, type='n', xlab = "", ylab = "", main = "Blackened Image with 51 pixel steps")
    rasterImage(newimage,1,1,2,2,interpolate = FALSE)
  }
  
  if(exists("rasterImage")){
    plot(x=c(0,2.1),y=c(0,1), type='n', xlab = "", ylab = "")
    rasterImage(newimage,1.1,0,2.1,1,interpolate = FALSE)
    rasterImage(newz,0,0,1,1,interpolate = FALSE) }
  
  