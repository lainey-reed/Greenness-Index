DrawROI_edited <-function(img,path_ROIs,file_name, roi.names=NULL, file.type=".jpg") {	
  #This function is edited from the original DrawROI function in the phenopix library
  #The original program takes a photo from the library and has the user draw the ROIs and then returns that shape
  #But since every photo is from a different angle, each photo needs it's own ROIs
  #All that is changed is the way the ROI is named so that they can be differentiated
  
  #First the image is displayed so the number of ROIs needed can be indicated
  ratio <- dim(img)[1]/dim(img)[2]
  par(mar=c(1,1,4,1))
  plot(0, type='n', xlim=c(0,1), ylim=c(0,1), axes=FALSE)
  rasterImage(img, xleft=0, ybottom=0, xright=1, ytop=ratio)
  nroi<-readline(prompt="Number of regions: ")
  nroi<-as.integer(nroi)
  
  if (is.null(roi.names)) roi.names <- paste0('roi', 1:nroi)
  # file <- list.files(path=path_img_ref,pattern = file.type)
  # # img<-readJpeg(paste(path_img_ref,file,sep=""))
  # img <- readJPEG(paste(path_img_ref,file,sep=""))
  ## convert values from 0:1 to 0:255
  img.converted <- img
  img.converted[,,1] <- img[,,1]*255
  img.converted[,,2] <- img[,,2]*255
  img.converted[,,3] <- img[,,3]*255
  
  ratio <- dim(img)[1]/dim(img)[2]
  #output list with ROI data
  roi.data <- list()
  
  for (i in (1:nroi)) {
    # dev.new()
    # infos <- Sys.info()['sysname']
    #  if (infos=='Darwin') quartz() else x11()
    # plot(img)
    par(mar=c(1,1,4,1))
    plot(0, type='n', xlim=c(0,1), ylim=c(0,1), axes=FALSE)
    rasterImage(img, xleft=0, ybottom=0, xright=1, ytop=ratio)
    
    #The following line was giving errors for some unknown reason
    title(main=paste('ROI',i,sep=""))
    
    vertices<-locator(type="l")
    polygon(vertices,lwd=2)
    # coordinates <- data.frame(rowpos=dim(img)[1]-vertices$y,colpos=vertices$x) #rowpos=dim(img)[1]-vertices$y perch locator ha coordinata y che parte dal basso
    coordinates <- data.frame(rowpos=ratio-vertices$y,colpos=vertices$x) #rowpos=dim(img)[1]-vertices$y perch locator ha coordinata y che parte dal basso
    
    # image.array <- expand.grid(rowpos=seq(1:nrow(img)),colpos=seq(1:ncol(img)))
    image.array <- expand.grid(rowpos=seq(1:nrow(img))/(nrow(img)/ratio),colpos=seq(1:ncol(img))/ncol(img))
    
    pixels.in.roi <- pnt.in.poly(image.array,coordinates) 
    
    #The following line was changed from the orignal to have file_name
    dev.print(jpeg, file=paste(path_ROIs,file_name,'_',roi.names[i],".jpg",sep=''), width=1024, height=1024)
    dev.off()
    
    plant<-readline(prompt="What plant is in this region: ")
    
    out <- list(pixels.in.roi,vertices,plant)
    names(out) <- c('pixels.in.roi','vertices','Plant')
    
    roi.data[[i]] <- out
  }
  
  names(roi.data) <- roi.names
  
  #The following line was changed to have file_name
  #Also added as write.csv to make data easier to use, but need the Rdata for the other function
  #May remove the write,csv as it makes the program run slower
  write.csv(roi.data,file.path(paste(path_ROIs,file_name,'roi.data.Rdata.csv',sep='')))
  save(roi.data,file=paste(path_ROIs,file_name,'roi.data.Rdata',sep=''))
  return(invisible(roi.data))    
}

extractVIs_edit2 <- function(df,files,file.name,img.path,roi.path,vi.path=NULL,roi.name,plot=TRUE, begin=NULL, spatial=FALSE, date.code, npixels=1, 
                             file.type='.jpg', bind=FALSE, ncores='all', log.file=NULL) { 
  #The following code is edited from the original extractVIs function in the phenopix library
  #The original function analyzes several photos in parallel to determine the DNs and Greenness Indices of the photoset
  #This program instead analyzes one photo at a time since the photos have differing perspectives
  #Lines from the orignal code have been commented out
  #the output has been changed to append an existing table
  
  #files var is chr in form of "xxxx_mm_dd_yyyy_HHMM.jpg"
  
  #img.path
  roi.data <- NULL
  #to differentiate between ROIs they have the name of the photo in the Rdata file
  load(paste(roi.path,'/',file.name,'roi.data.Rdata',sep=''))
  if (is.null(roi.name)) {    
    roi.name <- names(roi.data)        
  } 
  
  roi.pos <- which(names(roi.data) %in% roi.name == TRUE)
  
  #only doing with one file, and already have the directory and the image loaded from earlier in the code
  # files <-list.files(path=img.path,recursive=TRUE, pattern = file.type)
  # n_files <-length(files)
  #instead just going to use the photo already loaded
  #files var is chr in form of "xxxx_mm_dd_yyyy_HHMM.jpg"
  dates <- as.POSIXct(sapply(files, extractDateFilename, date.code=date.code), origin='1970-01-01')
  if (any(is.na(dates))) stop(paste('Something wrong in your date!'))
  
  # if (!is.null(begin)) {
  #   beg.date <- as.POSIXct(begin, origin='1970-01-01')
  #   pos.good <- which(dates>=beg.date)          
  # } else {
  #   pos.good <- 1:n_files
  #   beg.date <- as.POSIXct('1970-01-01')
  # }
  # files <- files[pos.good]
  # n_files <- length(files)  
  # if (npixels!=1) {
  #   r <- brick(paste(img.path,'/',files[1],sep=''))
  #   aggregated.r <- aggregate(r,npixels)
  #   back.array <- raster::as.array(aggregated.r)
  #   sample.img <- back.array/255    
  #   roi.data <- updateROI(roi.data, sample.img)
  # }  
  if (spatial==FALSE) {
    VI.data <- list()
    k<-0
    #loop trough ROIs
    #this part is not changed from orignial because many do have multiple ROIs
    for (roi in roi.pos) { 
      k<-k+1
      temp.roi <- roi.data[[roi]]
      pos.pix.roi <- which(temp.roi$pixels.in.roi$pip == 1)
      temp.plant=temp.roi[3]
      VI.data.roi <- NULL
      ## loop trough images
      #if (ncores=='all') cores <- detectCores() else cores <- ncores
      #cl <- makeCluster(cores)
      #registerDoParallel(cl)
      # cl <- makeCluster(detectCores()-1)
      # registerDoParallel(cl)
      if (!is.null(log.file)) writeLines(c(""), paste(log.file, "log.txt", sep='/'))
      img <- NULL   
      #VI.data.roi
      #VI.data.roi <- #foreach(img=1:n_files, .packages=c('raster', 'phenopix'), .combine=rbind) %dopar% {
      #if (!is.null(log.file)) {
      #sink(paste(log.file, "log.txt", sep='/'), append=TRUE)  
      #cat(paste(round(img/n_files*100), '% done\n'))
      #sink() 
      #}
      
      ## check date and begin
      temp.date <- extractDateFilename(paste(img.path,'/',files,sep=''), date.code)
      # if (is.na(temp.date)) stop('Something wrong in your date!')
      # print (files[img])      
      # temp.img <- readJpeg(paste(img.path,'/',files[img],sep=''))
      r <- brick(paste(img.path,'/',files,sep=''))
      if (npixels!=1) aggregated.r <- aggregate(r,npixels) else aggregated.r <- r
      red <- raster::as.array(raster(aggregated.r, 1))[,,1]
      green <- raster::as.array(raster(aggregated.r, 2))[,,1]
      blue <- raster::as.array(raster(aggregated.r,3))[,,1]
      red[temp.roi$pixels.in.roi$pip==0] <- NA
      green[temp.roi$pixels.in.roi$pip==0] <- NA
      blue[temp.roi$pixels.in.roi$pip==0] <- NA
      temp.r.av <- mean(red, na.rm=TRUE)
      temp.g.av <- mean(green, na.rm=TRUE)
      temp.b.av <- mean(blue, na.rm=TRUE)
      temp.r.sd <- sd(red, na.rm=TRUE)
      temp.g.sd <- sd(green, na.rm=TRUE)
      temp.b.sd <- sd(blue, na.rm=TRUE)
      temp.bri.av <- mean(red + green + blue, na.rm=TRUE)
      temp.bri.sd <- sd(red + green + blue, na.rm=TRUE)
      temp.gi.av <- mean(green/(red + green + blue),na.rm=TRUE)
      temp.gi.sd <- sd(green/(red + green + blue),na.rm=TRUE)
      temp.gei.av <- mean( 2*green - (red + blue),na.rm=TRUE)
      temp.gei.sd <- sd( 2*green - (red + blue),na.rm=TRUE)
      temp.ri.av <- mean(red/(red + green + blue),na.rm=TRUE)
      temp.ri.sd <- sd(red/(red + green + blue),na.rm=TRUE)
      temp.bi.av <- mean(blue/(red + green + blue),na.rm=TRUE)
      temp.bi.sd <- sd(blue/(red + green + blue),na.rm=TRUE)
      temp.doy <- as.numeric(format(temp.date,format="%j"))
      temp.VI <- data.frame(filename=file.name,ROI=k,date = temp.date, doy = temp.doy, r.av = temp.r.av, g.av = temp.g.av, b.av = temp.b.av, r.sd = temp.r.sd, g.sd = temp.g.sd, b.sd = temp.b.sd, bri.av = temp.bri.av, bri.sd = temp.bri.sd,
                            gi.av = temp.gi.av, gi.sd = temp.gi.sd, gei.av = temp.gei.av, gei.sd = temp.gei.sd, ri.av = temp.ri.av, ri.sd = temp.ri.sd, bi.av = temp.bi.av, bi.sd = temp.bi.sd, plant=temp.plant)      
      names(temp.VI)=names(df)
      df=rbind(df,temp.VI)
      #} #endfor loop images  
      # stopCluster(cl)
      #end <- max(VI.data.roi$date, na.rm=TRUE)  
      #if (end < beg.date) stop('Your begin date is later than last record in your timeseries')
      #end <- trunc(end, 'day')
      VI.data[[roi]] <- temp.VI     
      # if (plot == TRUE & is.null(begin)) {
      #   png(filename=paste(vi.path,roi.name[roi],'_roi_VI_plot.png',sep=''), width=800, height=5*400, pointsize=30)  
      #   #    } else {
      #   # png(filename=paste(vi.path,begin, '_', end, '_',roi.name[roi],'_roi_VI_plot.png',sep=''), width=800, height=5*400, pointsize=30)           
      #   #    }
      #   par(mfrow=c(5,1))
      #   par(mar=c(3,4,2,0.5))
      #   plot(VI.data.roi$date,VI.data.roi$r.av,col='red',pch=20,xlab='',ylab='R-G-B',main=paste('ROI: ',roi.name[roi],sep=''))
      #   points(VI.data.roi$date,VI.data.roi$g.av,col='green',pch=20)  
      #   points(VI.data.roi$date,VI.data.roi$b.av,col='blue',pch=20)
      #   par(mar=c(3,4,0.5,0.5))
      #   plot(VI.data.roi$date,VI.data.roi$ri.av,col='red',pch=20,xlab='',ylab='RI')
      #   par(mar=c(3,4,0.5,0.5))  
      #   plot(VI.data.roi$date,VI.data.roi$gi.av,col='green',pch=20,xlab='',ylab='GI')
      #   par(mar=c(3,4,0.5,0.5))  
      #   plot(VI.data.roi$date,VI.data.roi$bi.av,col='blue',pch=20,xlab='',ylab='BI')
      #   par(mar=c(4,4,0.5,0.5)) 
      #   plot(VI.data.roi$date,VI.data.roi$bri.av,col='grey',pch=20,xlab='doy',ylab='BRI')
      #   
      #   dev.off()
      # }   
    } #endfor loop rois
    
    names(VI.data) <- roi.name
    if (is.null(begin)) {
      write.csv(VI.data,file=paste(vi.path,file.name,'.VI.data.Rdata.csv',sep=''))
    } else {
      if (bind) {
        VI.data.new <- VI.data
        write.csv(file=paste(vi.path,file.name,'.VI.data.Rdata.csv',sep=''))
        check <- VI.data.new[[1]]$date[1]<tail(VI.data[[1]]$date,1)
        if (check) warning('New begin date is prior to the end of the already existing records in VI.data! Check your dates')
        for (p in 1:length(VI.data)) VI.data[[p]] <- rbind(VI.data[[p]], VI.data.new[[p]])
        write.csv(VI.data,file=paste(vi.path,file.name,'.VI.data.Rdata.csv',sep=''))
      } else {
        write.csv(VI.data,file=paste(vi.path,file.name,'.VI.data.Rdata.csv',sep='')) 
      }
    }
  } else {## if spatial == TRUE
    VI.data <- list()
    for (roi in roi.pos) {    
      temp.roi <- roi.data[[roi]]
      pos.pix.roi <- which(temp.roi$pixels.in.roi$pip == 1)
      #loop trough images
      #if (ncores=='all') cores <- detectCores() else cores <- ncores
      #cl <- makeCluster(cores)
      #registerDoParallel(cl)
      # cl <- makeCluster(detectCores()-1)
      # registerDoParallel(cl)
      if (!is.null(log.file)) writeLines(c(""), paste(log.file, "log.txt", sep='/'))
      img <- NULL    
      img <- NULL
      VI.data.roi <- foreach(img=1:n_files, .packages=c('raster', 'phenopix')) %dopar% {
        if (!is.null(log.file)) {
          sink(paste(log.file, "log.txt", sep='/'), append=TRUE)  
          cat(paste(round(img/n_files*100), '% done\n'))
          sink() 
        }
        ## check date and begin
        temp.date <- extractDateFilename(files, date.code)
        #   if (!is.null(begin)) {
        #     beg.date <- as.POSIXct(begin, origin='1970-01-01')
        #     if (beg.date >= temp.date) next()
        #   }
        # print (files[img])
        r <- brick(paste(img.path,'/',files,sep=''))
        if (npixels!=1) aggregated.r <- aggregate(r,npixels) else aggregated.r <- r
        # temp.img <- readJpeg(paste(img.path,'/',files[img],sep=''))
        # temp.img <- readJPEG(paste(img.path,'/',files[img],sep=''))
        red <- raster::as.array(raster(aggregated.r, 1))[,,1]
        green <- raster::as.array(raster(aggregated.r, 2))[,,1]
        blue <- raster::as.array(raster(aggregated.r,3))[,,1]
        all.reds <- red[pos.pix.roi] 
        all.greens <- green[pos.pix.roi] 
        all.blue <- blue[pos.pix.roi] 
        pixel.df <- data.frame(red=all.reds, green=all.greens, blue=all.blue)
        # VI.data.roi[[img]] <- pixel.df
        # names(VI.data.roi)[img] <- temp.date
      }
      #stopCluster(cl)
      names(VI.data.roi) <- dates
      ### remove unprocessed data if 
      null.pos <- which(lapply(VI.data.roi, is.null)==TRUE)
      if (length(null.pos)!=0) VI.data.roi <- VI.data.roi[-null.pos]
      end <- max(dates, na.rm=TRUE)  
      if (!is.null(begin)) if (end < beg.date) stop('Your final date is later than last record in your timeseries')
      end <- trunc(end, 'day')
      VI.data[[roi]] <- VI.data.roi
    }  ## end roi loop
    names(VI.data) <- roi.name
    if (is.null(begin)) {
      write.csv(VI.data,file=paste(vi.path,file.name,'.VI.data.Rdata.csv',sep=''))
    } else {
      write.csv(VI.data,file=paste(vi.path,begin,'_',end,'_',file.name,'.VI.data.spatial.Rdata.csv',sep=''))
    }
  }
  invisible(df)   
}


######### Main body of code

#The main part of this code loops through the images in the folder and calls the two functions
# The code has to be in a loop because the drawing of the ROI has to be done each time
# For this code to work the pictures must be in a folder within the main project folder called "Pics"
# There also need to be two empty folders called ROIs and VIs for saving the data

#current_path<-'C:/Users/reedl/Documents/Photos of GCREW/All_photos/test/'
#ROI_path<-"C:/Users/reedl/Documents/Photos of GCREW/All_photos/test/ROIs"
#VI_path<-"C:/Users/reedl/Documents/Photos of GCREW/All_photos/test/VIs/"
library(phenopix)
library(raster)
library(jpeg)
library(SDMTools)
library(data.table)
#list.files('.')
list_pictures<-list.files(path='./Pics/',pattern=".jpg")
#(filename=fie.name,ROI=temp.roi,date = temp.date, doy = temp.doy, r.av = temp.r.av, g.av = temp.g.av, 
#b.av = temp.b.av, r.sd = temp.r.sd, g.sd = temp.g.sd, b.sd = temp.b.sd, bri.av = temp.bri.av,bri.sd = temp.bri.sd,
#gi.av = temp.gi.av, gi.sd = temp.gi.sd, gei.av = temp.gei.av, gei.sd = temp.gei.sd, ri.av = temp.ri.av, 
#ri.sd = temp.ri.sd, bi.av = temp.bi.av, bi.sd = temp.bi.sd
all_data=data.frame(Filename=character(),ROI=integer(),Date=as.Date(character()),DOY=integer(),
                    Red_Avg=double(),Green_Avg=double(),Blue_Avg=double(),Red_SD=double(),Green_SD=double(),
                    Blue_SD=double(),Brightness_Avg=double(),Brightness_SD=double(),Green_Index=double(),
                    Green_Index_SD=double(),Greenness_Excess_index=double(),Greenness_Excess_index_SD=double(),
                    Red_index=double(),Red_Index_SD=double(),Blue_Index=double(),Blue_Index_SD=double(),plant=character())
for (i in 1:length(list_pictures)){
  pic_name<-paste(list.files('./Pics/',pattern=".jpg")[i])
  file_name<-substr(pic_name,1,nchar(list_pictures[i])-4)
  current_image<-readJPEG(paste('./Pics/',pic_name,sep=""))
  #In the previous verseion, the nroi was recorded here by user input, but that is now within the function
  DrawROI_edited(current_image,"./ROIs/",file_name)
  all_data=extractVIs_edit2(all_data,list_pictures[i],file_name,'./Pics/', "./ROIs/", "./VIs/", NULL, 
                            plot = TRUE, begin = NULL, spatial = FALSE, date.code = "mm_dd_yy_HHMM", 
                            npixels=1, file.type='.jpg', bind=FALSE, ncores='all', log.file=NULL)
  # VIs_table=as.data.frame(VIs)
  # names(VIs)<-names(df)
  # rbind(df,VIs)
}

#dt2=setDT(all_data)[, list(DOY=mean(DOY),Green_Index=mean(Green_Index),Greenness_Excess_index=mean(Greenness_Excess_index)), Filename]
#plot(dt2$DOY,dt2$Green_Index)

