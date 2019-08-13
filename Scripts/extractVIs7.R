DrawROI_edited <-function(img,path_ROIs,file_name,roi.names=NULL, file.type=".jpg") {	
  #This function is edited from the original DrawROI function in the phenopix library
  #The original program takes a photo from the library 
  #and has the user draw the ROIs and then returns that shape
  #Try to make ROIs big as possible
  #But since every photo is from a different angle, each photo needs its own ROIs
  # So this function is run for every single picture
  #The function also asks how many ROIs are needed
  #The naming for the ROIs was also changed to differentiate between pictures
  
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
  
  #loops through the drawing of each ROI corresponding to the current image
  for (i in (1:nroi)) {
    # dev.new()
    # infos <- Sys.info()['sysname']
    #  if (infos=='Darwin') quartz() else x11()
    # plot(img)
    par(mar=c(1,1,4,1))
    plot(0, type='n', xlim=c(0,1), ylim=c(0,1), axes=FALSE)
    rasterImage(img, xleft=0, ybottom=0, xright=1, ytop=ratio)
    
    #The following line was giving errors for some unknown reason
    title(main=paste('ROI',i,', click "Finish" when done',sep=""))
    
    vertices<-locator(type="l")
    polygon(vertices,lwd=2)
    #sys.sleep(2)
    # coordinates <- data.frame(rowpos=dim(img)[1]-vertices$y,colpos=vertices$x) 
    #rowpos=dim(img)[1]-vertices$y perch locator ha coordinata y che parte dal basso
    coordinates <- data.frame(rowpos=ratio-vertices$y,colpos=vertices$x) 
    #rowpos=dim(img)[1]-vertices$y perch locator ha coordinata y che parte dal basso
    
    # image.array <- expand.grid(rowpos=seq(1:nrow(img)),colpos=seq(1:ncol(img)))
    image.array <- expand.grid(rowpos=seq(1:nrow(img))/(nrow(img)/ratio),colpos=seq(1:ncol(img))/ncol(img))
    
    pixels.in.roi <- pnt.in.poly(image.array,coordinates) 
    
    #The following line was changed from the orignal to have file_name
    dev.print(jpeg, file=paste(path_ROIs,file_name,'_',roi.names[i],".jpg",sep=''), width=1024, height=1024)
    
    
    Major_Plants<-readline(prompt="What plants are in this region (major plant species, separate by spaces if more than one): ")
    Minor_Plants<-readline(prompt="What minor plants are in this region (enter N/A if none, separate by space if more than one): ")
    
    out <- list(pixels.in.roi,vertices,Major_Plants,Minor_Plants)
    names(out) <- c('pixels.in.roi','vertices','Major_Plants',"Minor_Plants")
    
    roi.data[[i]] <- out
    #out2<-as.data.frame(out)
    #names(out2)<-names(roi_data_all)
    fwrite(as.data.frame(out$pixels.in.roi),file=paste(path_ROIs,file_name,'roi.',i,'.data.csv',sep=''),col.names=TRUE)
    #roi_data_all<-rbind(roi_data_all,as.data.frame(out,row.names=NULL),deparse.level=0)
    dev.off()
  }
  
  names(roi.data) <- roi.names
  
  #The following line was changed to have file_name
  #Also added as write.csv to make data easier to use,
  # but still need the Rdata for the other function
  #May remove the write,csv as it makes the program run slower
  
  save(roi.data,file=paste(path_ROIs,file_name,'roi.data.Rdata',sep=''))
  #data2<-as.data.frame(roi.data)
  #write.csv(data2,file=paste(path_ROIs,file_name,'roi.data.csv',sep=''))
  #cbind(all.roi.data,roi.data)
  #return(invisible(roi_data_all))    
}

extractVIs_edit2 <- function(df,files,file.name,img.path,roi.path,vi.path,roi.name,
                             date.code, npixels=1, file.type='.jpg',log.file=NULL) { 
  #The following code is edited from the original extractVIs function in the phenopix library
  #The original function analyzes several photos in parallel to determ1ine the 
  #DNs and Greenness Indices of the photoset
  #This program instead analyzes one photo at a time since the photos 
  #have differing perspectives
  #Lines from the orignal code have been commented out
  #the output has been changed to append an existing table
  #a csv file is also created and put in the VIs folder within the project directory
  
  #files var is chr in form of "xxxx_mm_dd_yyyy_HHMM.jpg"
  
  #img.path
  roi.data <- NULL
  #to differentiate between ROIs they have the name of the photo in the Rdata file
  load(paste(roi.path,'/',file.name,'roi.data.Rdata',sep=''))
  if (is.null(roi.name)) { # its always Null   
    roi.name <- names(roi.data)        
  } 
  
  roi.pos <- which(names(roi.data) %in% roi.name == TRUE) 
  #instead just going to use the photo already loaded
  #files var is chr in form of "xxxx_mm_dd_yyyy_HHMM.jpg"
  dates <- as.POSIXct(sapply(files, extractDateFilename, date.code=date.code), origin='1970-01-01')
  if (any(is.na(dates))) stop(paste('Something wrong in your date!'))
    VI.data <- list()
    k<-0 #initialize counter for rois
    #loop trough ROIs
    #this part is not changed from orignial because many do have multiple ROIs
    for (roi in roi.pos) { 
      k<-k+1 #counter is for numbering the ROIs
      temp.roi <- roi.data[[roi]]
      pos.pix.roi <- which(temp.roi$pixels.in.roi$pip == 1)
      temp.major.plant=temp.roi[3]
      temp.minor.plant=temp.roi[4]
      VI.data.roi <- NULL
      ## loop trough images
      
      #for log.file a file path can be given to write a notepad file that logs the process
      if (!is.null(log.file)) writeLines(c(""), paste(log.file, "log.txt", sep='/'))
      img <- NULL 
      
      #The next lines are commented out because they are for analyzing multiple
      #pictures from the same vantage w the same ROIs at once
      
      
      ## check date and begin
      temp.date <- extractDateFilename(paste(img.path,'/',files,sep=''), date.code)
      
      #the brick function makes the photo into a raster object, which is
      #divided into layers from which the color bands can be extracted
      r <- brick(paste(img.path,'/',files,sep='')) 
      
      # the npixels changes the resolution of the photo being analyzed
      # the aggregate function in raster makes it lower resolution
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
      temp.VI <- data.frame(filename=file.name,ROI=k,date = temp.date, 
                            doy = temp.doy, r.av = temp.r.av, g.av = temp.g.av, 
                            b.av = temp.b.av, r.sd = temp.r.sd, g.sd = temp.g.sd, 
                            b.sd = temp.b.sd, bri.av = temp.bri.av, 
                            bri.sd = temp.bri.sd, gi.av = temp.gi.av, 
                            gi.sd = temp.gi.sd, gei.av = temp.gei.av, 
                            gei.sd = temp.gei.sd, ri.av = temp.ri.av, 
                            ri.sd = temp.ri.sd, bi.av = temp.bi.av, 
                            bi.sd = temp.bi.sd, Major_Plants=temp.major.plant,Minor_Plants=temp.minor.plant)      
      names(temp.VI)=names(df)
      df=rbind(df,temp.VI) #all data is stored in data table created at beginning
      
      #} 
      VI.data[[roi]] <- temp.VI     
    } #endfor loop rois
    
    names(VI.data) <- roi.name
    #write.csv(VI.data,file=paste(vi.path,file.name,'.VI.data.spatial.Rdata.csv',sep=''))
  
  invisible(df)   
}


######### Main body of code

#The main part of this code loops through the images in the folder and calls the two functions
# The code has to be in a loop because the drawing of the ROI has to be done each time
# For this code to work the pictures must be in a folder within the main project folder called "Pics"
# All pictures need to be named "vantagename_mm_dd_yy_HHMM.jpg" for this to work
# There also need to be two empty folders called ROIs and VIs for saving the data
# The program also allows you to identify plant groups within regions
# Major Plant groups are the ones that can be modelled,
# while the minor ones may just offer insight into some of the noisiness of the data

library(phenopix)
library(raster)
library(jpeg)
library(SDMTools)
library(data.table)

list_pictures<-list.files(path='./Pics/',pattern=".jpg")

#This table holds all the output data from the extractVIs function
#It is initialized here as an empty table, with columns appended to it in the function
all_data=data.frame(Filename=character(),ROI=integer(),Date=as.Date(character()),
                    DOY=integer(), Red_Avg=double(),Green_Avg=double(),
                    Blue_Avg=double(),Red_SD=double(),Green_SD=double(),
                    Blue_SD=double(),Brightness_Avg=double(),Brightness_SD=double(),
                    Green_Index=double(), Green_Index_SD=double(),
                    Greenness_Excess_index=double(),Greenness_Excess_index_SD=double(),
                    Red_index=double(),Red_Index_SD=double(),Blue_Index=double(),
                    Blue_Index_SD=double(),Major_Plants=character(),Minor_Plants=character())

#This loop runs to separately analyze each photo in folder
#Has to be loop (cannot use mapping) because new ROIs need to be drawn every time
for (i in 1:length(list_pictures)){
  #pulls current image from list of files
  pic_name<-paste(list.files('./Pics/',pattern=".jpg")[i])
  #takes of the .jpg of file to just have the name
  file_name<-substr(pic_name,1,nchar(list_pictures[i])-4)
  current_image<-readJPEG(paste('./Pics/',pic_name,sep=""))
  #In the previous verseion, the nroi was recorded here by user input, but that is now within the function
  DrawROI_edited(current_image,"./ROIs/",file_name)
  #check extrractVIs function for information on all the arguments
  all_data<-extractVIs_edit2(all_data,list_pictures[i],file_name,'./Pics/', "./ROIs/", 
                            "./VIs/", NULL, date.code = "mm_dd_yy_HHMM", 
                            npixels=1, file.type='.jpg', log.file=NULL)
}

write.csv(all_data,file=("./VIs/VI_data.csv"))

