bappend <- function(..., file){
  files <- paste(path.expand(file),c("dict","objects")
                ,sep=.Platform$file.sep)
  dp <- file.info(path.expand(file))['isdir']
  if(is.na(dp)){
    dir.create(path.expand(file))
    files <- paste(path.expand(file),c("dict","objects")
                   ,sep=.Platform$file.sep)
    invisible(sapply(files,file.create))
    f1 <- file(files[1],"wb")
    dict <- new.env()
    save(dict,file=f1)
    close(f1)
  }else{
    files <- paste(path.expand(file),c("dict","objects")
                   ,sep=.Platform$file.sep)
    u=file.info(files)
    if(is.na(u[1,'size'])){
      invisible(sapply(files,file.create))
      f1 <- file(files[1],"wb")
      dict <- names=new.env()
      save(dict,file=f1)
      close(f1)
    }
  }
  .append(..., file=files)
}

.append <- function(..., list=character(0),file){
  ## browser()
  names <- as.character(substitute(list(...)))[-1L]
  names <- c(list, names)
  load(file[1])
  objectfile <- file(file[2],"ab")
  ## seek(objectfile,dict$end)
  curp <- seek(objectfile)
  sapply(names,function(r){
    save(list=r,file=objectfile)
    assign(r,list(curp,file[2]),envir=dict)
    curp <<- seek(objectfile)
  })
  ## dict$end <- seek(objectfile)
  close(objectfile)
  save(dict,file=file[1])
}  

bload <- function(file){
  dictfiles <- list.files(path.expand(file),pattern="dict",recursive=T,full=T)
  dictinfo <- new.env()
  sapply(dictfiles,function(r){
    load(r)
    sapply(ls(dict),function(r){
      assign(r, dict[[r]],dictinfo)
    })})
  en <- parent.frame()
  sapply(ls(dictinfo),function(varname){
    delayedAssign(varname, value={
      objectfile=file(dictinfo[[varname]][[2]],"rb")
      seek(objectfile,dictinfo[[varname]][[1]])
      o=new.env()
      load(objectfile,env=o)
      close(objectfile)
      get(varname,envi=o)
    },assign.env=en)})
  message(sprintf("Lazy loaded %d objects", length(ls(dictinfo))))
  return(ls(dictinfo))
}

##1.1GB, 100 objects, creating R bff data set: 20 minutes, 4 threads.
