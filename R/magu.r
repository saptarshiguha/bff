.bsave <- function(..., file){
  system(paste("rm -rf ",file))
  dp <- file.info(file)['isdir']
  if(!is.na(dp)){
    stop(sprintf("%s is present",file))
  }
  stopifnot(dir.create(file))
  files <- paste(path.expand(file),c("dict","objects")
        ,sep=.Platform$file.sep)
  invisible(sapply(files,file.create))
  f1 <- file(files[1],"wb")
  dict <- new.env()
  save(dict,file=f1)
  close(f1)
  .append(...,file=files)
}

bappend <- function(..., file){
  files <- paste(path.expand(file),c("dict","objects")
                ,sep=.Platform$file.sep)
  dp <- file.info(file)['isdir']
  if(is.na(dp)){
    .bsave(...,file=file)
  }else{
    .append(..., file=files)
  }
}

.append <- function(..., file=1){
  ## browser()
  names <- as.character(substitute(list(...)))[-1L]
  load(file[1])
  objectfile <- file(file[2],"ab")
  curp <- seek(objectfile)
  sapply(names,function(r){
    save(list=r,file=objectfile)
    assign(r,list(curp,file[2]),envir=dict)
    curp <<- seek(objectfile)
  })
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
      load(objectfile)
      close(objectfile)
      get(varname)
    },assign.env=en)})
  message(sprintf("Lazy loaded %d objects", length(ls(dictinfo))))
  return(ls(dictinfo))
}
