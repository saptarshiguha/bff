bappend <- function(..., file,recurse=TRUE){
  files <- paste(path.expand(file),c("dict","objects")
                ,sep=.Platform$file.sep)
  dp <- file.info(path.expand(file))['isdir']
  if(is.na(dp)){
    dir.create(path.expand(file))
    files <- paste(path.expand(file),c("dict","objects")
                   ,sep=.Platform$file.sep)
    invisible(sapply(files,file.create))
    f1 <- file(files[1],"wb")
    freepool=new.env();freepool$locs <- freepool$quan <- c()
    dictinfo <- list(names = new.env(),freepool=freepool)
    save(dictinfo,file=f1)
    close(f1)
  }else{
    files <- paste(path.expand(file),c("dict","objects")
                   ,sep=.Platform$file.sep)
    u=file.info(files)
    if(is.na(u[1,'size'])){
      invisible(sapply(files,file.create))
      f1 <- file(files[1],"wb")
      freepool=new.env(); freepool$locs <- freepool$quan <- c()
      dictinfo <- list(names=new.env(),freepool=freepool)
      save(dictinfo,file=f1)
      close(f1)
    }
  }
  .append(..., file=files,root=file)
}

.append <- function(..., list=character(0),file,root){
  ## browser()
  names <- as.character(substitute(list(...)))[-1L]
  names <- c(list, names)
  load(file[1])
  ## objectfile <- file(file[2],"ab")
  ## seek(objectfile,dict$end)
  ## curp <- seek(objectfile)
  sapply(names,function(r){
    ## browser()
    into <- dictinfo$names[[r]][[2]][1]
    if(is.null(into)) into <- file[2]
    objectfile <- file(into,"ab")
    curp <- seek(objectfile)
    save(list=r,file=objectfile)
    bytewritten <- seek(objectfile) - curp
    pointer <- dictinfo$names[[ r ]]
    if(!is.null(pointer)){
      ## browser()
      add_free_space_pool(dictinfo$freepool
                          ,start=pointer[[1]],size=pointer[[3]],pointer[[2]])
      pointer[[1]] <-  curp #c(curp,pointer[[1]])
      pointer[[2]] <-  into #c(into,pointer[[2]])
      pointer[[3]] <-  bytewritten #c(bytewritten,pointer[[3]])
    }else pointer <- list(curp,file[2],bytewritten)
    assign(r,pointer,envir=dictinfo$names)
    ## curp <<- seek(objectfile)
    close(objectfile)
  })
  ## dict$end <- seek(objectfile)
  ## close(objectfile)
  ## unlink(sprintf("%s%s.compact",root,.Platform$file.sep))
  
  save(dictinfo,file=file[1])
}  

add_free_space_pool <- function(en,start,size,file){
  ## browser()
  en$locs <- c(en$locs,start)
  en$quan <- c(en$quan,size)
  o <- order(en$locs)
  en$locs <- en$locs[o]; en$quan <- en$quan[o]
  
}
bload <- function(file,names=character(0),recurse=TRUE){
  ## iscompact <- sprintf("%s%s%s",path.expand(file),.Platform$file.sep,".compact")
  ## if(!is.na(file.info(iscompact)['size']) && !sweep){
  ##   dictfiles <- list.files(path.expand(file),pattern="dict",recursive=F,full=T)
  ## }else{
  ## browser()
  dictfiles <- list.files(path.expand(file),pattern="dict",recursive=recurse,full=T)
  ##   file.create(iscompact)
  ## }
  dictinfo <- list(names=new.env())
  ee <- new.env()
  y=(system.time(
  sapply(dictfiles,function(r){
    ## print(r)
    load(r,envir=ee)
    sapply(ls(ee$dictinfo$names),function(r){
      assign(r, ee$dictinfo$names[[r]],dictinfo$names)
    })})))
  if(y['elapsed']>.01)
    message(sprintf("Took %s seconds to read in index files", round(y['elapsed'],3)))
  save(dictinfo,file=sprintf("%s%s%s",path.expand(file),.Platform$file.sep,"dict"))
  en <- parent.frame()
  wh <- ls(dictinfo$names)
  if(length(names)>0) wh <- names
  v=system.time({
  sapply(wh,function(varname){
    delayedAssign(varname, value={
      objectfile=file(dictinfo$names[[varname]][[2]][1],"rb")
      seek(objectfile,dictinfo$names[[varname]][[1]][1])
      o=new.env()
      load(objectfile,env=o)
      close(objectfile)
      get(varname,envi=o)
    },assign.env=en)})})
  if(v['elapsed']>0.01)
    message(sprintf("Took %s seconds to lazy load  %s object(s)", round(v['elapsed'],3),length(wh)))
  ## message(sprintf("Lazy loaded %d objects", length(wh)))
  return(wh)
}

##1.1GB, 100 objects, creating R bff data set: 20 minutes, 4 threads.
