library(bff)
filename=Sys.getenv("INPUT_FILE")
outputfile=Sys.getenv("OUTPUT_FILE")
var_name=Sys.getenv("VARNAME")
assign(var_name,read.table(filename,header=F, stringsAsFactors=F))
bappend(list=var_name,file=outputfile)

x=bload(outputfile)
print(x)
