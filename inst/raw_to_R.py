# 1. Create source data (raw data if you will)
#  - 100 files, each file a 10 columns of normal numbers, last column string of 5
#    characters
#  - each file is N rows, roughly 20MB each 
#  - each file named 1 .. 100
import sys
import string
import os
from threading import Thread
from optparse import OptionParser

class MyThread(Thread):
   def __init__ (self,ofolder):
      Thread.__init__(self)
      self.files=[]
      self.ofolder=ofolder
   def append_file(self,f):
       self.files.append(f)
   def run(self):
       for i,x in enumerate(self.files):
           print self.ofolder+"::"+x
           ii=int(x.split("/")[-1])
           os.system("INPUT_FILE=%s OUTPUT_FILE=%s VARNAME=a%d R CMD BATCH --no-save inst/create_r.r /tmp/%d.out"% (x,self.ofolder,ii,ii))

def setup_options():
    usage="%prog [--threads=] output_folder "
    par=OptionParser(usage=usage)
    par.add_option("--threads","-t",dest="threads",default="1",type="string",help="Number of threads(R processes)")
    return(par)

def main(numthreads, ifolder,ofolder):
    ifolder=os.path.abspath(os.path.normpath(ifolder))
    ofolder=os.path.abspath(os.path.normpath(ofolder))
    try:
        os.mkdir(ofolder)
    except:
        pass
    files=map(lambda x: ifolder+os.sep+x,os.listdir(ifolder))
    thread_data=[]
    thread_pool=[]
    for i in range(numthreads):
        thread_data.append([])
        thread_pool.append( MyThread( ofolder+os.path.sep+str(i)))
    for i,f in enumerate(files):
        thread_pool[ i % numthreads ].append_file(f)
    for th in thread_pool:
        th.start()
    for th in thread_pool:
        th.join()

    
        
if __name__== "__main__":
    parser=setup_options()
    (ops, args)=parser.parse_args()
    if not len(args)==2:
        print("Please provide input folder and output dataset directory")
        exit(1)
    main(int(ops.threads),args[0],args[1])
