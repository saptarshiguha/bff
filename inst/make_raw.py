# 1. Create source data (raw data if you will)
#  - 100 files, each file a 10 columns of normal numbers, last column string of 5
#    characters
#  - each file is N rows, roughly 20MB each 
#  - each file named 1 .. 100
import sys
import string
from threading import Thread

def moo(filename, ncols, nrows):
    import random
    f=open(filename, "w")
    for x in xrange(nrows):
        for y in xrange(ncols):
            g=random.gauss(10,2)
            f.write("%f " % g)
        s=random.randint(5,13)
        p=''.join(random.sample(string.uppercase,s))
        f.write("%s\n" % p)
    f.close()


if __name__== "__main__":
    if len(sys.argv)==4:
        moo(sys.argv[1],int(sys.argv[2]),int(sys.argv[3]))
    elif len(sys.argv)==5:
        for i in xrange(int(sys.argv[4])):
            print "Creating %s" % (sys.argv[1]+str(i))
            moo(sys.argv[1]+str(i), int(sys.argv[2]),int(sys.argv[3]))
            
    else:
        print("usage: moo filename ncols nrows")
        sys.exit(1)

# 150 files each 5,5mb, 5 minutes to create
# bff data set using 7 threads
        

