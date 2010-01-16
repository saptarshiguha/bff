\name{bff}
\alias{bappend}
\alias{bload}

\title{bff: Big File? Format}
\description{
	Lazy Loading R Objects like S Plus  
}
\usage{

bappend(..., file)
bload(file)

}
\arguments{
  \item{...}{Objects, that must be named,symbols or characters(representing the object) only}
  \item{file}{The directory where the data will be stored}
}


\details{
	To add objects use \code{bappend(object1,object2,file=path-to-directory)}. To continue adding objects, use the same function e.g \code{bappend(object3,object4,file=path-to-directory)}


	To load a data set, \code{bload(file=path-to-directory)}


	A bff file format is a directory that must consist of two files \code{dict} and \code{object}. The former file is a list of one entry: \code{names} which is a hashtable of names and positions in the \code{object} file. An R object can be read from the \code{object} file without reading other objects.

	The data set is lazy loaded using \code{delayedAssign}: the names of all objects in the data are resent in memory but none are loaded until needed.

	There can be multiple subdirectories inside of \code{file} (arbitrarily deep), each directory must contain at the least \code{object} and \code{dict}. \code{bff} will recursively read in \code{dict} files and populate the current environment's namespace with the names of the objects present.
}
