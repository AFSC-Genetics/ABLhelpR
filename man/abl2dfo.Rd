\name{abl2dfo}
\alias{abl2dfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format microsatellite alleles called in different labs.
}
\description{
Reformat microsatellite calls to standardize across labs.
}
\usage{
abl2dfo(Infile,GenoStartCol,AlleleConversions,Outfile,DataSheet,rows2skip=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Infile}{
an excel or .csv formatted file that needs to be processed. If an excel file the DataSheet argument is required.
}
\item{GenoStartCol}{
In which column do the genotypes start.
}
\item{AlleleConversions}{
an object that has all the standardized allele calls.
}
\item{Outfile}{
the name of the file to write.
}
\item{DataSheet}{
If an excel file is provided, this is the name of the sheet with the genotypes to convert.
}
\item{rows2skip}{
if you need to skip rows when reading in an excel file. Not valid for .csv files.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Writes out reformatted microsatellite genotypes.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Patrick Barry
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
abl2dfo(Infile = "./Chum_2010_ABL.csv",GenoStartCol = 2,AlleleConversions = "./ChumAlleleConversion.csv",Outfile = "./Chum_2010_DFO.csv",rows2skip=0)
}
