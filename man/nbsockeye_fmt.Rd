\name{nbsockeye_fmt}
\alias{nbsockeye_fmt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format sockeye salmon sample sheets from ADFG.
}
\description{
Reformat the sample sheets provided by ADFG for the D101 and D104 genetic stock identification project.
}
\usage{
nbsockeye_fmt(infile = )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{infile}{
the excel formatted file that needs to be processed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Writes out reformatted sockeye salmon tissue sample infor for plate maps. If no outfile
is supplied, the infile name is used as an outfile.
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
%%  Patrick Barry
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
nbsockeye_fmt(infile="./2024 101 Gillnet - 104 Seine Detailed ASL Samples SW 25-29.xlsx",
          outfile = "PlateMaps_D101_D104_SW25_29.csv")

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
 \keyword{ ~DNA }
 \keyword{ ~Reformat }
