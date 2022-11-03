\name{quant_fmt}
\alias{quant_fmt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format DNA quantification data.
}
\description{
Reformat the DNA quantification data from ABLs Synergy H1 excel file. 
}
\usage{
quant_fmt(infile = )
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
Writes out reformatted quantification data to the file specified. If no outfile
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
quant_fmt(infile = "DNA0047.xlsx",
          outfile = "DNA0047quants")

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
 \keyword{ ~DNA }
 \keyword{ ~Quantification }
