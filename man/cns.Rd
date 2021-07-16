\name{cns}
\alias{cns}
\title{A Color Naming System}
\usage{
  cns(descriptions, ...)
}
\arguments{
  \item{descriptions, ...}{color names.}
}
\description{
  Convert color names to RGB hex codes.
}
\value{
  A character vector of RGB hex codes.
}
\details{
Color names may be provided with the following format:

\code{"<Lightness> <Saturation> <Hue> <Hue2>"}

where words are seperated by spaces, dashes, or underscores.

\code{<Lightness>} may take the values:

white, very light, light, medium, dark, very dark, black

\code{<Saturation>} may take the following values:

gray, grayish, moderate, strong, vivid.

\code{<Hue>} may take the values:

red, orange, brown, yellow, green, blue, purple

If two hues are specified, they are blended 1:1, unless the \code{'ish'} form
is used, which specifies a 3:1 blend.

If the hue is not built-in, the hue of the corresponding system color
is used.


}
\section{Note for SAS users}{

  This is parameterized using the 1986 paper, which probably
  has different values than the SAS 9 implementation. While the
  color names should be compatible, the shades may be slightly different,
  particularly for browns.

}

\section{References}{

Berk, T., Brownston, L., & Kaufman, A. (1982). A New Color-Naming System for Graphics Languages. IEEE Computer Graphics and Applications, 2(03), 37-44.

Kaufman, A. (1986). Computer artist's color naming system. The Visual Computer, 2(4), 255-260.

}

\examples{

  barplot(1:3, col=cns('light red', 'red', 'dark purplish red'))

  # Unmatched hues are looked up in the system colors but can still be modified
  barplot(1:3, col=cns(
    'dark dodgerblue',
    'medium royalblue4',
    'grayish green midnightblue')
  )

}
