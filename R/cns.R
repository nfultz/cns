# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# cns(c(
#   "black",
#   "white",
#   "grey",
#   "gray",
#   "very light blue",
#   "moderate green",
#   "grayish purplish red"))


cns <- function(descriptions, ...) {

  c(descriptions, ...)  |>
    trimws()            |>
    tolower()           |>
    strsplit('[ _]+')   |>
    lapply(matchCNS)    |>
    vapply(cns2rgb, "") |>
    structure(class='cns')

}

ish <- function(ret, t) {
  stopifnot('splash colors are not allowed in the second position'=is.na(ret["hue"]))

  ret["hue"] <- switch(t,
    reddish   = 'red',
    orangish  = 'orange',
    brownish  = 'brown',
    yellowish = 'yellow',
    greenish  = 'green',
    bluish    = 'blue',
    purplish  = 'purple',
  )

  ret["ish"] <- "ish"

  ret

}


matchCNS <- function(tokens) {

  ret <- c(
    very = "", # blank for later pasting
    lightness = 'medium',
    saturation = 'vivid',
    hue  = NA_character_,
    ish  = NA_character_,
    hue2 = NA_character_
  )

  HUE <- "hue"
  DONE <- FALSE

  for(t in tokens) {

    if(!is.na(ret["hue"])) HUE <- "hue2"

    switch(t,
           black =,
           white = { ret["lightness"] <- t; DONE = TRUE},

           very =  { ret["very"] <- t; },

           dark =,
           medium =,
           light = { ret["lightness"] <- t; },



           grey = ,
           gray = { ret["saturation"] <- "gray"; DONE = TRUE},

           greyish  =,
           grayish  =,
           moderate =,
           strong   =,
           vivid    = { ret["saturation"] <- t },

           reddish   =,
           orangish  =,
           brownish  =,
           yellowish =,
           greenish  =,
           bluish    =,
           purplish  = { ret <- ish(ret, t) },

           red     =,
           orange  =,
           brown   =,
           yellow  =,
           green   =,
           blue    =,
           purple  = { ret[HUE] <- t },


           ## default
           warning(t, " not recognized.")

           )
    #print(t)
  }

  ret
}





cns2rgb <- function(x){

  if(is.na(x['hue'])) {
    if(x["lightness"] == "white") {
      return("#FFFFFF")
    }

    if(x["lightness"] == "black") {
      return("#000000")

    }

    if(x["saturation"] == "gray") {
      x["hue"] = "gray"
    }
  }


  h1 <- x["hue"] |> col2rgb() |> rgb2hsv()

  if(!is.na(x["hue2"])) {

    h2 <- x["hue2"] |> col2rgb() |> rgb2hsv()


    theta1 <- h1["h",]
    theta2 <- h2["h",]

    if(theta2 - theta1 > .5) {
      theta1 <- theta1 + 1
    } else if (theta2 - theta1 < -.5) {
      theta2 <- theta2 + 1
    }

    # ish colors are 3/4 hue2, 1/4 hue 1
    if(!is.na(x["ish"])) {

      h1["h",] <- 3/4*theta2 + 1/4*theta1

    } else {

      h1["h",] <- 1/2*theta2 + 1/2*theta1

    }

  }

  h1["s",] <- switch(x["saturation"],
                     gray=0,
                     grayish=.25,
                     moderate=.5,
                     strong=.75,
                     vivid=1)

  h1["v",] <- switch(paste0(x["very"], x["lightness"]),
                     veryblack=,
                     black = 0,
                     verydark =.1,
                     dark = .3,
                     medium = .5,
                     light = .7,
                     verylight = .9,
                     verywhite=,
                     white = 1)



  hsv(h1["h",], h1["s",], h1["v",])

}



#rgb2hsv(col2rgb(c("red", "orange", "yellow", "green", "blue", "purple")))



#x <- cns("light bluish purple")[[1]]



