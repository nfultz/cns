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
    strsplit('[ _-]+')  |>
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

  for(t in tokens) {

    switch(t,

           very = { ret["very"] <- t },

           black  =,
           white  =,
           dark   =,
           medium =,
           light  = { ret["lightness"] <- t },

           grey =,
           gray = { ret["saturation"] <- "gray"},

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
           purplish  = { ret <- ish(ret, t) ; HUE <- "hue2" },

           ish = { ret['ish'] <- t; HUE <- "hue2" },

           red     =,
           orange  =,
           brown   =,
           yellow  =,
           green   =,
           blue    =,
           purple  =,

           ## default
           { ret[HUE] <- t ; HUE <- "hue2" }

     )
  }

  ret
}


col2hue <- function(col) {
  switch(col,
         red     = 0.0,
         orange  = 0.1078,
         brown   = 0.1078,
         yellow  = 1 / 6,
         green   = 1 / 3,
         blue    = 2 / 3,
         purple  = 0.7692,
         gray    = 0,
         rgb2hsv(col2rgb(col))["h",]
         )
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


  h <- col2hue(x["hue"])

  if(!is.na(x["hue2"])) {

    h2 <- col2hue(x["hue2"])


    if(h2 - h > .5) {
      h <- h + 1
    } else if (h2 - h < -.5) {
      h2 <- h2 + 1
    }

    # ish colors are 3/4 hue2, 1/4 hue 1
    if(!is.na(x["ish"])) {

      h <- 3/4*h2 + 1/4*h

    } else {

      h <- 1/2*h2 + 1/2*h

    }

  }

  s <- switch(x["saturation"],
              gray     = 0,
              greyish  = 1/4,
              grayish  = 1/4,
              moderate = 2/4,
              strong   = 3/4,
              vivid    = 1)

  v <- switch(paste0(x["very"], x["lightness"]),
              veryblack =,
              black     = 0,
              verydark  = 1/6,
              dark      = 2/6,
              medium    = 3/6,
              light     = 4/6,
              verylight = 5/6,
              verywhite =,
              white     = 1)



  hsv(h, s, v)

}



#rgb2hsv(col2rgb(c("red", "orange", "yellow", "green", "blue", "purple")))



#x <- cns("light bluish purple")[[1]]

barplot(matrix(1, 28, 6), col=

cns(do.call(paste, expand.grid(c("very light"),c("grayish", "moderate", "strong", "vivid"), c("red", "orange", "brown", "yellow", "green", "blue", "purple"))))


)
