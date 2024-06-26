## Set options, load packages
options(stringsAsFactors=FALSE)
require(shiny,quietly=TRUE,warn.conflicts=FALSE)
require(DT,quietly=TRUE,warn.conflicts=FALSE)
require(Cairo,quietly=TRUE,warn.conflicts=FALSE)

## Import the Base WhiteBlaze Symbols font
if (Sys.info()['sysname'] != "Windows") {
  dir.create('~/.fonts')
  file.copy("www/Base WhiteBlaze Symbols-11-24-21.ttf", "~/.fonts")
  system('fc-cache -f ~/.fonts')
}

## Load elevation profile and trail waypoint files
load("data/at_profile_2024.Rdata",envir=sys.frame(0))
load("data/profile_labels_2024.Rdata",envir=sys.frame(0))

## Set variables used in multiple places in the app
at.length <- max(wb.points$dist.mi)
at.states <- c("Georgia","North Carolina/Tennessee","Virginia/West Virginia","Maryland","Pennsylvania",
  "New Jersey","New York","Connecticut","Massachusetts","Vermont","New Hampshire","Maine")

## Set the label text for the legend
legend.txt <- c(
  ",! = highway or paved road",
  ",^ = gravel or dirt road",
  ",+ = trail junction",
  ",D = trash cans",
  ",e = access to off-trail amenities",
  ",G = grocery store or other resupply",
  ",h = hiker hostel",
  ",H = hotel or motel",
  ",J = food storage (e.g. bear box)",
  ",k = trailhead parking",
  ",M = post office",
  ",N = picnic tables",
  ",O = fire or observation tower",
  ",p = privy or pit toilet",
  ",Q = landmark or point of interest",
  ",r = restaurant or food service",
  ",R = restrooms",
  ",s = shelter or lean-to",
  ",S = showers",
  ",t = tent sites",
  ",T = railroad crossing",
  ",u = waterfall",
  ",v = vista or overlook",
  ",w = water, reliable",
  ",W = water, seasonal",
  ",Y = power or gas line clearing",
  ",z = swimming area")

## Function to get all waypoints in a particular state
get.state.pts <- function(state) {
  if (is.null(state)) { return() }
  if (!(state %in% at.states)) { return() }
  index <- match(state,at.states)
  border.pts <- c(1,grep(" State Line",paste(wb.points$location,wb.points$add.text)),
    nrow(wb.points))[-c(3,5,6)]
  state.pts <- wb.points[border.pts[index]:border.pts[index+1],]
  return(state.pts)
}

## Function to get mileages based on waypoints
get.mile <- function(state,loc) {
  if (is.null(state)) { return() }
  if (is.null(loc)) { return() }
  state.pts <- subset(get.state.pts(state),location != "")
  state.table <- mapply(function(location,add.text)
    ifelse(add.text == "",location,paste(location,add.text,sep="/")),
      state.pts$location,state.pts$add.text)
  if (loc %in% state.table) {
    loc.index <- match(loc,state.table)
  } else { 
    try.vals <- unlist(strsplit(loc,split="/"))
    loc.index <- max(sapply(try.vals,match,table=state.table,nomatch=1))
  }
  loc.mi <- state.pts$dist.mi[loc.index]
  return(loc.mi)
}