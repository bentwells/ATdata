## Main server function for AT Data app
shinyServer(function(input,output) {
  
  ## Create starting location selection input
  output$start.location <- renderUI({
    if (is.null(input$start.state)) { return() }
    state.pts <- get.state.pts(input$start.state)
    start.pts <- subset(state.pts,location != "" & (grepl("k",symbols) |
      grepl(" State Line",paste(location,add.text)) | grepl("Terminus",add.text)))
    start.choices <- mapply(function(p,n) ifelse(n == "",p,paste(p,n,sep="/")),
      p=start.pts$location,n=start.pts$add.text)
    if (input$start.state == "Georgia") {
      start.choices[17] <- "Dicks Creek Gap/US Hwy 76"
    }
    if (input$start.state == "North Carolina/Tennessee") {
      start.choices[10] <- "Nantahala Outdoor Center/US Hwy 19"
      start.choices[20] <- "I-40/Pigeon River"
      start.choices[21] <- "Green Corner Rd/Standing Bear Farm"
      start.choices[31] <- "Devil Fork Gap/NC Hwy 212/TN Hwy 352"
      start.choices[37] <- "Indian Grave Gap/NC Hwy 197/TN Hwy 395"
      start.choices[40] <- "Iron Mtn Gap/NC Hwy 226/TN Hwy 107"
      start.choices <- append(start.choices,"US Hwy 19E",after=44)
      start.choices <- start.choices[-c(16,17,26,28,33,36,38,42,44)]
    }
    if (input$start.state == "Virginia/West Virginia") {
      start.choices[9] <- "Massie Gap/Grayson Highlands SP"
      start.choices[18] <- "Atkins, VA/US Hwy 11"
      start.choices[19] <- "Davis Valley Rd/VA Rd 617"
      start.choices[22] <- "Poor Valley Rd/VA Rd 625"
      start.choices[27] <- "Bland, VA/US Hwy 52"
      start.choices[35] <- "Pearisburg, VA/VA Hwy 100"
      start.choices <- append(start.choices,"New River/US Hwy 460",after=35)
      start.choices[40] <- "Johns Creek/VA Rd 632"
      start.choices <- append(start.choices,"Sinking Creek Valley/VA Hwy 42",after=41)
      start.choices[44] <- "Craig Creek/VA Rd 621"
      start.choices[48] <- "Catawba, VA/VA Hwy 311"
      start.choices[51] <- "Troutville, VA/US Hwy 11"
      start.choices[64] <- "James River/US Hwy 501"
      start.choices[67] <- "Robinson Gap Rd/VA Rd 607"
      start.choices[72] <- "North Salt Log Gap/VA Rd 634"
      start.choices[84] <- "Blackrock Gap/Skyline 87.4"
      start.choices <- append(start.choices,"Loft Mtn Campground (0.1)",after=88)
      start.choices <- append(start.choices,"Lewis Mtn Campground",after=96)
      start.choices[100] <- "Big Meadows Campground"
      start.choices[102] <- "Hawksbill Gap/Skyline 45.6"
      start.choices[104] <- "Skyland Loop Rd"
      start.choices[105] <- "Pinnacles Picnic Area/Skyline 36.7"
      start.choices[107] <- "Thornton Gap/US Hwy 211/Skyline 31.5"
      start.choices[109] <- "Elkwallow Wayside (0.1)/Skyline 23.9"
      start.choices[118] <- "Front Royal, VA/US Hwy 522"
      start.choices <- append(start.choices,"Ashby Gap/US Hwy 17/50",after=120)
      start.choices[127] <- "Harpers Ferry, WV/ATC Headquarters"
      start.choices <- start.choices[-c(2,17,23,26,45,50,52,54:58,62,65,66,
        75,77,81,83,85,87,88,90,94,96,101,103,106,110:113,115,116,124,126)]
    }
    if (input$start.state == "Maryland") {
      start.choices[9] <- "I-70/US Hwy 40"
      start.choices[13] <- "Raven Rock Rd/MD Rd 491"
      start.choices[14] <- "Pen-Mar County Park/MD/PA State Line"
      start.choices <- start.choices[-15]
    }
    if (input$start.state == "Pennsylvania") {
      start.choices[1] <- "Pen-Mar County Park/MD/PA State Line"
      start.choices[8] <- "Caledonia State Park/US Hwy 30"
      start.choices[15] <- "Pine Grove Furnace State Park"
      start.choices <- append(start.choices,"PA Hwy 34",after=15)
      start.choices <- append(start.choices,"PA Hwy 94",after=16)
      start.choices[25] <- "Carlisle, PA/US Hwy 11"
      start.choices[28] <- "Duncannon, PA/US Hwy 15/PA Hwy 274"
      start.choices <- append(start.choices,"Port Clinton, PA/PA Hwy 61",after=36)
      start.choices <- start.choices[-c(2,4,6,9,10,12,18,19,22,23,26,39)]
    }
    if (input$start.state == "New Jersey") {
      start.choices[9] <- "High Point State Park/NJ Hwy 23"
      start.choices[13] <- "Unionville, NY/Lott Rd"
      start.choices <- start.choices[-c(2,3,11)]
    }
    if (input$start.state == "New York") {
      start.choices[7] <- "NY Hwy 17/I-87"
      start.choices[10] <- "Hudson River/US Hwy 6/202"
      start.choices <- append(start.choices,"Albany Post Rd/US Hwy 9/NY Hwy 403",after=10)
      start.choices <- append(start.choices,"Taconic Parkway",after=15)
      start.choices <- start.choices[-c(8,20)]
    }
    if (input$start.state == "Connecticut") {
      start.choices <- append(start.choices,"CT Hwy 55",after=1)
      start.choices[5] <- "Kent, CT/CT Hwy 341"
      start.choices <- append(start.choices,"Cornwall Bridge Rd/CT Hwy 4",after=7)
      start.choices[10] <- "CT Hwy 112/US Hwy 7"
      start.choices <- start.choices[-c(4,6,7)]
    }
    if (input$start.state == "Massachusetts") {
      start.choices[3] <- "Undermountain Rd/MA Hwy 41"
      start.choices <- append(start.choices,"Great Barrington, MA/US Hwy 7",after=4)
      start.choices[16] <- "I-90/US Hwy 20"
      start.choices[24] <- "Cheshire, MA/MA Hwy 8"
      start.choices <- start.choices[-c(7,10,17,19,26)]
    }
    if (input$start.state == "Vermont") {
      start.choices <- append(start.choices,"White River/VT Hwy 14",after=20)
      start.choices <- append(start.choices,"Norwich, VT/US Hwy 5",after=21)
      start.choices <- start.choices[-c(10)]
    }
    if (input$start.state == "New Hampshire") {
      start.choices <- append(start.choices,"Hanover, NH/NH Hwy 120",after=1)
      start.choices <- append(start.choices,"Franconia Notch/I-93/US Hwy 3",after=14)
      start.choices <- start.choices[-c(5,10,13,18,21)]
    }
    if (input$start.state == "Maine") {
      start.choices[15] <- "Monson, ME/ME Hwy 15"
      start.choices[17] <- "Katahdin Ironworks Rd"
      start.choices[22] <- "The Birches Lean-tos (0.3)"
      start.choices <- start.choices[-c(9,11,16,19,20)]
    }
    names(start.choices) <- start.choices
    selectInput(inputId="start.location",label="Select Starting Location:",
      choices=start.choices,selected=start.choices[1])
  })
  
  ## Create ending location selection input
  output$end.location <- renderUI({
    if (is.null(input$end.state)) { return() }
    state.pts <- get.state.pts(input$end.state)
    end.pts <- subset(state.pts,location != "" & (grepl("k",symbols) |
      grepl(" State Line",paste(location,add.text)) | grepl("Terminus",add.text)))
    end.choices <- mapply(function(p,n) ifelse(n == "",p,paste(p,n,sep="/")),
      p=end.pts$location,n=end.pts$add.text)
    if (input$end.state == "Georgia") {
      end.choices[17] <- "Dicks Creek Gap/US Hwy 76"
    }
    if (input$end.state == "North Carolina/Tennessee") {
      end.choices[10] <- "Nantahala Outdoor Center/US Hwy 19"
      end.choices[20] <- "I-40/Pigeon River"
      end.choices[21] <- "Green Corner Rd/Standing Bear Farm"
      end.choices[31] <- "Devil Fork Gap/NC Hwy 212/TN Hwy 352"
      end.choices[37] <- "Indian Grave Gap/NC Hwy 197/TN Hwy 395"
      end.choices[40] <- "Iron Mtn Gap/NC Hwy 226/TN Hwy 107"
      end.choices <- append(end.choices,"US Hwy 19E",after=44)
      end.choices <- end.choices[-c(16,17,26,28,33,36,38,42,44)]
    }
    if (input$end.state == "Virginia/West Virginia") {
      end.choices[9] <- "Massie Gap/Grayson Highlands SP"
      end.choices[18] <- "Atkins, VA/US Hwy 11"
      end.choices[19] <- "Davis Valley Rd/VA Rd 617"
      end.choices[22] <- "Poor Valley Rd/VA Rd 625"
      end.choices[27] <- "Bland, VA/US Hwy 52"
      end.choices[35] <- "Pearisburg, VA/VA Hwy 100"
      end.choices <- append(end.choices,"New River/US Hwy 460",after=35)
      end.choices[40] <- "Johns Creek/VA Rd 632"
      end.choices <- append(end.choices,"Sinking Creek Valley/VA Hwy 42",after=41)
      end.choices[44] <- "Craig Creek/VA Rd 621"
      end.choices[48] <- "Catawba, VA/VA Hwy 311"
      end.choices[51] <- "Troutville, VA/US Hwy 11"
      end.choices[64] <- "James River/US Hwy 501"
      end.choices[67] <- "Robinson Gap Rd/VA Rd 607"
      end.choices[72] <- "North Salt Log Gap/VA Rd 634"
      end.choices[84] <- "Blackrock Gap/Skyline 87.4"
      end.choices <- append(end.choices,"Loft Mtn Campground (0.1)",after=88)
      end.choices <- append(end.choices,"Lewis Mtn Campground",after=96)
      end.choices[100] <- "Big Meadows Campground"
      end.choices[102] <- "Hawksbill Gap/Skyline 45.6"
      end.choices[104] <- "Skyland Loop Rd"
      end.choices[105] <- "Pinnacles Picnic Area/Skyline 36.7"
      end.choices[107] <- "Thornton Gap/US Hwy 211/Skyline 31.5"
      end.choices[109] <- "Elkwallow Wayside (0.1)/Skyline 23.9"
      end.choices[118] <- "Front Royal, VA/US Hwy 522"
      end.choices <- append(end.choices,"Ashby Gap/US Hwy 17/50",after=120)
      end.choices[127] <- "Harpers Ferry, WV/ATC Headquarters"
      end.choices <- end.choices[-c(2,17,23,26,45,50,52,54:58,62,65,66,
        75,77,81,83,85,87,88,90,94,96,101,103,106,110:113,115,116,124,126)]
    }
    if (input$end.state == "Maryland") {
      end.choices[9] <- "I-70/US Hwy 40"
      end.choices[13] <- "Raven Rock Rd/MD Rd 491"
      end.choices[14] <- "Pen-Mar County Park/MD/PA State Line"
      end.choices <- end.choices[-15]
    }
    if (input$end.state == "Pennsylvania") {
      end.choices[1] <- "Pen-Mar County Park/MD/PA State Line"
      end.choices[8] <- "Caledonia State Park/US Hwy 30"
      end.choices[15] <- "Pine Grove Furnace State Park"
      end.choices <- append(end.choices,"PA Hwy 34",after=15)
      end.choices <- append(end.choices,"PA Hwy 94",after=16)
      end.choices[25] <- "Carlisle, PA/US Hwy 11"
      end.choices[28] <- "Duncannon, PA/US Hwy 15/PA Hwy 274"
      end.choices <- append(end.choices,"Port Clinton, PA/PA Hwy 61",after=36)
      end.choices <- end.choices[-c(2,4,6,9,10,12,18,19,22,23,26,39)]
    }
    if (input$end.state == "New Jersey") {
      end.choices[9] <- "High Point State Park/NJ Hwy 23"
      end.choices[13] <- "Unionville, NY/Lott Rd"
      end.choices <- end.choices[-c(2,3,11,15,21)]
    }
    if (input$end.state == "New York") {
      end.choices[7] <- "NY Hwy 17/I-87"
      end.choices[10] <- "Hudson River/US Hwy 6/202"
      end.choices <- append(end.choices,"Albany Post Rd/US Hwy 9/NY Hwy 403",after=10)
      end.choices <- append(end.choices,"Taconic Parkway",after=15)
      end.choices <- end.choices[-c(8,20)]
    }
    if (input$end.state == "Connecticut") {
      end.choices <- append(end.choices,"CT Hwy 55",after=1)
      end.choices[5] <- "Kent, CT/CT Hwy 341"
      end.choices <- append(end.choices,"Cornwall Bridge Rd/CT Hwy 4",after=7)
      end.choices[10] <- "CT Hwy 112/US Hwy 7"
      end.choices <- end.choices[-c(4,6,7)]
    }
    if (input$end.state == "Massachusetts") {
      end.choices[3] <- "Undermountain Rd/MA Hwy 41"
      end.choices <- append(end.choices,"Great Barrington, MA/US Hwy 7",after=4)
      end.choices[16] <- "I-90/US Hwy 20"
      end.choices[24] <- "Cheshire, MA/MA Hwy 8"
      end.choices <- end.choices[-c(7,10,17,19,26)]
    }
    if (input$end.state == "Vermont") {
      end.choices <- append(end.choices,"White River/VT Hwy 14",after=20)
      end.choices <- append(end.choices,"Norwich, VT/US Hwy 5",after=21)
      end.choices <- end.choices[-c(10)]
    }
    if (input$end.state == "New Hampshire") {
      end.choices <- append(end.choices,"Hanover, NH/NH Hwy 120",after=1)
      end.choices <- append(end.choices,"Franconia Notch/I-93/US Hwy 3",after=14)
      end.choices <- end.choices[-c(5,10,13,18,21)]
    }
    if (input$end.state == "Maine") {
      end.choices[15] <- "Monson, ME/ME Hwy 15"
      end.choices[17] <- "Katahdin Ironworks Rd"
      end.choices[22] <- "The Birches Lean-tos (0.3)"
      end.choices <- end.choices[-c(9,11,16,19,20)]
    }
    names(end.choices) <- end.choices
    selectInput(inputId="end.location",label="Select Ending Location:",
      choices=end.choices,selected=end.choices[length(end.choices)])
  })
  
  ## Function to match start location to waypoints
  start.mi <- eventReactive(input$go.button,{
    if (is.null(input$start.location)) { return() }
    return(get.mile(state=input$start.state,loc=input$start.location))
  })
  
  ## Function to match end location to waypoints
  end.mi <- eventReactive(input$go.button,{
    if (is.null(input$end.location)) { return() }
    return(get.mile(state=input$end.state,loc=input$end.location))
  })
  
  ## Function to get direction (north/south)
  get.dir <- eventReactive(input$go.button,{
    if (is.null(start.mi())) { return() }
    if (is.null(end.mi())) { return() }
    if (start.mi() < end.mi()) { return("north") }
    if (start.mi() == end.mi()) { return() }
    if (start.mi() > end.mi()) { return("south") }
  })
  
  ## Function to return table of waypoints based on start/end locations
  get.pts <- eventReactive(input$go.button,{
    if (is.null(get.dir())) { return() }
    if (get.dir() == "north") {
      pts <- subset(wb.points,dist.mi >= start.mi() & dist.mi <= end.mi())
    }
    if (get.dir() == "south") {
      pts <- subset(wb.points,dist.mi <= start.mi() & dist.mi >= end.mi())
      pts$dist.mi <- at.length - pts$dist.mi
      pts <- pts[rev(1:nrow(pts)),]
    }
    return(pts)
  })
  
  ## Function to subset profile based on start and end locations
  get.prof <- eventReactive(input$go.button,{
    if (is.null(get.dir())) { return() }
    start.ind <- 10*start.mi()+1
    end.ind <- 10*end.mi()+1
    prof <- profile[start.ind:end.ind,]
    if (get.dir() == "south") {
      prof$dist.mi <- at.length - prof$dist.mi
      prof$change <- c(0,diff(prof$elev.ft))
      prof$grade <- round(prof$change/5.28,1)
    }
    return(prof)
  })
  
  ## Display slider bar beneath images
  output$panel.num <- renderUI({
    if (is.null(input$mi.per.panel)) { return() }
    if (is.null(get.pts())) { return() }
    pts <- get.pts()
    dist.mi <- abs(pts$dist.mi[1] - pts$dist.mi[nrow(pts)])
    N <- dist.mi %/% input$mi.per.panel + 1
    sliderInput("panel.num",label="Page Number",min=1,max=N,value=1,step=1,width="960px")
  })
  
  ## Display legend beneath slider bar
  output$legend.panel <- renderPlot({
    if (is.null(input$panel.num)) { return() }
    symbols <- unlist(strsplit(wb.points$symbols,split=","))
    s <- names(table(symbols))
    par(mar=c(0,0,0,0),bg="gray95")
    plot(x=NULL,y=NULL,type='n',axes=FALSE,xlab="",xlim=c(0,1),ylab="",ylim=c(0,1),main="")
    text(x=0,y=0.97,labels="Legend",pos=4,cex=1.5,family="Tahoma")
    text(x=0,y=seq(0.88,0.04,-0.07),labels=s[1:13],pos=4,cex=1.25,family="Base WhiteBlaze Symbols")
    text(x=0.5,y=seq(0.95,0.04,-0.07),labels=s[14:27],pos=4,cex=1.25,family="Base WhiteBlaze Symbols")
    text(x=0.02,y=seq(0.88,0.04,-0.07),labels=legend.txt[1:13],pos=4,cex=1.25,family="Tahoma")
    text(x=0.52,y=seq(0.95,0.04,-0.07),labels=legend.txt[14:27],pos=4,cex=1.25,family="Tahoma")
  })
  
  ## Display elevation profile plots in the main panel
  output$display.plot <- renderPlot({
    if (is.null(input$mile.count)) { return() }
    if (is.null(input$panel.num)) { return() }
    pts <- get.pts()
    prof <- get.prof()
    if (get.dir() == "south") {
      pts$text.x <- -pts$text.x
      pts$sym.x <- -pts$sym.x
    }
    if (input$mile.count == "Start from 0") {
      pts$dist.mi <- pts$dist.mi - pts$dist.mi[1]
      prof$dist.mi <- prof$dist.mi - prof$dist.mi[1]
    }
    begin <- pts$dist.mi[1] + (input$panel.num - 1)*input$mi.per.panel
    end <- pts$dist.mi[1] + input$panel.num*input$mi.per.panel
    pro <- subset(prof,dist.mi >= begin & dist.mi <= end)
    pt <- subset(pts,dist.mi >= begin & dist.mi <= end)
    ylim <- c(0,6660) - ifelse(max(pro$elev.ft) < 3000 | pt$location[1] == "Mt Katahdin" |
      pt$location[nrow(pt)] == "Mt Katahdin",1000,0)
    xaxs.lab <- seq(ceiling(begin/5),floor(end/5),1)*5
    yaxs.lab <- seq(0,ylim[2],1000)
    b1 <- 0.5 - ifelse(pt$dist.mi[1] - begin + min(pt$text.x[1],pt$sym.x[1]) < 0,
      pt$dist.mi[1] - begin + min(pt$text.x[1],pt$sym.x[1]),0)
    b2 <- 0.5 + ifelse(pt$dist.mi[nrow(pt)] - end + max(pt$text.x[nrow(pt)],pt$sym.x[nrow(pt)]) > 0,
      pt$dist.mi[nrow(pt)] - end + max(pt$text.x[nrow(pt)],pt$sym.x[nrow(pt)]),0)
    par(mar=c(1,1,0.1,0.1),mgp=c(0.75,0,0),tcl=0,ps=14)
    plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlab="",yaxs='i',ylab="",
      xlim=c(begin-b1,end+b2),ylim=ylim)
    if (max(pro$dist.mi) < end) {
      end <- max(pro$dist.mi)
      b2 <- 0.5 + ifelse(max(pt$text.x[nrow(pt)],pt$sym.x[nrow(pt)]) > 0,
        max(pt$text.x[nrow(pt)],pt$sym.x[nrow(pt)]),0)
    }
    polygon(x=c(end+b2,begin-b1,begin-b1,pro$dist.mi,end+b2),
      y=c(ylim[1],ylim[1],pro$elev.ft[1],pro$elev.ft,pro$elev.ft[nrow(pro)]),
      border="white",col="gray80",lwd=1,lend=2,ljoin=1)
    abline(h=seq(0,ylim[2],500),v=seq(ceiling(begin),floor(end),1),col="white",lwd=0.5,lend=2)
    lines(pro$dist.mi,pro$elev.ft,lwd=2,lend=2,ljoin=1)
    for (j in 1:nrow(pt)) {
      x.adj <- pt$dist.mi[j]+pt$text.x[j]
      y.mid <- pt$elev.ft[j]+(pt$text.pos[j]-3)*pt$text.y[j]/2
      y.end <- pt$elev.ft[j]+(pt$text.pos[j]-3)*pt$text.y[j]
      segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j],x1=pt$dist.mi[j],y1=y.mid,
        lwd=0.5,lend=2,ljoin=1)
      segments(x0=pt$dist.mi[j],y0=y.mid,x1=x.adj,y1=y.mid,lwd=0.5,lend=2,ljoin=1)
      segments(x0=x.adj,y0=y.mid,x1=x.adj,y1=y.end,lwd=0.5,lend=2,ljoin=1)
      if (pt$sym.pos[j] > 1 & pt$sym.pos[j] != pt$text.pos[j]) {
        x.adj <- pt$dist.mi[j]+pt$sym.x[j]
        y.mid <- pt$elev.ft[j]+(pt$sym.pos[j]-3)*pt$sym.y[j]/2
        y.end <- pt$elev.ft[j]+(pt$sym.pos[j]-3)*pt$sym.y[j]
        segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j],x1=pt$dist.mi[j],y1=y.mid,
          lwd=0.5,lend=2,ljoin=1)
        segments(x0=pt$dist.mi[j],y0=y.mid,x1=x.adj,y1=y.mid,lwd=0.5,lend=2,ljoin=1)
        segments(x0=x.adj,y0=y.mid,x1=x.adj,y1=y.end,lwd=0.5,lend=2,ljoin=1)
      }
      adj.x <- 0.009*input$mi.per.panel
      adj.y <- switch(paste("p",input$mi.per.panel,sep=""),p20=500,p25=405,p30=345,p35=295,p40=260)
      sym <- paste(unlist(strsplit(pt$symbols[j],split=",")),collapse="")
      sym.len <- strwidth(sym,family="Base WhiteBlaze Symbols")*adj.y
      if (sym.len > 0 & pt$sym.pos[j] %in% c(2,4)) {
        sym.x <- x.adj+ifelse(pt$sym.pos[j] == 2,adj.x,-adj.x)
        sym.y <- y.end+25*(pt$sym.pos[j]-3)
        text(x=sym.x,y=sym.y,labels=sym,pos=pt$sym.pos[j],srt=90,
          family="Base WhiteBlaze Symbols")
      }
      if (get.dir() == "south" & pt$sym.pos[j] %in% c(0,1,3,5)) {
        pt$sym.pos[j] <- switch(paste("p",pt$sym.pos[j],sep=""),p0=1,p1=0,p3=5,p5=3)
      }
      if (pt$sym.pos[j] == 0) { 
        segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j],x1=pt$dist.mi[j],
          y1=pt$elev.ft[j]-pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
        segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j]-pt$sym.y[j],x1=pt$dist.mi[j]-0.25,
          y1=pt$elev.ft[j]-pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
        text(x=pt$dist.mi[j],y=pt$elev.ft[j]-pt$sym.y[j],labels=sym,pos=2,
          family="Base WhiteBlaze Symbols")
      }
      if (pt$sym.pos[j] == 1) { 
        segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j],x1=pt$dist.mi[j],
          y1=pt$elev.ft[j]-pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
        segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j]-pt$sym.y[j],x1=pt$dist.mi[j]+0.25,
          y1=pt$elev.ft[j]-pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
        text(x=pt$dist.mi[j],y=pt$elev.ft[j]-pt$sym.y[j],labels=sym,pos=4,
          family="Base WhiteBlaze Symbols")
      }
      if (pt$sym.pos[j] == 3) { 
        segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j],x1=pt$dist.mi[j],
          y1=pt$elev.ft[j]+pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
        segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j]+pt$sym.y[j],x1=pt$dist.mi[j]+0.25,
          y1=pt$elev.ft[j]+pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
        text(x=pt$dist.mi[j],y=pt$elev.ft[j]+pt$sym.y[j],labels=sym,pos=4,
          family="Base WhiteBlaze Symbols")
      }
      if (pt$sym.pos[j] == 5) { 
        segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j],x1=pt$dist.mi[j],
          y1=pt$elev.ft[j]+pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
        segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j]+pt$sym.y[j],x1=pt$dist.mi[j]-0.25,
          y1=pt$elev.ft[j]+pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
        text(x=pt$dist.mi[j],y=pt$elev.ft[j]+pt$sym.y[j],labels=sym,pos=2,
          family="Base WhiteBlaze Symbols")
      }
      txt.x <- pt$dist.mi[j]+pt$text.x[j]+ifelse(pt$text.pos[j] == 2,adj.x,-adj.x)
      txt.y <- pt$elev.ft[j]+(pt$text.pos[j]-3)*(50+pt$text.y[j]+
        ifelse(pt$sym.pos[j] == pt$text.pos[j],sym.len,0))
      txt.lab <- ifelse(pt$add.text[j] == "",pt$location[j],
        paste(pt$location[j],pt$add.text[j],sep="/"))
      text(x=txt.x,y=txt.y,labels=txt.lab,pos=pt$text.pos[j],
        family="Tahoma",font=1,srt=90)
    }
    mtext(xaxs.lab,side=1,line=0,at=xaxs.lab,adj=c(0.25,rep(0.5,length(xaxs.lab)-2),0.75),
      family="Tahoma",font=2)
    mtext(yaxs.lab,side=2,line=0,at=yaxs.lab,adj=c(0,rep(0.5,length(yaxs.lab)-1)),
      family="Tahoma",font=2)
    box(lwd=1,lend=2,ljoin=1)
  })
  
  ## Display waypoint data table beneath slider bar
  output$display.table <- DT::renderDataTable({
    if (is.null(get.pts())) { return() }
    pts <- subset(get.pts(),location != "")
    if (input$mile.count == "Start from 0") {
      pts$dist.mi <- pts$dist.mi - pts$dist.mi[1]
    }
    loc <- mapply(function(p,n) ifelse(n == "",p,paste(p,n,sep="/")),
      p=pts$location,n=pts$add.text)
    table.out <- data.frame(Location=loc,
      Distance=c("",sprintf("%3.1f",diff(pts$dist.mi))),
      Total=sprintf("%6.1f",pts$dist.mi),
      Elevation=sprintf("%4.0f",pts$elev.ft),
      Change=c("",sprintf("%5.0f",diff(pts$elev.ft))),
      Grade=c("",sprintf("%4.1f",diff(pts$elev.ft)/(52.8*diff(pts$dist.mi)))),
      Amenities=pts$symbols)
      if (any(duplicated(table.out$Total))) {
        dups <- which(duplicated(table.out$Total))
        table.out$Location[dups-1] <- paste(table.out$Location[dups-1],table.out$Location[dups],sep="/")
        table.out <- table.out[-dups,]
      }
    datatable(table.out,
      options=list(autoWidth=TRUE,info=FALSE,ordering=FALSE,paging=FALSE,searching=FALSE,scrollY="600px",
      columnDefs=list(list(className='dt-left',width='300px',targets=0),
                      list(className='dt-center',targets=1:6))),
      class="compact hover row-border stripe",rownames=FALSE,
      colnames=c("Location/Waypoint","Distance (mi)","Total Distance (mi)",
      "Elevation (ft)","Elevation Change (ft)","Grade (%)","Amenities"))
  })
  
  ## Download images and tables from the main screen in PDF format
  output$download.data <- downloadHandler(
    filename=function() { 
      start <- gsub("/","",gsub(",","",gsub(" ","",input$start.location,fixed=TRUE),fixed=TRUE),fixed=TRUE)
      end <- gsub("/","",gsub(",","",gsub(" ","",input$end.location,fixed=TRUE),fixed=TRUE),fixed=TRUE)
      return(paste(start,"_to_",end,".pdf",sep="")) 
    },
    content=function(file) {
      if (is.null(get.pts())) { return() }
      if (is.null(get.prof())) { return() }
      pts <- get.pts()
      prof <- get.prof()
      if (get.dir() == "south") {
        pts$text.x <- -pts$text.x
        pts$sym.x <- -pts$sym.x
      }
      if (input$mile.count == "Start from 0") {
        pts$dist.mi <- pts$dist.mi - pts$dist.mi[1]
        prof$dist.mi <- prof$dist.mi - prof$dist.mi[1]
      }
      N <- (pts$dist.mi[nrow(pts)] - pts$dist.mi[1]) %/% input$mi.per.panel + 1
      CairoPDF(file,width=7.5,height=10,paper="letter",title="Appalachian Trail")
      par(mfrow=c(2,1))
      for (i in 1:N) {
        begin <- pts$dist.mi[1] + (i-1)*input$mi.per.panel
        end <- pts$dist.mi[1] + i*input$mi.per.panel
        pro <- subset(prof,dist.mi >= begin & dist.mi <= end)
        pt <- subset(pts,dist.mi >= begin & dist.mi <= end)
        ylim <- c(0,6660) - ifelse(max(pro$elev.ft) < 3000,1000,0)
        xaxs.lab <- seq(ceiling(begin/5),floor(end/5),1)*5
        yaxs.lab <- seq(0,ylim[2],1000)
        b1 <- 0.5 - ifelse(pt$dist.mi[1] - begin + min(pt$text.x[1],pt$sym.x[1]) < 0,
          pt$dist.mi[1] - begin + min(pt$text.x[1],pt$sym.x[1]),0)
        b2 <- 0.5 + ifelse(pt$dist.mi[nrow(pt)] - end + max(pt$text.x[nrow(pt)],pt$sym.x[nrow(pt)]) > 0,
          pt$dist.mi[nrow(pt)] - end + max(pt$text.x[nrow(pt)],pt$sym.x[nrow(pt)]),0)
        par(mar=c(0.75,0.75,0.1,0.1),mgp=c(0.75,0,0),tcl=0,ps=8)
        plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlab="",yaxs='i',ylab="",
          xlim=c(begin-b1,end+b2),ylim=ylim)
        if (i == N & max(prof$dist.mi) < end) {
          end <- max(prof$dist.mi)
          b2 <- 0.5 + ifelse(max(pt$text.x[nrow(pt)],pt$sym.x[nrow(pt)]) > 0,
            max(pt$text.x[nrow(pt)],pt$sym.x[nrow(pt)]),0)
        }
        polygon(x=c(end+b2,begin-b1,begin-b1,pro$dist.mi,end+b2),
          y=c(ylim[1],ylim[1],pro$elev.ft[1],pro$elev.ft,pro$elev.ft[nrow(pro)]),
          border="white",col="gray80",lwd=0.5,lend=2,ljoin=1)
        abline(h=seq(0,ylim[2],500),v=seq(ceiling(begin),floor(end),1),col="white",lwd=0.5,lend=2)
        lines(pro$dist.mi,pro$elev.ft,lwd=1,lend=2,ljoin=1)
        for (j in 1:nrow(pt)) {
          x.adj <- pt$dist.mi[j]+pt$text.x[j]
          y.mid <- pt$elev.ft[j]+(pt$text.pos[j]-3)*pt$text.y[j]/2
          y.end <- pt$elev.ft[j]+(pt$text.pos[j]-3)*pt$text.y[j]
          segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j],x1=pt$dist.mi[j],y1=y.mid,
            lwd=0.5,lend=2,ljoin=1)
          segments(x0=pt$dist.mi[j],y0=y.mid,x1=x.adj,y1=y.mid,lwd=0.5,lend=2,ljoin=1)
          segments(x0=x.adj,y0=y.mid,x1=x.adj,y1=y.end,lwd=0.5,lend=2,ljoin=1)
          if (pt$sym.pos[j] > 1 & pt$sym.pos[j] != pt$text.pos[j]) {
            x.adj <- pt$dist.mi[j]+pt$sym.x[j]
            y.mid <- pt$elev.ft[j]+(pt$sym.pos[j]-3)*pt$sym.y[j]/2
            y.end <- pt$elev.ft[j]+(pt$sym.pos[j]-3)*pt$sym.y[j]
            segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j],x1=pt$dist.mi[j],y1=y.mid,
              lwd=0.5,lend=2,ljoin=1)
            segments(x0=pt$dist.mi[j],y0=y.mid,x1=x.adj,y1=y.mid,lwd=0.5,lend=2,ljoin=1)
            segments(x0=x.adj,y0=y.mid,x1=x.adj,y1=y.end,lwd=0.5,lend=2,ljoin=1)
          }
          adj.x <- 0.012*input$mi.per.panel
          adj.y <- switch(paste("p",input$mi.per.panel,sep=""),p20=475,p25=380,p30=320,p35=275,p40=240)
          sym <- paste(unlist(strsplit(pt$symbols[j],split=",")),collapse="")
          sym.len <- strwidth(sym,family="Base WhiteBlaze Symbols")*adj.y
          if (sym.len > 0 & pt$sym.pos[j] %in% c(2,4)) {
            sym.x <- x.adj+ifelse(pt$sym.pos[j] == 2,adj.x+0.1,-adj.x)
            sym.y <- y.end+25*(pt$sym.pos[j]-3)
            text(x=sym.x,y=sym.y,labels=sym,pos=pt$sym.pos[j],srt=90,
              family="Base WhiteBlaze Symbols")
          }
          if (get.dir() == "south" & pt$sym.pos[j] %in% c(0,1,3,5)) {
            pt$sym.pos[j] <- switch(paste("p",pt$sym.pos[j],sep=""),p0=1,p1=0,p3=5,p5=3)
          }
          if (pt$sym.pos[j] == 0) { 
            segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j],x1=pt$dist.mi[j],
              y1=pt$elev.ft[j]-pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
            segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j]-pt$sym.y[j],x1=pt$dist.mi[j]-0.25,
              y1=pt$elev.ft[j]-pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
            text(x=pt$dist.mi[j],y=pt$elev.ft[j]-pt$sym.y[j],labels=sym,pos=2,
              family="Base WhiteBlaze Symbols")
          }
          if (pt$sym.pos[j] == 1) { 
            segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j],x1=pt$dist.mi[j],
              y1=pt$elev.ft[j]-pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
            segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j]-pt$sym.y[j],x1=pt$dist.mi[j]+0.25,
              y1=pt$elev.ft[j]-pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
            text(x=pt$dist.mi[j],y=pt$elev.ft[j]-pt$sym.y[j],labels=sym,pos=4,
              family="Base WhiteBlaze Symbols")
          }
          if (pt$sym.pos[j] == 3) { 
            segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j],x1=pt$dist.mi[j],
              y1=pt$elev.ft[j]+pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
            segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j]+pt$sym.y[j],x1=pt$dist.mi[j]+0.25,
              y1=pt$elev.ft[j]+pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
            text(x=pt$dist.mi[j],y=pt$elev.ft[j]+pt$sym.y[j],labels=sym,pos=4,
              family="Base WhiteBlaze Symbols")
          }
          if (pt$sym.pos[j] == 5) { 
            segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j],x1=pt$dist.mi[j],
              y1=pt$elev.ft[j]+pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
            segments(x0=pt$dist.mi[j],y0=pt$elev.ft[j]+pt$sym.y[j],x1=pt$dist.mi[j]-0.25,
              y1=pt$elev.ft[j]+pt$sym.y[j],lwd=0.5,lend=2,ljoin=1)
            text(x=pt$dist.mi[j],y=pt$elev.ft[j]+pt$sym.y[j],labels=sym,pos=2,
              family="Base WhiteBlaze Symbols")
          }
          txt.x <- pt$dist.mi[j]+pt$text.x[j]+ifelse(pt$text.pos[j] == 2,adj.x+0.1,-adj.x)
          txt.y <- pt$elev.ft[j]+(pt$text.pos[j]-3)*(50+pt$text.y[j]+
            ifelse(pt$sym.pos[j] == pt$text.pos[j],sym.len,0))
          txt.lab <- ifelse(pt$add.text[j] == "",pt$location[j],
            paste(pt$location[j],pt$add.text[j],sep="/"))
          text(x=txt.x,y=txt.y,labels=txt.lab,pos=pt$text.pos[j],
            family="Tahoma",font=1,srt=90)
        }
        mtext(xaxs.lab,side=1,line=-0.25,at=xaxs.lab,adj=c(0.25,rep(0.5,length(xaxs.lab)-2),0.75),
          family="Tahoma",font=2)
        mtext(yaxs.lab,side=2,line=0.05,at=yaxs.lab,adj=c(0,rep(0.5,length(yaxs.lab)-1)),
          family="Tahoma",font=2)
        box(lwd=0.5,lend=2,ljoin=1)
      }
      if (N %% 2 == 1) {
        symbols <- unlist(strsplit(wb.points$symbols,split=","))
        s <- names(table(symbols))
        txt <- sapply(legend.txt,function(x) substr(x,3,nchar(x)))
        par(mar=c(0,0,0,0))
        plot(x=NULL,y=NULL,type='n',axes=FALSE,xlab="",xlim=c(0,1),ylab="",ylim=c(0,1),main="")
        text(x=0,y=0.97,labels="Legend",pos=4,cex=1.5,family="Tahoma")
        text(x=0,y=seq(0.88,0.04,-0.07),labels=s[1:13],pos=4,cex=1.25,family="Base WhiteBlaze Symbols")
        text(x=0.5,y=seq(0.95,0.04,-0.07),labels=s[14:27],pos=4,cex=1.25,family="Base WhiteBlaze Symbols")
        text(x=0.02,y=seq(0.88,0.04,-0.07),labels=txt[1:13],pos=4,cex=1.25,family="Tahoma")
        text(x=0.52,y=seq(0.95,0.04,-0.07),labels=txt[14:27],pos=4,cex=1.25,family="Tahoma")
      }
      pts <- subset(pts,location != "")
      loc <- mapply(function(p,n) ifelse(n == "",p,paste(p,n,sep="/")),
        p=pts$location,n=pts$add.text)
      table.out <- data.frame(Location=loc,
        Distance=c("",sprintf("%3.1f",diff(pts$dist.mi))),
        Total=sprintf("%6.1f",pts$dist.mi),
        Elevation=sprintf("%4.0f",pts$elev.ft),
        Change=c("",sprintf("%5.0f",diff(pts$elev.ft))),
        Grade=c("",sprintf("%4.1f",diff(pts$elev.ft)/(52.8*diff(pts$dist.mi)))),
        Amenities=pts$symbols)
      if (any(duplicated(table.out$Total))) {
        dups <- which(duplicated(table.out$Total))
        table.out$Location[dups-1] <- paste(table.out$Location[dups-1],table.out$Location[dups],sep="/")
        table.out <- table.out[-dups,]
      }
      colnames(table.out) <- c("Location/Waypoint","Distance (mi)","Total (mi)",
        "Elevation (ft)","Change (ft)","Grade (%)","Amenities")
      P <- nrow(table.out) %/% 50 + 1
      par(mfrow=c(1,1),mar=c(0,0,0,0),ps=12)
      for (i in 1:P) {
        rows <- c(((i-1)*50+1):pmin(nrow(table.out),i*50))
        pr <- unlist(t(table.out[rows,]))
        bg <- sapply(1:length(pr),function(x) ifelse(((x-1) %/% 7) %% 2 == 1,"cyan","white"))
        layout(matrix(1:364,nrow=52,ncol=7,byrow=TRUE),widths=c(0.3,rep(0.1,5),0.2),heights=1/52)
        for (j in 1:7) { plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1)) }
        for (j in 1:7) {
          plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1),
            xaxs='i',xlab="",yaxs='i',ylab="",main="")
          rect(xleft=0,xright=1,ybottom=0,ytop=1,col='grey90',border='black')
          text(colnames(table.out)[j],x=0.5,y=0.5,adj=c(0.5,0.5))
        }
        for (j in 1:length(pr)) {
          plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1),
            xaxs='i',xlab="",yaxs='i',ylab="",main="")
          rect(xleft=0,xright=1,ybottom=0,ytop=1,col=bg[j],border='black')
          if (j %% 7 == 1) { text(pr[j],x=0.02,y=0.5,adj=c(0,0.5)) }
          if (j %% 7 %in% c(2:6)) { text(pr[j],x=0.5,y=0.5,adj=c(0.5,0.5)) }
          if (j %% 7 == 0) { pr[j] <- gsub(",","",pr[j])
            text(pr[j],x=0.5,y=0.5,adj=c(0.5,0.5),family="Base WhiteBlaze Symbols") }
        }
      }
      if (N %% 2 == 0) {
        symbols <- unlist(strsplit(wb.points$symbols,split=","))
        s <- names(table(symbols))
        txt <- sapply(legend.txt,function(x) substr(x,3,nchar(x)))
        par(mfrow=c(2,1),mar=c(0,0,0,0),ps=8)
        plot(x=NULL,y=NULL,type='n',axes=FALSE,xlab="",xlim=c(0,1),ylab="",ylim=c(0,1),main="")
        text(x=0,y=0.97,labels="Legend",pos=4,cex=1.5,family="Tahoma")
        text(x=0,y=seq(0.88,0.04,-0.07),labels=s[1:13],pos=4,cex=1.25,family="Base WhiteBlaze Symbols")
        text(x=0.5,y=seq(0.95,0.04,-0.07),labels=s[14:27],pos=4,cex=1.25,family="Base WhiteBlaze Symbols")
        text(x=0.02,y=seq(0.88,0.04,-0.07),labels=txt[1:13],pos=4,cex=1.25,family="Tahoma")
        text(x=0.52,y=seq(0.95,0.04,-0.07),labels=txt[14:27],pos=4,cex=1.25,family="Tahoma")
      }
    dev.off()
  })
})
