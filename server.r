options(stringsAsFactors=FALSE,width=150)
require(shiny,quietly=TRUE,warn.conflicts=FALSE)
require(DT,quietly=TRUE,warn.conflicts=FALSE)

shinyServer(function(input,output) {
  ## Load elevation profile and trail waypoint files
  load("data/at_profile_2018.Rdata",envir=sys.frame(0))
  load("data/at_waypoints.Rdata",envir=sys.frame(0))
  
  ## Create starting location selection input
  output$start.location <- renderUI({
    if (is.null(input$start.state)) { return() }
    state.choices <- c("Georgia","North Carolina/Tennessee","Virginia",
      "West Virginia/Maryland","Pennsylvania","New Jersey","New York",
      "Connecticut","Massachusetts","Vermont","New Hampshire","Maine")
    index <- match(input$start.state,state.choices)
    border.pts <- c(1,grep("Border",paste(at.points$to.location,at.points$notes)),
      nrow(at.points))[-c(3,6)]
    state.pts <- at.points[border.pts[index]:border.pts[index+1],]
    check.txt <- subset(state.pts,sapply(paste(to.location,notes,sep="/"),function(n)
      grepl(" Rd",n) | grepl("Hwy",n) | grepl("I-",n) | grepl("Border",n) | 
      grepl("BRP",n) | grepl("Skyline",n)))
    start.choices <- mapply(function(p,n) ifelse(n == "",p,paste(p,n,sep="/")),
      p=check.txt$to.location,n=check.txt$notes)
    if (input$start.state == "Georgia") {
      start.choices <- append(start.choices,"Springer Mtn/AT Southern Terminus",after=0) 
    }
    if (input$start.state == "North Carolina/Tennessee") {
      start.choices <- append(start.choices,c("Fontana Dam","Clingmans Dome Tower"),after=16)
      start.choices <- append(start.choices,"Erwin, TN/Nolichucky River",after=39)
      start.choices <- append(start.choices,"Hampton, TN",after=52)
      start.choices[55] <- "Wilbur Dam Rd"
    }
    if (input$start.state == "Virginia") {
      start.choices <- append(start.choices,"Damascus, VA/Water St",after=1)
      start.choices <- append(start.choices,"Massie Gap",after=10)
      start.choices <- start.choices[-52]
      start.choices <- append(start.choices,"Loft Mtn Campground",after=99)
      start.choices <- append(start.choices,"Lewis Mtn Campground",after=106)
      start.choices <- append(start.choices,"Big Meadows Campground",after=109)
      start.choices <- start.choices[-114]
    }
    if (input$start.state == "West Virginia/Maryland") {
      start.choices <- append(start.choices,"Harpers Ferry, WV/ATC Headquarters",after=4)
      start.choices <- start.choices[-11]
    }
    if (input$start.state == "Pennsylvania") {
      start.choices <- start.choices[-5]
      start.choices <- append(start.choices,"Interstate 81",after=30)
      start.choices <- start.choices[-32]
      start.choices <- append(start.choices,"Port Clinton, PA/Schuylkill River",after=48)
    }
    if (input$start.state == "New Jersey") {
      start.choices <- start.choices[-c(13:17,19,22:23)]
      start.choices <- append(start.choices,c("Warwick Turnpike","Longhouse Dr"),after=22)
    }
    if (input$start.state == "New York") {
      start.choices <- append(start.choices,"Orange Turnpike",after=5)
      start.choices <- append(start.choices,c("Seven Lakes Dr","Bear Mtn Tower",
        "Hessian Lake/Bear Mtn Inn"),after=11)
      start.choices <- append(start.choices,"Taconic Parkway",after=26)
      start.choices <- start.choices[-c(28,32,37)]
    }
    if (input$start.state == "Connecticut") {
      start.choices <- append(start.choices,c("Warren Turnpike",
        "Falls Village, CT/Housatonic River"),after=13)
    }
    if (input$start.state == "Massachusetts") {
      start.choices <- start.choices[-c(5,6,9)]
      start.choices <- append(start.choices,"Dalton, MA/Depot St",after=22)
      start.choices <- append(start.choices,"Dalton, MA/High St",after=24)
      start.choices <- start.choices[-c(26,28,29)]
      start.choices <- append(start.choices,"Mt Greylock/Bascom Lodge",after=26)
      start.choices <- append(start.choices,"Williamstown, MA/Phelps Ave",after=29)     
    }
    if (input$start.state == "Vermont") {
      start.choices <- start.choices[-c(2:4,19:20,24,27:32,36,37)]
      start.choices <- append(start.choices,"Norwich, VT/Elm St",after=23)
    }
    if (input$start.state == "New Hampshire") {
      start.choices <- start.choices[-c(10,12,15,19)]
      start.choices <- append(start.choices,"Mt Washington",after=15)
    }
    if (input$start.state == "Maine") {
      start.choices <- start.choices[-c(7,13,15,16,18,19,22,23,26,29)]
      start.choices <- append(start.choices,"Mt Katahdin/AT Northern Terminus",after=19)
    }
    names(start.choices) <- start.choices
    selectInput(inputId="start.location",label="Select Starting Location:",
      choices=start.choices,selected=start.choices[1])
  })
  
  ## Create ending location selection input
  output$end.location <- renderUI({
    if (is.null(input$end.state)) { return() }
    state.choices <- c("Georgia","North Carolina/Tennessee","Virginia",
      "West Virginia/Maryland","Pennsylvania","New Jersey","New York",
      "Connecticut","Massachusetts","Vermont","New Hampshire","Maine")
    index <- match(input$end.state,state.choices)
    border.pts <- c(1,grep("Border",paste(at.points$to.location,at.points$notes)),
      nrow(at.points))[-c(3,6)]
    state.pts <- at.points[border.pts[index]:border.pts[index+1],]
    check.txt <- subset(state.pts,sapply(paste(to.location,notes,sep="/"),function(n)
      grepl(" Rd",n) | grepl("Hwy",n) | grepl("I-",n) | grepl("Border",n) | 
      grepl("BRP",n) | grepl("Skyline",n)))
    end.choices <- mapply(function(p,n) ifelse(n == "",p,paste(p,n,sep="/")),
      p=check.txt$to.location,n=check.txt$notes)
    if (input$end.state == "Georgia") {
      end.choices <- append(end.choices,"Springer Mtn/AT Southern Terminus",after=0)
    }
    if (input$end.state == "North Carolina/Tennessee") {
      end.choices <- append(end.choices,c("Fontana Dam","Clingmans Dome Tower"),after=16)
      end.choices <- append(end.choices,"Erwin, TN/Nolichucky River",after=38)
      end.choices <- append(end.choices,"Hampton, TN",after=51)
      end.choices[54] <- "Wilbur Dam Rd"
    }
    if (input$end.state == "Virginia") {
      end.choices <- append(end.choices,"Damascus, VA/Water St",after=1)
      end.choices <- append(end.choices,"Massie Gap",after=10)
      end.choices <- end.choices[-52]
      end.choices <- append(end.choices,"Loft Mtn Campground",after=99)
      end.choices <- append(end.choices,"Lewis Mtn Campground",after=106)
      end.choices <- append(end.choices,"Big Meadows Campground",after=109)
      end.choices <- end.choices[-114]
    }
    if (input$end.state == "West Virginia/Maryland") {
      end.choices <- append(end.choices,"Harpers Ferry, WV/ATC Headquarters",after=4)
      end.choices <- end.choices[-11]
    }
    if (input$end.state == "Pennsylvania") {
      end.choices <- end.choices[-5]
      end.choices <- append(end.choices,"Interstate 81",after=30)
      end.choices <- end.choices[-32]
      end.choices <- append(end.choices,"Port Clinton, PA/Schuylkill River",after=48)
    }
    if (input$end.state == "New Jersey") {
      end.choices <- end.choices[-c(13:17,19,22:23)]
      end.choices <- append(end.choices,c("Warwick Turnpike","Longhouse Dr"),after=22)
    }
    if (input$end.state == "New York") {
      end.choices <- append(end.choices,"Orange Turnpike",after=5)
      end.choices <- append(end.choices,c("Seven Lakes Dr","Bear Mtn Tower",
        "Hessian Lake/Bear Mtn Inn"),after=11)
      end.choices <- append(end.choices,"Taconic Parkway",after=26)
      end.choices <- end.choices[-c(28,32,37)]
    }
    if (input$end.state == "Connecticut") {
      end.choices <- append(end.choices,c("Warren Turnpike",
        "Falls Village, CT/Housatonic River"),after=13)
    }
    if (input$end.state == "Massachusetts") {
      end.choices <- end.choices[-c(5,6,9)]
      end.choices <- append(end.choices,"Dalton, MA/Depot St",after=22)
      end.choices <- append(end.choices,"Dalton, MA/High St",after=24)
      end.choices <- end.choices[-c(26,28,29)]
      end.choices <- append(end.choices,"Mt Greylock/Bascom Lodge",after=26)
      end.choices <- append(end.choices,"Williamstown, MA/Phelps Ave",after=29)     
    }
    if (input$end.state == "Vermont") {
      end.choices <- end.choices[-c(2:4,19:20,24,27:32,36,37)]
      end.choices <- append(end.choices,"Norwich, VT/Elm St",after=23)
    }
    if (input$end.state == "New Hampshire") {
      end.choices <- end.choices[-c(10,12,15,19)]
      end.choices <- append(end.choices,"Mt Washington",after=15)
    }
    if (input$end.state == "Maine") {
      end.choices <- end.choices[-c(7,13,15,16,18,19,22,23,26,29)]
      end.choices <- append(end.choices,"Mt Katahdin/AT Northern Terminus",after=19)
    }
    names(end.choices) <- end.choices
    selectInput(inputId="end.location",label="Select Ending Location:",
      choices=end.choices,selected=end.choices[length(end.choices)])
  })
  
  ## Function to match input start/end locations to waypoints
  match.pts <- eventReactive(input$go.button,{
    if (is.null(input$start.location)) { return() }
    if (is.null(input$end.location)) { return() }
    match.table <- mapply(function(to.location,notes)
      ifelse(notes == "" | grepl("Camp here",notes) | grepl("Stay here",notes),to.location,
      paste(to.location,notes,sep="/")),at.points$to.location,at.points$notes)
    start.pt <- match(input$start.location,match.table,nomatch=1)
    end.pt <- match(input$end.location,match.table,nomatch=1)
    return(list(start=start.pt,end=end.pt))
  })
  
  ## Function to subset waypoints based on start and end locations
  get.pts <- eventReactive(input$go.button,{
    if (is.null(input$start.location)) { return() }
    if (is.null(input$end.location)) { return() }
    t <- match.pts()
    if (t$start == t$end) { return() }
    if (t$start < t$end) {
      pts <- at.points[t$start:t$end,]
      if (t$start == 1) {
        add <- data.frame(from.location="",to.location="Springer Mtn",dist=NA,total=0,
          from.elev=NA,to.elev=3780,change=NA,grade=NA,water="NO",notes="AT Southern Terminus")
        pts <- rbind(add,pts)
      }
    }
    if (t$start > t$end) {
      temp <- data.frame(
        from.location=c("",rev(at.points$to.location)),
        to.location=c("Mt Katahdin",rev(at.points$from.location)),
        dist=c(NA,-diff(rev(at.points$total)),0.2),
        total=c(0,cumsum(rev(at.points$dist))),
        from.elev=c(NA,rev(at.points$to.elev)),
        to.elev=c(5260,rev(at.points$from.elev)),
        change=c(NA,-rev(at.points$change)),
        grade=c(NA,-rev(at.points$grade)),
        water=c(rev(at.points$water),"NO"),
        notes=c(rev(at.points$notes),"AT Southern Terminus"))
      pts <- temp[(nrow(temp)-t$start):(nrow(temp) - t$end + (t$end == 1)),]
    }
    return(pts)
  })
  
  ## Function to subset profile based on start and end locations
  get.prof <- eventReactive(input$go.button,{
    if (is.null(input$start.location)) { return() }
    if (is.null(input$end.location)) { return() }
    t <- match.pts()
    if (t$start == t$end) { return() }
    if (t$start < t$end) { 
      prof <- subset(profile,dist.mi >= ifelse(t$start == 1,0,at.points$total[t$start]-0.01) & 
        dist.mi <= at.points$total[t$end]+0.01,c("dist.mi","elev.ft"))
    }
    if (t$start > t$end) {
      temp <- data.frame(dist.mi=profile$dist.mi,elev.ft=rev(profile$elev.ft))
      tl <- at.points$total[nrow(at.points)]
      prof <- subset(temp,dist.mi >= tl-at.points$total[t$start]-0.01 &
        dist.mi <= ifelse(t$end == 1,tl,tl-at.points$total[t$end+0.01]),c("dist.mi","elev.ft"))
    }
    return(prof)
  })
  
  ## Display slider bar beneath images
  output$panel.num <- renderUI({
    if (is.null(input$start.location)) { return() }
    if (is.null(input$end.location)) { return() }
    if (is.null(input$mi.per.panel)) { return() }
    t <- match.pts()
    dist.mi <- abs(at.points$total[t$end] - at.points$total[t$start])
    N <- dist.mi %/% input$mi.per.panel + 1
    sliderInput("panel.num",label="Page Number",min=1,max=N,value=1,step=1,width="960px")
  })
  
  ## Display elevation profile plots in the main panel
  output$display.plot <- renderPlot({
    if (is.null(input$start.location)) { return() }
    if (is.null(input$end.location)) { return() }
    if (is.null(input$mile.count)) { return() }
    if (is.null(input$mi.per.panel)) { return() }
    if (is.null(input$panel.num)) { return() }
    pts <- get.pts()
    prof <- get.prof()
    if (is.null(pts)) { return() }
    if (is.null(prof)) { return() }
    if (input$mile.count == "Start from 0") {
      pts$total <- pts$total - pts$total[1]
      prof$dist.mi <- prof$dist.mi - prof$dist.mi[1]
    }
    begin <- pts$total[1] + (input$panel.num - 1)*input$mi.per.panel
    end <- pts$total[1] + input$panel.num*input$mi.per.panel
    ind <- which(prof$dist.mi >= begin & prof$dist.mi <= end)
    pt <- subset(pts,total >= begin & total <= end)
    ymax <- ceiling(max(prof$elev.ft)/500)*500
    xaxs.lab <- seq(ceiling(begin/5),floor(end/5),1)*5
    yaxs.lab <- seq(0,ymax,ifelse(ymax > 3500,1000,500))
    txt.x <- mapply(function(x,y) ifelse(y > ymax/2,x+0.007*input$mi.per.panel,
      x-0.007*input$mi.per.panel),pt$total,pt$to.elev)
    txt.y <- sapply(pt$to.elev,function(x) ifelse(x > ymax/2,x-ymax*0.05,x+ymax*0.05))
    seg.y <- sapply(pt$to.elev,function(x) ifelse(x > ymax/2,x-ymax*0.04,x+ymax*0.04))
    txt.pos <- sapply(pt$to.elev,function(x) ifelse(x > ymax/2,2,4))
    txt.size <- ceiling(300/input$mi.per.panel)
    txt.lab <- mapply(function(p,n) 
      ifelse(grepl("Rd",n) | grepl(" St",n) | grepl("Hwy",n) | grepl("I-",n) | grepl("Trail",n) |
      grepl("Border",n) | grepl("Gap",n) | grepl("Mtn",n) | grepl("Peak",n) | grepl("Pond",n) |
      grepl("Brook",n) | grepl("Creek",n) | grepl("River",n) | grepl("Stream",n) |
      grepl("Campground",n) | grepl("Campsite",n) | grepl("Hostel",n) | grepl("Lodge",n) | 
      grepl("BRP",n) | grepl("Skyline",n) | grepl("Wayside",n) | grepl("End",n) | grepl("AT",n),
      paste(p,n,sep="/"),p),p=pt$to.location,n=pt$notes)
    if (txt.lab[1] == "Springer Mtn/AT Southern Terminus") { txt.x[2] <- txt.x[2] + 0.1 }
    if (txt.lab[length(txt.lab)] == "Springer Mtn/AT Southern Terminus") {
      txt.x[length(txt.x)-1] <- txt.x[length(txt.x)-1] - 0.1 }
    par(mar=c(1,1,0,0),mgp=c(0.75,0,0),tcl=0,ps=txt.size)
    plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlab="",yaxs='i',ylab="",
      xlim=c(begin-0.25,end+0.25),ylim=c(0,ymax))
    if (end > max(prof$dist.mi)) { end <- max(prof$dist.mi) }
    polygon(x=c(end+0.25,begin-0.25,begin-0.25,prof$dist.mi[ind],end+0.25),
      y=c(0,0,prof$elev.ft[ind[1]],prof$elev.ft[ind],prof$elev.ft[ind[length(ind)]]),
      border="white",col="gray80",lwd=1,lend=2,ljoin=1)
    abline(h=seq(0,ymax,500),v=seq(ceiling(begin),floor(end),1),col="white",lwd=1,lend=2)
    lines(prof$dist.mi[ind],prof$elev.ft[ind],lwd=3,lend=2,ljoin=1)
    text(x=txt.x,y=txt.y,labels=txt.lab,pos=txt.pos,srt=90)
    segments(x0=pt$total,y0=pt$to.elev,x1=pt$total,y1=seg.y,lwd=1,lend=2)
    mtext(xaxs.lab,side=1,line=0,at=xaxs.lab,adj=c(0.25,rep(0.5,length(xaxs.lab)-2),0.75))
    mtext(yaxs.lab,side=2,line=0,at=yaxs.lab,adj=c(0,rep(0.5,length(yaxs.lab)-2),1))
    box(lwd=1,lend=2,ljoin=1)
  })
  
  ## Display waypoint data table beneath slider bar
  output$display.table <- DT::renderDataTable({
    if (is.null(input$start.location)) { return() }
    if (is.null(input$end.location)) { return() }
    if (is.null(input$mile.count)) { return() }
    pts <- get.pts()
    if (input$mile.count == "Start from 0") {
      pts$total <- pts$total - pts$total[1]
    }
    loc <- mapply(function(p,n) 
      ifelse(grepl("Rd",n) | grepl(" St",n) | grepl("Hwy",n) | grepl("I-",n) | grepl("Trail",n) |
      grepl("Border",n) | grepl("Gap",n) | grepl("Mtn",n) | grepl("Peak",n) | grepl("Pond",n) |
      grepl("Brook",n) | grepl("Creek",n) | grepl("River",n) | grepl("Stream",n) |
      grepl("Campground",n) | grepl("Campsite",n) | grepl("Hostel",n) | grepl("Lodge",n) | 
      grepl("BRP",n) | grepl("Skyline",n) | grepl("Wayside",n) | grepl("End",n) | grepl("AT",n),
      paste(p,n,sep="/"),p),p=pts$to.location,n=pts$notes)
    table.out <- data.frame(Location=loc,
      Distance=c("",sprintf("%3.1f",pts$dist[2:nrow(pts)])),
      Total=sprintf("%6.1f",pts$total),
      Elevation=sprintf("%4.0f",pts$to.elev),
      Change=c("",sprintf("%5.0f",pts$change[2:nrow(pts)])),
      Grade=c("",sprintf("%4.1f",pts$grade[2:nrow(pts)])),
      Water=pts$water)
    datatable(table.out,
      options=list(autoWidth=TRUE,info=FALSE,paging=FALSE,searching=FALSE,scrollY="600px",
      columnDefs=list(list(orderable=FALSE,targets='_all'),
                      list(className='dt-left',width='300px',targets=0),
                      list(className='dt-center',targets=1:6))),
      class="compact hover row-border stripe",rownames=FALSE,
      colnames=c("Location/Waypoint","Distance (mi)","Total Distance (mi)",
      "Elevation (ft)","Elevation Change (ft)","Grade (%)","Water Source"))
  })
  
  ## Download images and tables from the main screen in PDF format
  output$download.data <- downloadHandler(
    filename=function() { return(paste(gsub(",","",gsub(" ","",input$start.location)),
      "_to_",gsub(",","",gsub(" ","",input$end.location)),".pdf",sep="")) },
    content=function(file) {
      if (is.null(input$start.location)) { return() }
      if (is.null(input$end.location)) { return() }
      if (is.null(input$mile.count)) { return() }
      if (is.null(input$mi.per.panel)) { return() }
      pts <- get.pts()
      prof <- get.prof()
      if (is.null(pts)) { return() }
      if (is.null(prof)) { return() }
      if (input$mile.count == "Start from 0") {
        pts$total <- pts$total - pts$total[1]
        prof$dist.mi <- prof$dist.mi - prof$dist.mi[1]
      }
      N <- (pts$total[nrow(pts)] - pts$total[1]) %/% input$mi.per.panel + 1
      pdf(file,width=7.5,height=10,paper="letter",title="Appalachian Trail")
      par(mfrow=c(2,1))
      for (i in 1:N) {
        begin <- pts$total[1] + (i - 1)*input$mi.per.panel
        end <- pts$total[1] + i*input$mi.per.panel
        ind <- which(prof$dist.mi >= begin & prof$dist.mi <= end)
        pt <- subset(pts,total >= begin & total <= end)
        ymax <- ceiling(max(prof$elev.ft)/500)*500
        xaxs.lab <- seq(ceiling(begin/5),floor(end/5),1)*5
        yaxs.lab <- seq(0,ymax,ifelse(ymax > 3500,1000,500))
        txt.x <- mapply(function(x,y) ifelse(y > ymax/2,x+0.015*input$mi.per.panel,
          x-0.015*input$mi.per.panel),pt$total,pt$to.elev)
        txt.y <- sapply(pt$to.elev,function(x) ifelse(x > ymax/2,x-ymax*0.05,x+ymax*0.05))
        seg.y <- sapply(pt$to.elev,function(x) ifelse(x > ymax/2,x-ymax*0.04,x+ymax*0.04))
        txt.pos <- sapply(pt$to.elev,function(x) ifelse(x > ymax/2,2,4))
        txt.size <- ceiling(200/input$mi.per.panel)
        txt.lab <- mapply(function(p,n) 
          ifelse(grepl("Rd",n) | grepl(" St",n) | grepl("Hwy",n) | grepl("I-",n) | 
          grepl("Trail",n) | grepl("Border",n) | grepl("Gap",n) | grepl("Mtn",n) | 
          grepl("Peak",n) | grepl("Pond",n) | grepl("Brook",n) | grepl("Creek",n) |
          grepl("River",n) | grepl("Stream",n) | grepl("Campground",n) | grepl("Campsite",n) |
          grepl("Hostel",n) | grepl("Lodge",n) |  grepl("BRP",n) | grepl("Skyline",n) |
          grepl("Wayside",n) | grepl("End",n) | grepl("AT",n),paste(p,n,sep="/"),p),
          p=pt$to.location,n=pt$notes)
        if (txt.lab[1] == "Springer Mtn/AT Southern Terminus") { txt.x[2] <- txt.x[2] + 0.1 }
        if (txt.lab[length(txt.lab)] == "Springer Mtn/AT Southern Terminus") {
          txt.x[length(txt.x)-1] <- txt.x[length(txt.x)-1] - 0.1 }
        par(mar=c(0.8,0.8,0.2,0.2),tcl=0,ps=txt.size)
        plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlab="",yaxs='i',ylab="",
          xlim=c(begin-0.25,end+0.25),ylim=c(0,ymax))
        if (end > max(prof$dist.mi)) { end <- max(prof$dist.mi) }
        polygon(x=c(end+0.25,begin-0.25,begin-0.25,prof$dist.mi[ind],end+0.25),
          y=c(0,0,prof$elev.ft[ind[1]],prof$elev.ft[ind],prof$elev.ft[ind[length(ind)]]),
            border="white",col="gray80",lwd=1,lend=2,ljoin=1)
        abline(h=seq(0,ymax,500),v=seq(ceiling(begin),floor(end),1),col="white",lwd=1,lend=2)
        lines(prof$dist.mi[ind],prof$elev.ft[ind],lwd=2,lend=2,ljoin=1)
        text(x=txt.x,y=txt.y,labels=txt.lab,pos=txt.pos,srt=90)
        segments(x0=pt$total,y0=pt$to.elev,x1=pt$total,y1=seg.y,lwd=1,lend=2)
        mtext(xaxs.lab,side=1,line=-0.2,at=xaxs.lab,adj=c(0.25,rep(0.5,length(xaxs.lab)-2),0.75))
        mtext(yaxs.lab,side=2,line=0,at=yaxs.lab,adj=c(0,rep(0.5,length(yaxs.lab)-2),1))
        box(lwd=1,lend=2,ljoin=1)
      }
      if (N %% 2 == 1) {
        plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="")
      }
      loc <- mapply(function(p,n) 
        ifelse(grepl("Rd",n) | grepl(" St",n) | grepl("Hwy",n) | grepl("I-",n) | 
        grepl("Trail",n) | grepl("Border",n) | grepl("Gap",n) | grepl("Mtn",n) | 
        grepl("Peak",n) | grepl("Pond",n) | grepl("Brook",n) | grepl("Creek",n) |
        grepl("River",n) | grepl("Stream",n) | grepl("Campground",n) | grepl("Campsite",n) |
        grepl("Hostel",n) | grepl("Lodge",n) |  grepl("BRP",n) | grepl("Skyline",n) |
        grepl("Wayside",n) | grepl("End",n) | grepl("AT",n),paste(p,n,sep="/"),p),
        p=pts$to.location,n=pts$notes)
      table.out <- data.frame(Location=loc,
        Distance=c("",sprintf("%3.1f",pts$dist[2:nrow(pts)])),
        Total=sprintf("%6.1f",pts$total),
        Elevation=sprintf("%4.0f",pts$to.elev),
        Change=c("",sprintf("%5.0f",pts$change[2:nrow(pts)])),
        Grade=c("",sprintf("%4.1f",pts$grade[2:nrow(pts)])),
        Water=pts$water)
      colnames(table.out) <- c("Location/Waypoint","Distance (mi)","Total (mi)",
        "Elevation (ft)","Change (ft)","Grade (%)","Water?")
      N <- nrow(table.out) %/% 50 + 1
      par(mfrow=c(1,1),mar=c(0,0,0,0),ps=12)
      for (i in 1:N) {
        rows <- c(((i-1)*50+1):pmin(nrow(table.out),i*50))
        pr <- unlist(t(table.out[rows,]))
        txt.x <- sapply(pr,function(x) ifelse(nchar(x) > 6,0.02,0.5))
        adj.x <- sapply(pr,function(x) ifelse(nchar(x) > 6,0,0.5))
        bg <- sapply(1:length(pr),function(x) ifelse(((x-1) %/% 7) %% 2 == 1,"cyan","white"))
        layout(matrix(1:364,nrow=52,ncol=7,byrow=TRUE),widths=c(0.4,rep(0.1,6)),heights=1/52)
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
          text(pr[j],x=txt.x[j],y=0.5,adj=c(adj.x[j],0.5))
        }
      }
      dev.off()
  })
})


