helio.plot2 <- function (c, cv = 1, xvlab = c$xlab, yvlab = c$ylab, x.name = "X Variables", 
    y.name = "Y Variables", lab.cex = 1, wid.fact = 0.75, main = "Helio Plot", 
    sub = paste("Canonical Variate", cv, sep = ""), zero.rad = 30, 
    range.rad = 20, name.padding = 5, name.cex = 1.5, axis.circ = c(-1, 
        1), x.group = rep(0, dim(c$xstructcorr)[1]), y.group = rep(0, 
        dim(c$ystructcorr)[1]), type = "correlation") 
{
    plot.new()
    plot.window(c(-100, 100), c(-100, 100))
    if (type == "correlation") {
        xdat <- c$xstructcorr
        ydat <- c$ystructcorr
    }
    else if (type == "variance") {
        xdat <- c$xstructcorrsq
        ydat <- c$ystructcorrsq
    }
    else if (type == "loadings") {
        xdat <- c$xcoef
        ydat <- c$ycoef
    }
    else stop(paste("Plot type ", type, " not supported.\n", 
        sep = ""))
    ir <- zero.rad - range.rad
    mr <- zero.rad
    or <- zero.rad + range.rad
    nr <- zero.rad + range.rad + name.padding
    lines(c(0, 0), c(-90, 90))
    lines(mr * sin(2 * pi * ((0:100)/100)), mr * cos(2 * pi * 
        ((0:100)/100)), lty = 1)
    if (!is.null(axis.circ)) 
        for (i in 1:length(axis.circ)) lines((mr + range.rad * 
            axis.circ[i]) * sin(2 * pi * ((0:100)/100)), (mr + 
            range.rad * axis.circ[i]) * cos(2 * pi * ((0:100)/100)), 
            lty = 3)
    text(-50, 95, label = x.name, cex = name.cex)
    text(50, 95, label = y.name, cex = name.cex)
    nx <- dim(xdat)[1]
    ny <- dim(ydat)[1]
    for (i in 1:nx) {
        if (xdat[i, cv] > 0) 
            bcol <- 1
        else bcol <- NA
        bang <- (-pi/(nx + 1)) * i
        binc <- pi/(max(nx, ny) + 1) * wid.fact/2
        bwinc <- ir * sin(binc)
        bx <- vector()
        bx[1] <- mr * sin(bang) - bwinc * cos(-bang)
        bx[2] <- (mr + range.rad * xdat[i, cv]) * sin(bang) - 
            bwinc * cos(-bang)
        bx[3] <- (mr + range.rad * xdat[i, cv]) * sin(bang) + 
            bwinc * cos(-bang)
        bx[4] <- mr * sin(bang) + bwinc * cos(-bang)
        by <- vector()
        by[1] <- mr * cos(bang) - bwinc * sin(-bang)
        by[2] <- (mr + range.rad * xdat[i, cv]) * cos(bang) - 
            bwinc * sin(-bang)
        by[3] <- (mr + range.rad * xdat[i, cv]) * cos(bang) + 
            bwinc * sin(-bang)
        by[4] <- mr * cos(bang) + bwinc * sin(-bang)
        polygon(bx, by, col = bcol, lty = 1)
        text(nr * sin(bang), nr * cos(bang), label = xvlab[i], 
            srt = (3 * pi/2 - bang) * (360/(2 * pi)), pos = 2, 
            cex = lab.cex)
    }
    for (i in 1:ny) {
        if (ydat[i, cv] > 0) 
            bcol <- 1
        else bcol <- NA
        bang <- (pi/(ny + 1)) * i
        binc <- pi/(max(nx, ny) + 1) * wid.fact/2
        bwinc <- ir * sin(binc)
        bx <- vector()
        bx[1] <- mr * sin(bang) - bwinc * cos(-bang)
        bx[2] <- (mr + range.rad * ydat[i, cv]) * sin(bang) - 
            bwinc * cos(-bang)
        bx[3] <- (mr + range.rad * ydat[i, cv]) * sin(bang) + 
            bwinc * cos(-bang)
        bx[4] <- mr * sin(bang) + bwinc * cos(-bang)
        by <- vector()
        by[1] <- mr * cos(bang) - bwinc * sin(-bang)
        by[2] <- (mr + range.rad * ydat[i, cv]) * cos(bang) - 
            bwinc * sin(-bang)
        by[3] <- (mr + range.rad * ydat[i, cv]) * cos(bang) + 
            bwinc * sin(-bang)
        by[4] <- mr * cos(bang) + bwinc * sin(-bang)
        polygon(bx, by, col = bcol, lty = 1)
        text(nr * sin(bang), nr * cos(bang), label = yvlab[i], 
            srt = (pi/2 - bang) * (360/(2 * pi)), pos = 4, cex = lab.cex)
    }
    if ((!is.null(x.group)) & (max(x.group) > 0)) {
        for (i in unique(x.group)) if (i > 0) {
            gvect <- (x.group %in% i) * (1:length(x.group))
            gvect <- gvect[gvect > 0]
            minang <- min(gvect) * (-pi/(nx + 1))
            maxang <- max(gvect) * (-pi/(nx + 1))
            lines(((or + nr)/2) * sin((((0:100)/100) * (maxang - 
                minang) + minang)), ((or + nr)/2) * cos((((0:100)/100) * 
                (maxang - minang) + minang)), lty = 1)
            lines(c(((or + nr)/2) * sin(minang), nr * sin(minang)), 
                c(((or + nr)/2) * cos(minang), nr * cos(minang)), 
                lty = 1)
            lines(c(((or + nr)/2) * sin(maxang), nr * sin(maxang)), 
                c(((or + nr)/2) * cos(maxang), nr * cos(maxang)), 
                lty = 1)
        }
    }
    if ((!is.null(y.group)) & (max(y.group) > 0)) {
        for (i in unique(y.group)) if (i > 0) {
            gvect <- (y.group %in% i) * (1:length(y.group))
            gvect <- gvect[gvect > 0]
            minang <- min(gvect) * (pi/(ny + 1))
            maxang <- max(gvect) * (pi/(ny + 1))
            lines(((or + nr)/2) * sin((((0:100)/100) * (maxang - 
                minang) + minang)), ((or + nr)/2) * cos((((0:100)/100) * 
                (maxang - minang) + minang)), lty = 1)
            lines(c(((or + nr)/2) * sin(minang), nr * sin(minang)), 
                c(((or + nr)/2) * cos(minang), nr * cos(minang)), 
                lty = 1)
            lines(c(((or + nr)/2) * sin(maxang), nr * sin(maxang)), 
                c(((or + nr)/2) * cos(maxang), nr * cos(maxang)), 
                lty = 1)
        }
    }
    title(main = main, sub = sub)
}