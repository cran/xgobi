## These should really match the *brushColor[0-9]  `fallback resources' in
##  XGOBISRC/src/xgobitop.h :
## [these are ok for the "Dec. 1999" version of xgobi]:
xgobi.colors.default <-
  c("DeepPink", "OrangeRed1", "DarkOrange", "Gold", "Yellow",
    "DeepSkyBlue1", "SlateBlue1", "YellowGreen",
    "MediumSpringGreen", "MediumOrchid")

if(!exists("Sys.sleep", mode = "function")) {
    warning("\n*** Your R version is outdated.\n*** Consider upgrading!!\n")
    Sys.sleep <- function(time) system(paste("sleep",time))
}

xgobi <-
function(matrx,
	 collab = dimnames(matrx)[[2]],
	 rowlab = dimnames(matrx)[[1]],
	 colors = NULL,
	 glyphs = NULL,
	 erase	= NULL,
	 lines	= NULL,
	 linecolors = NULL,
	 resources  = NULL,
	 title	= deparse(substitute(matrx)),
	 vgroups= NULL,
	 std	= "mmx",
	 nlinkable  = NULL,
	 subset = NULL,
	 display= NULL,
	 keep	= FALSE,
	 fprefix= "xgobi-")
{
    x <- if(is.expression(matrx) || is.character(matrx))
	eval(matrx) else matrx
    if(is.data.frame(x)) x <- data.matrix(x)

    if (any(is.infinite(x[!is.na(x)])))
	stop("Sorry, xgobi can't handle Inf's")

    if (!is.null(title) && !is.character(title))
        stop("title must be a character string")
    dfile <- tempfile(paste(fprefix,
                            abbreviate(gsub("[^A-Za-z0-9]","",title), 5),
                            sep=""))
    write.table(x, file = dfile, quote = FALSE,
		row.names = FALSE, col.names = FALSE)
    if(!keep) on.exit(unlink(dfile), add = TRUE)

    args <- paste("-std", std) ##, "-dev", dev)

    ## Column / Var labels ###
    if (!is.null(collab)) {
	if (!is.vector(collab) || !is.character(collab))# check data type
	    stop("The `collab' argument needs to be a character vector")
	if (!missing(collab) && length(collab) != NCOL(x))
	    stop("`collab' has wrong length (not matching NCOL(x))")
        cat(collab, file = (colfile <- paste(dfile, ".col", sep="")), sep="\n")
        if(!keep) on.exit(unlink(colfile), add = TRUE)
    }
    ## Row / Case labels ###
    if (!is.null(rowlab)) {
	if (!is.vector(rowlab) || !is.character(rowlab))
	    stop("The `rowlab' argument needs to be a character vector")
	if (!missing(rowlab) && length(rowlab) != NROW(x))
	    stop("`rowlab' has wrong length (not matching NROW(x))")
        cat(rowlab, file = (rowfile <- paste(dfile, ".row", sep="")), sep="\n")
        if(!keep) on.exit(unlink(rowfile), add = TRUE)
    }
    ## Variable groups ##
    if (!is.null(vgroups)) {
	   if (!is.vector(vgroups) || !is.numeric(vgroups))
	       stop("The `vgroups' argument needs to be a numeric vector")
	   cat(vgroups, file=(vgfile <- paste(dfile,".vgroups",sep="")), sep="\n")
           if(!keep) on.exit(unlink(vgfile), add = TRUE)
    }
    ## Colors ##
    if (!is.null(colors)) {
	if (!is.vector(colors) || !is.character(colors))
	    stop("The `colors' argument needs to be a character vector")
	cat(colors, file = (clrfile <- paste(dfile,".colors",sep="")), sep="\n")
        if(!keep) on.exit(unlink(clrfile), add = TRUE)
    }
    ## Glyphs ##
    if (!is.null(glyphs)) {
	if (!is.vector(glyphs) || !is.numeric(glyphs))
	    stop("The `glyphs' argument needs to be a numeric vector")
	glyphfile <- paste(dfile, ".glyphs", sep = "")
	cat(glyphs, file = glyphfile, sep = "\n")
        if(!keep) on.exit(unlink(glyphfile), add = TRUE)
    }
    ## Erase ##
    if (!is.null(erase)) {
	if (!is.vector(erase) || !is.numeric(erase))
	    stop("The `erase' argument needs to be a numeric vector")
	erasefile <- paste(dfile, ".erase", sep = "")
	cat(erase, file = erasefile, sep = "\n")
        if(!keep) on.exit(unlink(erasefile), add = TRUE)
    }
    ## Connected lines ##
    if (!is.null(lines)) {
	if (!is.matrix(lines) || !is.numeric(lines) || dim(lines)[2] != 2)
	    stop("The `lines' argument must be a numeric 2-column matrix")
	linesfile <- paste(dfile, ".lines", sep = "")
	unlink(linesfile)# in case it existed
	if (nrow(lines) > 0) {
	    for (i in 1:nrow(lines))
		cat(lines[i, ], "\n", file = linesfile, append = TRUE)
	}
        if(!keep) on.exit(unlink(linesfile), add = TRUE)

	## Line colors ##
	if (!is.null(linecolors)) {
	    if (!is.vector(linecolors) || !is.character(linecolors))
		stop("The `linecolors' argument must be a character vector")
	    linecolorfile <- paste(dfile, ".linecolors", sep = "")
	    cat(linecolors, file = linecolorfile, sep = "\n")

            if(!keep) on.exit(unlink(linecolorfile), add = TRUE)
	}
    }
    ## Resources ##
    if (!is.null(resources)) {
	if (!is.vector(resources) || !is.character(resources))
	    stop("The `resources' argument must be a character vector")
	resourcefile <- paste(dfile, ".resources", sep = "")
	cat(resources, file = resourcefile, sep = "\n")
        if(!keep) on.exit(unlink(resourcefile), add = TRUE)
    }
    ## nlinkable ##
    if (!is.null(nlinkable)) {
	nlinkable <- as.integer(nlinkable)
	if (length(nlinkable) > 1)
	    stop("The `nlinkable' argument must be a scalar integer")
	linkablefile <- paste(dfile, ".nlinkable", sep = "")
	cat(nlinkable, "\n", file = linkablefile)
        if(!keep) on.exit(unlink(linkablefile), add = TRUE)
    }
    ## subset ##
    subsetarg <- ""
    if (!is.null(subset)) {
	subset <- as.integer(subset)
	if (length(subset) > 1)
	    stop("The `subset' argument must be a scalar integer")
	if (subset == 0 || subset > nrow(x))
	    stop("The `subset' argument must be >0 and <= nrows")
	subsetarg <- paste(" -subset ", subset, sep = "")
	args <- paste(args, subsetarg, sep = " ")
    }

    if (!is.null(display)) {
	if (!is.character(display))
	    warning("display must be a character string")
	else args <- paste("-display", display, args)
    }
    args <- paste("-title", paste("'", title, "'", sep = ""), args)

### Note to installer:
### Here you will need to specify the path to the xgobi executable
### on your system (here we assume it *is* in the user's PATH :)

    command <- paste("xgobi", args, dfile, "&")

    cat(command, "\n")
    s <- system(command, FALSE)

    ## Now wait a bit before unlinking all the files via  on.exit(.) :
    if(!keep) Sys.sleep(3)
    invisible(s)
}
