xgobi <- function(matrx,
                  collab     = dimnames(matrx)[[2]],
                  rowlab     = dimnames(matrx)[[1]],
                  colors     = NULL,
                  glyphs     = NULL,
                  erase      = NULL,
                  lines      = NULL,
                  linecolors = NULL,
                  resources  = NULL,
                  title      = deparse(substitute(matrx)),
                  vgroups    = NULL,
                  std        = "mmx",
                  nlinkable  = NULL,
                  subset     = NULL,
                  display    = NULL)
{
    ## MM: More fixes.   Let S or R give errors;  use  stop(), not cat() ...
    ##      work for data frames
    x <- if(is.expression(matrx) || is.character(matrx))
        eval(matrx) else matrx
    if(is.data.frame(x)) x <- data.matrix(x)

    if (any(is.infinite(x[!is.na(x)])))
        stop("Sorry, xgobi can't handle Inf's")

    dfile <- tempfile("unix")
    write.table(x, file = dfile, quote = FALSE,
                row.names = FALSE, col.names = FALSE)

    ## <MM> args <- ""
    args <- paste("-std", std)          ##, "-dev", dev)

    ## Column labels ###
    if (!is.vector(collab) || !is.character(collab))
        stop("The 'collab' argument needs to be a character vector")
    if (length(collab) > 0)
        cat(collab, file = paste(dfile, ".col", sep = ""), sep = "\n")

    ## Row labels ##
    if (!is.vector(rowlab) || !is.character(rowlab))
        stop("The 'rowlab' argument needs to be a character vector")
    if (length(rowlab) > 0)
        cat(rowlab, file =  paste(dfile, ".row", sep = ""), sep = "\n")

    ## Variable groups ##
    if (!is.null(vgroups)) {
        if (!is.vector(vgroups) || !is.numeric(vgroups))
            stop("The 'vgroups' argument needs to be a numeric vector")
        cat(vgroups, file = paste(dfile, ".vgroups", sep = ""), sep = "\n")
    }
    ## Colors ##
    if (!is.null(colors)) {
        if (!is.vector(colors) || !is.character(colors))
            stop("The 'colors' argument needs to be a character vector")
        cat(colors, file = paste(dfile, ".colors", sep = ""), sep = "\n")
    }
    ## Glyphs ##
    if (!is.null(glyphs)) {
        if (!is.vector(glyphs) || !is.numeric(glyphs))
            stop("The 'glyphs' argument needs to be a numeric vector")
        glyphfile <- paste(dfile, ".glyphs", sep = "")
        cat(glyphs, file = glyphfile, sep = "\n")
    }
    ## Erase ##
    if (!is.null(erase)) {
        if (!is.vector(erase) || !is.numeric(erase))
            stop("The 'erase' argument needs to be a numeric vector")
        erasefile <- paste(dfile, ".erase", sep = "")
        cat(erase, file = erasefile, sep = "\n")
    }
    ## Connected lines ##
    if (!is.null(lines)) {
        if (!is.matrix(lines) || !is.numeric(lines) || dim(lines)[2] != 2)
            stop("The 'lines' argument must be a numeric 2-column matrix")
        linesfile <- paste(dfile, ".lines", sep = "")
        unix(paste("rm -f", linesfile), output = FALSE)
        if (nrow(lines) > 0) {
            for (i in 1:nrow(lines))
                cat(lines[i, ], "\n", file = linesfile, append = TRUE)
        }
        ## Line colors ##
        if (!is.null(linecolors)) {
            if (!is.vector(linecolors) || !is.character(linecolors))
                stop("The 'linecolors' argument must be a character vector")
            linecolorfile <- paste(dfile, ".linecolors", sep = "")
            cat(linecolors, file = linecolorfile, sep = "\n")
        }
    }
    ## Resources ##
    if (!is.null(resources)) {
        if (!is.vector(resources) || !is.character(resources))
            stop("The 'resources' argument must be a character vector")
        resourcefile <- paste(dfile, ".resources", sep = "")
        cat(resources, file = resourcefile, sep = "\n")
    }
    ## nlinkable ##
    if (!is.null(nlinkable)) {
        nlinkable <- as.integer(nlinkable)
        if (length(nlinkable) > 1)
            stop("The 'nlinkable' argument must be a scalar integer")
        linkablefile <- paste(dfile, ".nlinkable", sep = "")
        cat(nlinkable, "\n", file = linkablefile)
    }
    ## subset ##
    subsetarg <- ""
    if (!is.null(subset)) {
        subset <- as.integer(subset)
        if (length(subset) > 1)
            stop("The 'subset' argument must be a scalar integer")
        if (subset == 0 || subset > nrow(x))
            stop("The 'subset' argument must be >0 and <= nrows")
        subsetarg <- paste(" -subset ", subset, sep = "")
        args <- paste(args, subsetarg, sep = " ")
    }

    if (!is.null(display)) {
        if (!is.character(display))
            warning("display must be a character string")
        else args <- paste("-display", display, args)
    }
    if (!is.null(title)) {
        if (!is.character(title)) {
            warning("title must be a character string")
            title <- deparse(substitute(matrx))
        }
    }
    args <- paste("-title", paste("'", title, "'", sep = ""), args)
###
### Note to installer:
### Here you will need to specify the path to the xgobi executable
### on your system.
    ##<KH>
    ## path <- paste("/usr/dfs/xgobi/src/")

    ## command <- paste(path, "xgobi", sep="")
    ## command <- paste(command, args)
    ## command <- paste(command, nrow(x), ncol(x),
    ##     search()[1], dfile, "&")
    command <- paste("xgobi", args, dfile, "&")
    ##</KH>
    cat(command, "\n")

    ##<KH>
    ## invisible(unix(command,output.to.S=F))
    invisible(system(command, FALSE))
    ##</KH>
}
