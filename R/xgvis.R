xgvis <-
function(dmat       = NULL,
         edges      = NULL,
         pos        = NULL,
         rowlab     = dimnames(dmat)[[1]],
         colors     = NULL,
         glyphs     = NULL,
         erase      = NULL,
         lines      = NULL,
         linecolors = NULL,
         resources  = NULL,
         display    = NULL)
{
  if (is.null(edges) && is.null(pos) && is.null(dmat))
    stop("One of dmat, edges, or pos must be present")

  basefile <- tempfile("xgvis")

  ## distance matrix ###
  if (!is.null(dmat)) {
    dmat <- eval(dmat)
    if (any(isinf <- is.infinite(dmat[!is.na(dmat)]))) {
        warning("xgvis can't handle Inf's in dmat; replaced with NA")
        dmat[isinf] <- NA
    }
    dfile <- paste(basefile, ".dist", sep="")
    write(t(dmat), file = dfile, ncolumns = ncol(dmat))
    on.exit(unlink(dfile), add=TRUE)
  }

  ## Edges ###
  if (!is.null(edges)) { # check data type
    if (!is.matrix(edges) || !is.numeric(edges) || dim(edges)[2] != 2)
      stop("The `edges' argument must be a numeric 2-column matrix")

    edgesfile <- paste(basefile, ".edges", sep="")
    if (nrow(edges) > 0) {
	write(t(edges), file = edgesfile, ncol=2)
    }
    on.exit(unlink(edgesfile), add=TRUE)
  }

  ## position matrix ###
  if (!is.null(pos)) {
    pos <- eval(pos)
    if (any(isinf <- is.infinite(pos[!is.na(pos)]))) {
        warning("xgvis can't handle Inf's in pos; replaced with NA")
        pos[isinf] <- NA
    }
    pfile <- paste(basefile, ".pos", sep="")
    write(t(pos), file = pfile, ncolumns = ncol(pos))
    on.exit(unlink(pfile), add = TRUE)
  }

  ## Row / Case labels ###
  if (!is.null(rowlab)) {
      if (!is.vector(rowlab) || !is.character(rowlab))# check data type
          stop("The `rowlab' argument needs to be a character vector")
      if (!missing(rowlab) && length(rowlab) != NROW(dmat))
          stop("`rowlab' has wrong length (not matching NROW(dmat))")
      cat(rowlab, file = (rowfile <- paste(basefile, ".row", sep="")), sep="\n")
      on.exit(unlink(rowfile), add = TRUE)
  }

  ## Colors ###
  if (!is.null(colors)) {
    # check data type
    if (!is.vector(colors) || !is.character(colors))
      stop("The `colors' argument needs to be a character vector")

    colorfile <- paste(basefile, ".colors", sep="")
    write(colors, file = colorfile, ncol=1)
    on.exit(unlink(colorfile), add = TRUE)
  }

  ## Glyphs ###
  if (!is.null(glyphs)) {
    # check data type
    if (!is.vector(glyphs) || !is.numeric(glyphs))
      stop("The `glyphs' argument needs to be a numeric vector")

    glyphfile <- paste(basefile, ".glyphs", sep="")
    write(glyphs, file = glyphfile, ncol=1)
    on.exit(unlink(glyphfile), add = TRUE)
  }

  ## Erase ###
  if (!is.null(erase)) {
    # check data type
    if (!is.vector(erase) || !is.numeric(erase))
      stop("The `erase' argument needs to be a numeric vector")

    erasefile <- paste(basefile, ".erase", sep="")
    write(erase, file = erasefile, ncol=1)
    on.exit(unlink(erasefile), add = TRUE)
  }

  ## Connected lines ###
  if (!is.null(lines)) {
    # check data type
    if (!is.matrix(lines) || !is.numeric(lines) || dim(lines)[2] != 2)
      stop("The `lines' argument must be a numeric 2-column matrix")

    linesfile <- paste(basefile, ".lines", sep="")
    if (nrow(lines) > 0) {
      write(t(lines), file = linesfile, ncol=2)
      on.exit(unlink(linesfile), add = TRUE)
    }
  }

  ## Line colors ###
  if ((!is.null(lines) || !is.null(edges)) && !is.null(linecolors)) {
    # check data type
    if (!is.vector(linecolors) || !is.character(linecolors))
      stop("The `linecolors' argument must be a character vector")

    linecolorfile <- paste(basefile, ".linecolors", sep="")
    write(linecolors, file = linecolorfile, ncol=1)
    on.exit(unlink(linecolorfile), add = TRUE)
  }

  ## Resources ###
  if (!is.null(resources)) {
    # check data type
    if (!is.vector(resources) || !is.character(resources))
      stop("The `resources' argument must be a character vector")

    resourcefile <- paste(basefile, ".resources", sep="")
    write(resources, file = resourcefile, ncol=1)
    on.exit(unlink(resourcefile), add = TRUE)
  }

## DEBUGGING: Keep all the tempfiles by
##> on.exit()

### Note to installer:
### Here you need to specify the path to the xgvis executable / batch file
### on your system.

  command <- paste("xgvis",  basefile, "&")
  cat(command, "\n")
  ## dos:
  ## invisible(dos(command, multi= F, minimized=T, output.to.S=F, translate=T))
  s <- system(command, FALSE)

  ## Now wait a bit before unlinking all the files via  on.exit(.) :
  system("sleep 3")
  invisible(s)
}

