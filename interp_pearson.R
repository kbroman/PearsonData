# R function interp_pearson
#   - to read in a table from Pearson and Lee (1903)
#   - interpolate to get pairs of values
#   - randomize values somewhat
#   - write to a file (if 'output' is non-null)
#
# Note: The counts of pairs for the resulting file will differ somewhat
#       from the original table


interp_pearson <-
    function(input, output=NULL, col.names=NULL, digits=1)
{
    x <- as.matrix(read.table(input, check.names=FALSE))

    y <- x
    for(i in 1:nrow(x)) {
        for(j in 1:ncol(x)) {
            y[i,j] <- (x[i,j] %/% 1) + as.numeric(runif(1) < x[i,j] %% 1)
        }
    }

    z <- NULL
    co <- as.numeric(colnames(y))
    ro <- as.numeric(rownames(y))
    for(i in 1:nrow(y)) {
        for(j in 1:ncol(y)) {
            if(y[i,j] > 0) {
                zz <- matrix(rep(c(co[j]+0.5,ro[i]+0.5),y[i,j]),byrow=TRUE,ncol=2)
                if(is.null(z)) z <- zz
                else z <- rbind(z,zz)
            }
        }
    }

    if(!is.null(col.names))
        dimnames(z) <- list(NULL, col.names)

    z <- z+runif(length(z),0,1)

    if(!is.null(digits))
        z <- round(z, digits)

    if(!is.null(output)) {
        comments <- readLines(input)
        comments <- grep("^#", z, value=TRUE) # just save the comments

        cat(comments, file=output)

        write.table(z, output, row.names=FALSE, col.names=TRUE,
                quote=FALSE, sep=",", append=TRUE)
        return(invisible(z))
    }

    z
}
