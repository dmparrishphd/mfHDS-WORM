( function () {

    `%|%` <- function ( x , f ) f ( x )

    List <- list ()

    List $ .HEADER.BYTES <- 44L

    List $ .INAMES <- c (
        "STEP" , "PER" , 3 : 8 , "NCOL" , "NROW" , "LAY" )

    List $ .read.header.i <- function(., con) {
        Data <- readBin(con=con, what="integer", n=11L)
        names(Data) <- .$.INAMES
        Data }

    List $ .read.header <- function(., con) {
        Pos <- con %|% seek
        RR <- readBin(con=con, what="raw", n=44L)
        if (!length(RR)) return (
            list({
                II <- rep(NA_integer_, 11)
                names(II) <- .$.INAMES
                II }) )
        seek(con, Pos)
        II <- .$.read.header.i(., con=con)
        list(
            II,
            intToUtf8(RR[16 + 1:16]),
            RR ) }

    List $ .file <- function(.) file(
        description=.$.FILENAME, open="rb")

    List $ .fix.dry <- function (., a) {
        a[a==.$.DRY] <- NA
        a }

    List $ init <- function(., filename, dry=999,
            endian = "little", sizeFloat = 4L, sizeInt = 4L) {
        .$.ENDIANNESS <- endian
        .$.INT.SIZE <- sizeInt
        .$.FLOAT.SIZE <- sizeFloat
        .$.FILENAME <- filename
        con <- .$.file(.)
        .$.DIMS <- .$.read.header.i(., con=con)[c("NCOL", "NROW")]
        close(con)
        .$.DRY <- dry
        .$.NCOL <- .$.DIMS[1]
        .$.NROW <- .$.DIMS[2]
        .$.NCELLS <- prod(.$.DIMS[1:2])
        .$.RECORD.SIZE <- .$.HEADER.BYTES + .$.NCELLS * .$.INT.SIZE
        .$.INDEX <- .$.index(.)
        .$.PERIODS <- .$.INDEX [ , "PER" ] %|% unique %|% sort
        .$.LAYERS  <- .$.INDEX [ , "LAY" ] %|% unique %|% sort 
        . }

    List $ dim <- function(.) .$.DIM

    List $ .seek <- function ( con , ... ) {
        seek ( con = con , origin = "start" , ... )
        con }

    List $ .seek.record <- function(., con, n)
            .$.seek(con=con, where=.$.RECORD.SIZE * (n - 1))

    List $ .seek.data <- function(., con, n)
            .$.seek(
                con=con,
                where=.$.HEADER.BYTES + .$.RECORD.SIZE * (n - 1) )

    List $ periods <- function (.) .$.PERIODS

    List $ layers <- function (.) .$.LAYERS

    List $ .index <- function(.) {
        Data <- list()
        count <- 0L
        con <- .$.file(.)
        NAMES <- c ( "STEP" , "PER" , "LAY" )
        repeat {
            II <- .$.read.header(., .$.seek.record(., con=con, 1L + count))[[1]]
            if (is.na(II[1])) {
                close(con)
                return ( {
                    foo <- t(matrix(data=unlist(Data), nrow=3))
                    colnames(foo) <- NAMES
                    foo })
            } else {
                Data <- c ( Data , unlist ( II[NAMES] ) )
            }
            count <- count + 1L } }

    List $ index <- function (.) .$.INDEX

    List $ nrecords <- function(.) nrow(.$index(.))

    List $ .read.record=function(., con, n) {
        Return <- matrix (
            nrow=.$.NCOL,
            data = readBin(
                con=.$.seek.data(., con=con, n),
                what="double",
                n=.$.NCELLS,
                size=.$.FLOAT.SIZE,
                endian=.$.ENDIANNESS ) )
        close(con)
        Return }

    List $ `[[` <- function(., n)
            .$.fix.dry(., .$.read.record(., .$.file(.), n))

    List $ `[`=function (., step , per, lay)
            .$`[[` ( . , which (
                .$index(.)[, "STEP"] == step &
                .$index(.)[,  "PER"] ==  per &
                .$index(.)[,  "LAY"] ==  lay ) )

    List $ image=function(., n)
            image(
                x=seq(0, .$dim(.)[["NCOL"]]),
                y=seq(from=-.$dim(.)[["NROW"]], to=0),
                z=.$`[[`(., n)[,rev(seq(.$.NROW))],
                useRaster=T)

    List $ transect <- function(., lay, ij) {
        INDEX  <- .$index(.)
        Return <- matrix(
            data = NA_real_ ,
            nrow = nrow ( ij ) ,
            ncol = length ( unique ( INDEX [ , "PER" ] ) ) )
        k <- 1L
        for (n in which(INDEX[,"LAY"] == lay)) {
            Return[,k] <- .$`[[`(., n)[ij]
            k <- k + 1L }
        Return }

    List } ) ()
