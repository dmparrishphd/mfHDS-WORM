function ( hds , mi , echo = nullfile () ) {

    colnames(mi) <- c ( "COL" , "ROW" , "LAY" )

    HDS.INDEX <- ( function () {
        X <- as.data.frame ( hds $ index ( hds ) )
        # WON'T LOAD A RECORD IF THERE ARE NO REQUESTED
        # CELLS IN THE CORRESPONDIG LAYER:
        REQUESTED.LAYERS <- sort ( unique ( mi [ , "LAY" ] ) )
        X <- X [ X $ LAY %in% REQUESTED.LAYERS , ]
        X $ TIMESTAMP <- with ( X , paste0 ( PER , ":" , STEP ) )
        X } ) ()

    # PRE-ALLOCATE.
    # ROWS ALIGN WITH TIMESTAMPS.
    # COLUMNS OF TIME SERIES ALIGN WITH ROWS OF mi.
    TimeSeries <- ( function () {
        TIMESTAMPS <- with ( HDS.INDEX , TIMESTAMP [
            ! duplicated ( TIMESTAMP ) ] )
        M <- matrix (
            data = NA_real_ ,
            nrow = length ( TIMESTAMPS ) ,
            ncol = nrow ( mi ) )
        rownames ( M ) <- TIMESTAMPS
        M } ) ()

    for ( i in seq_len ( nrow ( HDS.INDEX ) ) ) {
        Step <- HDS.INDEX [ i ,       "STEP" ]
        Per  <- HDS.INDEX [ i ,        "PER" ]
        Lay  <- HDS.INDEX [ i ,        "LAY" ]
        Timestamp <-
                HDS.INDEX [ i ,  "TIMESTAMP" ]
        cat ( file = echo ,
             "STEP:", Step ,
             "PER:" , Per  ,
             "LAY:" , Lay  , "\n" )
        flush.console()
        Cells <- mi [ , "LAY" ] == Lay
        Grid <- hds $ `[` ( hds , Step , Per , Lay )
        TimeSeries [ Timestamp , Cells ] <- Grid [
            mi [ Cells , 1 : 2 , drop = FALSE ] ] }

    TimeSeries }
