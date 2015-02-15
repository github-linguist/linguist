: root-mean-square ( seq -- mean )
    [ [ sq ] map-sum ] [ length ] bi / sqrt ;
