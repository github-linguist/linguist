List do(
  bubblesort := method(
    t := true
    while( t,
      t := false
      for( j, 0, self size - 2,
        if( self at( j ) start > self at( j+1 ) start,
          self swapIndices( j,j+1 )
          t := true
        )
      )
    )
    return( self )
  )
)
