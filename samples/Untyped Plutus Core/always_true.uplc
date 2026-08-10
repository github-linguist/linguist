(program
  1.0.0
  [
    (lam
      i_0
      [
        (lam
          i_1
          [
            (lam
              i_2
              [
                (lam
                  i_3
                  (lam
                    i_4
                    [
                      (lam
                        i_5
                        (lam
                          i_6
                          [
                            (lam
                              i_7
                              (lam
                                i_8
                                (force
                                  [
                                    [
                                      [ i_3 (con bool True) ]
                                      (delay (con unit ()))
                                    ]
                                    (delay [ (error ) (force (error )) ])
                                  ]
                                )
                              )
                            )
                            [
                              (lam
                                i_9
                                (force
                                  [
                                    [
                                      [
                                        i_3
                                        [
                                          [
                                            (builtin equalsInteger)
                                            (con integer 0)
                                          ]
                                          [ i_2 i_9 ]
                                        ]
                                      ]
                                      (delay
                                        (force
                                          [
                                            [
                                              [ i_0 [ i_1 i_9 ] ]
                                              (delay (con unit ()))
                                            ]
                                            (delay (error ))
                                          ]
                                        )
                                      )
                                    ]
                                    (delay (error ))
                                  ]
                                )
                              )
                              [ (builtin unConstrData) i_6 ]
                            ]
                          ]
                        )
                      )
                      [
                        (lam
                          i_10
                          (force
                            [
                              [
                                [
                                  i_3
                                  [
                                    [ (builtin equalsInteger) (con integer 0) ]
                                    [ i_2 i_10 ]
                                  ]
                                ]
                                (delay
                                  (force
                                    [
                                      [
                                        [ i_0 [ i_1 i_10 ] ]
                                        (delay (con unit ()))
                                      ]
                                      (delay (error ))
                                    ]
                                  )
                                )
                              ]
                              (delay (error ))
                            ]
                          )
                        )
                        [ (builtin unConstrData) i_4 ]
                      ]
                    ]
                  )
                )
                (force (builtin ifThenElse))
              ]
            )
            (force (force (builtin fstPair)))
          ]
        )
        (force (force (builtin sndPair)))
      ]
    )
    (force (force (builtin chooseList)))
  ]
)