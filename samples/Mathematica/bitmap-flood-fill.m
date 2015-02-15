createMask[img_, pos_, tol_] :=
            RegionBinarize[img, Image[SparseArray[pos -> 1, ImageDimensions[img]]], tol];
floodFill[img_Image, pos_List, tol_Real, color_List] :=
            ImageCompose[
                 SetAlphaChannel[ImageSubtract[img, createMask[img, pos, tol]], 1],
                 SetAlphaChannel[Image[ConstantArray[color, ImageDimensions[img]]],
                                 Dilation[createMask[img, pos, tol],1]
                                ]
                        ]
