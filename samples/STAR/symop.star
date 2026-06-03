# symop.star: Converted from symop.lib on Sun Aug 27 08:39:43 2000

# Each set of symmetry operators is written in its own data block
# The data block tag is the space group number
# The original spacegroup definition line is given as a comment




############################# 1 #############################

data_1

# 1 1 1 P1 PG1 TRICLINIC 'P 1'

_symmetry.Int_Tables_number     1
_symmetry.space_group_name_H-M  P1
_symmetry.cell_setting          TRICLINIC

loop_
_symmetry_equiv.id
_symmetry_equiv.pos_as_xyz
1   X,Y,Z


############################# 2 #############################

data_2

# 2 2 2 P-1 PG1bar TRICLINIC 'P -1'

_symmetry.Int_Tables_number     2
_symmetry.space_group_name_H-M  P-1
_symmetry.cell_setting          TRICLINIC

loop_
_symmetry_equiv.id
_symmetry_equiv.pos_as_xyz
1   X,Y,Z
2   -X,-Y,-Z



############################# 5090row #############################

data_5090row

loop_
_symmetry_equiv.id
_symmetry_equiv.pos_as_xyz
1   X,Y,Z
2   -X,-Y,Z
3   -Y,X,Z
############################# 5090col #############################

data_5090col

# 5090 8 8 P4212 PG422 TETRAGONAL_4axis 'P 4 21 2'

_symmetry.Int_Tables_number     5090
_symmetry.space_group_name_H-M  P4212
_symmetry.cell_setting          TETRAGONAL_4axis

############################# 5090both #############################

data_5090both

# 5090 8 8 P4212 PG422 TETRAGONAL_4axis 'P 4 21 2'

_symmetry.Int_Tables_number     5090
_symmetry.space_group_name_H-M  P4212
_symmetry.cell_setting          TETRAGONAL_4axis

loop_
_symmetry_equiv.id
_symmetry_equiv.pos_as_xyz
1   X,Y,Z
2   -X,-Y,Z
3   -Y,X,Z
4   Y,-X,Z
