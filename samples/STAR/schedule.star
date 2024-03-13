
# version 30001

data_schedule_general

_rlnScheduleName                       Schedules/mvf_basic/
_rlnScheduleCurrentNodeName            UpdateMovies
 

# version 30001

data_schedule_floats

loop_ 
_rlnScheduleFloatVariableName #1 
_rlnScheduleFloatVariableValue #2 
_rlnScheduleFloatVariableResetValue #3 
mut_movies_count     0.000000     0.000000 
mut_movies_count_shadow     0.000000     0.000000 
set_angpix     0.531500     0.531500 
set_ctf_amp_contrast     0.100000     0.100000 
set_ctf_powerspectrum_window     4.500000     4.500000 
set_frame_dose     1.250000     1.250000 
set_voltage   300.000000   300.000000 
 

# version 30001

data_schedule_bools

loop_ 
_rlnScheduleBooleanVariableName #1 
_rlnScheduleBooleanVariableValue #2 
_rlnScheduleBooleanVariableResetValue #3 
mut_have_more_movies            0            0 
 

# version 30001

data_schedule_strings

loop_ 
_rlnScheduleStringVariableName #1 
_rlnScheduleStringVariableValue #2 
_rlnScheduleStringVariableResetValue #3 
let_movies_starfile Schedules/mvf_basic/UpdateMovies/movies.star Schedules/mvf_basic/UpdateMovies/movies.star 
let_movies_tablename     movies     movies 
set_gain_file_path Micrographs/gain.mrc Micrographs/gain.mrc 
set_movie_import_glob Micrographs/*.tif Micrographs/*.tif 
set_mtf_file_path Micrographs/mtf_k3_300kv.star Micrographs/mtf_k3_300kv.star 
 

# version 30001

data_schedule_operators

loop_ 
_rlnScheduleOperatorName #1 
_rlnScheduleOperatorType #2 
_rlnScheduleOperatorOutput #3 
_rlnScheduleOperatorInput1 #4 
_rlnScheduleOperatorInput2 #5 
mut_have_more_movies=mut_movies_count_GT_mut_movies_count_shadow    bool=gt mut_have_more_movies mut_movies_count mut_movies_count_shadow 
mut_movies_count=COUNT_IMGS_let_movies_starfile_let_movies_tablename float=count_images mut_movies_count let_movies_starfile let_movies_tablename 
mut_movies_count_shadow=SET_mut_movies_count  float=set mut_movies_count_shadow mut_movies_count mut_movies_count_shadow 
 

# version 30001

data_schedule_jobs

loop_ 
_rlnScheduleJobNameOriginal #1 
_rlnScheduleJobName #2 
_rlnScheduleJobMode #3 
_rlnScheduleJobHasStarted #4 
OutputProgress OutputProgress   continue            0 
 StreamCTF  StreamCTF   continue            0 
StreamMotion StreamMotion   continue            0 
UpdateMovies UpdateMovies   continue            0 
 

# version 30001

data_schedule_edges

loop_ 
_rlnScheduleEdgeInputNodeName #1 
_rlnScheduleEdgeOutputNodeName #2 
_rlnScheduleEdgeIsFork #3 
_rlnScheduleEdgeOutputNodeNameIfTrue #4 
_rlnScheduleEdgeBooleanVariable #5 
UpdateMovies mut_movies_count=COUNT_IMGS_let_movies_starfile_let_movies_tablename            0  undefined  undefined 
mut_movies_count=COUNT_IMGS_let_movies_starfile_let_movies_tablename mut_have_more_movies=mut_movies_count_GT_mut_movies_count_shadow            0  undefined  undefined 
mut_have_more_movies=mut_movies_count_GT_mut_movies_count_shadow UpdateMovies            1 StreamMotion mut_have_more_movies 
StreamMotion  StreamCTF            0  undefined  undefined 
 StreamCTF OutputProgress            0  undefined  undefined 
OutputProgress mut_movies_count_shadow=SET_mut_movies_count            0  undefined  undefined 
mut_movies_count_shadow=SET_mut_movies_count UpdateMovies            0  undefined  undefined 
 
