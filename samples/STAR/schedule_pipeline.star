
# version 30001

data_pipeline_general

_rlnPipeLineJobCounter                       6
 

# version 30001

data_pipeline_processes

loop_ 
_rlnPipeLineProcessName #1 
_rlnPipeLineProcessAlias #2 
_rlnPipeLineProcessType #3 
_rlnPipeLineProcessStatus #4 
Schedules/mvf_basic/UpdateMovies/       None            0            1 
Schedules/mvf_basic/StreamMotion/       None            1            1 
Schedules/mvf_basic/StreamCTF/       None            2            1 
Schedules/mvf_basic/OutputProgress/       None           99            1 
 

# version 30001

data_pipeline_nodes

loop_ 
_rlnPipeLineNodeName #1 
_rlnPipeLineNodeType #2 
Schedules/mvf_basic/UpdateMovies/movies.star            0 
Schedules/mvf_basic/StreamMotion/corrected_micrographs.star            1 
Schedules/mvf_basic/StreamMotion/logfile.pdf           13 
Schedules/mvf_basic/StreamCTF/micrographs_ctf.star            1 
Schedules/mvf_basic/StreamCTF/logfile.pdf           13 
 

# version 30001

data_pipeline_input_edges

loop_ 
_rlnPipeLineEdgeFromNode #1 
_rlnPipeLineEdgeProcess #2 
Schedules/mvf_basic/UpdateMovies/movies.star Schedules/mvf_basic/StreamMotion/ 
Schedules/mvf_basic/StreamMotion/corrected_micrographs.star Schedules/mvf_basic/StreamCTF/ 
Schedules/mvf_basic/StreamCTF/micrographs_ctf.star Schedules/mvf_basic/OutputProgress/ 
 

# version 30001

data_pipeline_output_edges

loop_ 
_rlnPipeLineEdgeProcess #1 
_rlnPipeLineEdgeToNode #2 
Schedules/mvf_basic/UpdateMovies/ Schedules/mvf_basic/UpdateMovies/movies.star 
Schedules/mvf_basic/StreamMotion/ Schedules/mvf_basic/StreamMotion/corrected_micrographs.star 
Schedules/mvf_basic/StreamMotion/ Schedules/mvf_basic/StreamMotion/logfile.pdf 
Schedules/mvf_basic/StreamCTF/ Schedules/mvf_basic/StreamCTF/micrographs_ctf.star 
Schedules/mvf_basic/StreamCTF/ Schedules/mvf_basic/StreamCTF/logfile.pdf 
 
