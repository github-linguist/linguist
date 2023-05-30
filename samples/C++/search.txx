
#ifdef __DEBUG__
#ifndef __DEBUG_PAR__
#define __DEBUG_PAR__
#endif
#endif

/**
@file search.txx
@brief Parallel Search Function
@author Rahul S. Sampath, rahul.sampath@gmail.com
*/

namespace par {
	
	template <typename T>
	int maxLowerBound(const std::vector<T> & keys, const std::vector<T> & searchList,
std::vector<T> & results, MPI_Comm comm) {
		PROF_SEARCH_BEGIN

		int rank, npes;

		MPI_Comm_size(comm, &npes);
		MPI_Comm_rank(comm, &rank);

		// allocate memory for the mins array
		std::vector<T> mins (npes);
		assert(!searchList.empty());

		par::MPI_Allgather<T>( &(*searchList.begin()), &(*mins.begin()), 1, comm);

		//For each key decide which processor to send to
		unsigned int *part = new unsigned int[keys.size()];
		for ( unsigned int i=0; i<keys.size(); i++ ) {
			//maxLB returns the smallest index in a sorted array such that a[ind] <= key and  a[index +1] > key
			bool found = par::maxLowerBound<T>(mins,keys[i], part+i);
			if ( !found ) {
				//This key is smaller than the mins from every processor.
				//No point in searching.
				part[i] = rank;
			}
		}
		mins.clear();

		int *numKeysSend = new int[npes];
		int *numKeysRecv = new int[npes];
		for ( int i=0; i<npes; i++ )
			numKeysSend[i] = 0;
		// calculate the number of keys to send ...
		for ( unsigned int i=0; i<keys.size(); i++ )
			numKeysSend[part[i]]++;

		// Now do an All2All to get numKeysRecv
		par::MPI_Alltoall(numKeysSend, 1, MPI_INT, numKeysRecv, 1, MPI_INT, comm);

		unsigned int totalKeys=0;	// total number of local keys ...
		for ( int i=0; i<npes; i++ )
			totalKeys += numKeysRecv[i];

		// create the send and recv buffers ...
		std::vector<T> sendK (keys.size());
		std::vector<T> recvK (totalKeys);
		// the mapping ..
		unsigned int * comm_map = new unsigned int [keys.size()];

		// Now create sendK
		int *sendOffsets = new int[npes]; sendOffsets[0] = 0;
		int *recvOffsets = new int[npes]; recvOffsets[0] = 0;
		int *numKeysTmp = new int[npes]; numKeysTmp[0] = 0; 

		// compute offsets ...
		for ( int i=1; i<npes; i++ ) {
			sendOffsets[i] = sendOffsets[i-1] + numKeysSend[i-1];
			recvOffsets[i] = recvOffsets[i-1] + numKeysRecv[i-1];
			numKeysTmp[i] = 0; 
		}

		for ( unsigned int i=0; i< keys.size(); i++ ) {
			unsigned int ni = numKeysTmp[part[i]];
			numKeysTmp[part[i]]++;
			// set entry ...
			sendK[sendOffsets[part[i]] + ni] = keys[i];
			// save mapping .. will need it later ...
			comm_map[i] = sendOffsets[part[i]] + ni;
		}
		delete [] part;
		delete [] numKeysTmp;


		par::MPI_Alltoallv_sparse((void *)&(*sendK.begin()), numKeysSend,
                sendOffsets,par::Mpi_datatype<T>::value(), 
                &(*recvK.begin()), numKeysRecv, recvOffsets,
                par::Mpi_datatype<T>::value(), comm);


		std::vector<T>  resSend (totalKeys);
		std::vector<T>  resRecv (keys.size());

		//Final local search.
		for ( unsigned int i=0; i<totalKeys;i++) {
			unsigned int idx;
			bool found = par::maxLowerBound<T>( searchList, recvK[i], &idx );
			if(found) {
				resSend[i] = searchList[idx];
			}
		}//end for i

		//Exchange Results
		//Return what you received in the earlier communication.
		par::MPI_Alltoallv_sparse((void*)&(*resSend.begin()), numKeysRecv,
                recvOffsets,par::Mpi_datatype<T>::value(), 
                &(*resRecv.begin()), numKeysSend, sendOffsets,
                par::Mpi_datatype<T>::value(), comm);

		delete [] sendOffsets;
		delete [] recvOffsets;
		delete [] numKeysSend;
		delete [] numKeysRecv;

		for ( unsigned int i=0; i < keys.size(); i++ ) {
			results[i] = resRecv[comm_map[i]];  
		}//end for

		// Clean up ...
		delete [] comm_map;

		PROF_SEARCH_END
	}


}//end namespace

