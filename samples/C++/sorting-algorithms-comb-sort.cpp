template<class ForwardIterator>
void combsort ( ForwardIterator first, ForwardIterator last )
{
    static const double shrink_factor = 1.247330950103979;
    typedef typename std::iterator_traits<ForwardIterator>::difference_type difference_type;
    difference_type gap = std::distance(first, last);
    bool swaps = true;

    while ( (gap > 1) || (swaps == true) ){
        if (gap > 1)
            gap = static_cast<difference_type>(gap/shrink_factor);

        swaps = false;
        ForwardIterator itLeft(first);
        ForwardIterator itRight(first); std::advance(itRight, gap);

        for ( ; itRight!=last; ++itLeft, ++itRight ){
            if ( (*itRight) < (*itLeft) ){
                std::iter_swap(itLeft, itRight);
                swaps = true;
            }
        }
    }
}
