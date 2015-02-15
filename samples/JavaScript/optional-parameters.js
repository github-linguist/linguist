function sorter(table, options) {
    opts = {}
    opts.ordering = options.ordering || 'lexicographic';
    opts.column   = options.column || 0;
    opts.reverse  = options.reverse || false;

    // ...
}

sorter(the_data, {reverse: true, ordering: 'numeric'});
