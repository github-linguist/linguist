% This script loads in the data used for the IEEE paper and creates the plots.
clc; close all;

bikes = {'Benchmark', 'Browserins', 'Browser', 'Pista', ...
         'Fisher', 'Yellow', 'Yellowrev'};

data = load_bikes(bikes, 'Steer');

rollData = generate_data('Benchmark', 5.0, ...
                         'input', 'Roll', ...
                         'gains', [1, 55, 3.76, 0.413, 0.076]);

create_ieee_paper_plots(data, rollData)
