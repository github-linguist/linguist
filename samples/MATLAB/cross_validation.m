function [ error ] = cross_validation(x,y,hyper_parameter)
    
    num_data = size(x,1);

    K = 10;
    indices = crossvalind('Kfold', num_data, K);

    errors = zeros(K,1);
    for i = 1:K
        % get indices
        test_idx = (indices == i);
        train_idx = ~test_idx;

        % get training data
        x_train = x(train_idx,:);
        y_train = y(train_idx,:);

        % train
        w = train(x_train, y_train, hyper_parameter);

        % get test data
        x_test = x(test_idx,:);
        y_test = y(test_idx,:);
        
        % calculate error
        errors(i) = calc_cost(x_test, y_test, w, hyper_parameter); %calc_error
        %errors(i) = calc_error(x_test, y_test, w);
    end
    
    error = mean(errors);
end
