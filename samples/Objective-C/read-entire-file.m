    /*** 0. PREPARATION    */
    // We need a text file to read; let's redirect a C string to a new file
    // using the shell by way of the stdlib system() function.
    system ("echo \"Hello, World!\" > ~/HelloRosetta");



    /*** 1. THE TASK      */
    // Instantiate an NSString which describes the filesystem location of
    // the file we will be reading.
    NSString *filePath = [NSHomeDirectory() stringByAppendingPathComponent:@"HelloRosetta"];

    // The selector we're going to use to complete this task,
    // stringWithContentsOfFile:encoding:error, has an optional `error'
    // parameter which can be used to return information about any
    // errors it might run into. It's optional, but we'll create an NSError
    // anyways to demonstrate best practice.
    NSError *anError;

    // And finally, the task: read and store the contents of a file as an
    // NSString.
    NSString *aString = [NSString stringWithContentsOfFile:filePath
                                                  encoding:NSUTF8StringEncoding
                                                     error:&anError];

    // If the file read was unsuccessful, display the error description.
    // Otherwise, display the NSString.
    if (!aString) {
        NSLog(@"%@", [anError localizedDescription]);
    } else {
        NSLog(@"%@", aString);
    }
