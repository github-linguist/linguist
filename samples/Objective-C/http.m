#import <Foundation/Foundation.h>

int main (int argc, const char * argv[]) {
    @autoreleasepool {

        NSError        *error;
        NSURLResponse *response;
        NSData *data = [NSURLConnection sendSynchronousRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"http://rosettacode.org"]]
                                                returningResponse:&response
                                                            error:&error];

        NSLog(@"%@", [[NSString alloc] initWithData:data
                                              encoding:NSUTF8StringEncoding]);

    }
    return 0;
}
