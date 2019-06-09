import ballerina.lang.messages;
import ballerina.net.http;
import ballerina.doc;

@doc:Description {value:"By default Ballerina assumes that the service is to be exposed via HTTP/1.1 using the system default port and that all requests coming to the HTTP server will be delivered to this service."}
service<http> helloWorld {
    @doc:Description {value:"All resources are invoked with an argument of type message, the built-in reference type representing a network invocation."}
    resource sayHello (message m) {
        // Creates an empty message.
        message response = {};
        // A util method that can be used to set string payload.
        messages:setStringPayload(response, "Hello, World!");
        // Reply keyword sends the response back to the client.
        reply response;
    }
}
