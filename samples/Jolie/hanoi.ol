// https://github.com/jolie/website/blob/master/docs/documentation/locations/code/local.ol

include "runtime.iol"
include "string_utils.iol"

type HanoiRequest: void{
  .src: string
  .aux: string
  .dst: string
  .n: int
  .sid?: string
}

type HanoiReponse: void {
  .move?: string
}

interface LocalOperations{
  RequestResponse:
    hanoiSolver( HanoiRequest )( HanoiReponse )
}

interface ExternalOperations{
  RequestResponse:
    hanoi( HanoiRequest )( string )
}

outputPort Self{
  Interfaces: LocalOperations
}

inputPort Self {
  Location: "local"
  Interfaces: LocalOperations
}

inputPort PowerService {
  Location: "socket://localhost:8000"
  Protocol: http{
    .format = "html"
  }
  Interfaces: ExternalOperations
}

execution { concurrent }

init
{
  getLocalLocation@Runtime()( Self.location )
}

main
{
  [ hanoi( request )( response ){
    getRandomUUID@StringUtils()(request.sid);
    hanoiSolver@Self( request )( subRes );
    response = subRes.move
  }]{ nullProcess }

  [ hanoiSolver( request )( response ){
    if ( request.n > 0 ){
      subReq.n = request.n;
      subReq.n--;
      with( request ){
        subReq.aux = .dst;
        subReq.dst = .aux;
        subReq.src = .src;
        subReq.sid = .sid
      };
      hanoiSolver@Self( subReq )( response );
      response.move +=  "<br>" + 
                ++global.counters.(request.sid) + 
                ") Move from " + request.src +
                " to " + request.dst + ";";
      with ( request ){
        subReq.src = .aux;
        subReq.aux = .src;
        subReq.dst = .dst
      };
      hanoiSolver@Self( subReq )( subRes );
      response.move += subRes.move
    }
  }]{ nullProcess }
}