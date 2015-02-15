{ tStack is the actual stack type, tStackNode a helper type }
type
  pStackNode = ^tStackNode;
  tStackNode = record
                next: pStackNode;
                data: integer;
               end;
  tStack = record
            top: pStackNode;
           end;

{ Always call InitStack before using a stack }
procedure InitStack(var stack: tStack);
 begin
  stack.top := nil
 end;

{ This function removes all content from a stack; call before disposing, or before a local stack variable goes out of scope }
procedure ClearStack(var stack: tStack);
 var
  node: pStackNode;
 begin
  while stack.top <> nil do
   begin
    node := stack.top;
    stack.top := stack.top^.next;
    dispose(node);
   end
 end;

function StackIsEmpty(stack: tStack):Boolean;
 begin
  StackIsEmpty := stack.top = nil
 end;

procedure PushToStack(var stack: tStack; value: integer);
 var
  node: pStackNode;
 begin
  new(node);
  node^.next := stack.top;
  node^.data := value;
  stack.top := node
 end;

{ may only be called on a non-empty stack! }
function PopFromStack(var stack: tStack): integer;
 var
  node: pStackNode;
 begin
  node := stack.top;
  stack.top := node^.next;
  PopFromStack := node^.data;
  dispose(node);
 end;
