MODULE UseQueue;
IMPORT StdLog,Queue,Boxes;

PROCEDURE Do*;
VAR
        q: Queue.Queue;
        o: Boxes.Object;
    BEGIN
        q := Queue.NewQueue(6);
        q.Push(Boxes.NewInteger(1));
        q.Push(Boxes.NewInteger(2));
        q.Push(Boxes.NewInteger(3));
        o := q.Pop();
        o := q.Pop();
        q.Push(Boxes.NewInteger(4));
        o := q.Pop();
        o := q.Pop();
        q.Push(Boxes.NewInteger(5));
        o := q.Pop();
        StdLog.String("Is empty: ");StdLog.Bool(q.IsEmpty());StdLog.Ln
END Do;
END UseQueue.
