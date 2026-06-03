put "Preparing game..."

setscreen ("graphics:max;max,")
put "[LM] Fullscreen"

const pervasive BLOCKSIZE : int := 32

% Splits screen into 64x64 sized grid
class pervasive GridRenderer
    export calculateX, calculateY, drawAt
    
    function calculateX(x : int) : int
        result BLOCKSIZE * x
    end calculateX
    
    function calculateY(y : int) : int
        result BLOCKSIZE * y
    end calculateY
    
    procedure drawAt(picID : int, x : int, y : int)
        var realX := calculateX(x) + 1
        var realY := calculateY(y) + 1
        drawfillbox(realX,realY,realX+BLOCKSIZE,realY+BLOCKSIZE,picID)
    end drawAt
end GridRenderer
put "[LM] GridRenderer"

type pervasive objectType : enum (player, grass, tree, faggot)
class pervasive GameObject
    var objName : string
    var objType : objectType
    procedure drawAt(x : int, y : int)
    end drawAt
end GameObject
put "[LM] GameObject"

class pervasive GameMap
    
    export setup, update
    
    var p1ViewingAreaTopLeftCornerX : int := 0
    var p1ViewingAreaTopLeftCornerY : int := 0
    var p1ViewingAreaBtmRightCornerX : int := 15
    var p1ViewingAreaBtmRightCornerY : int := 15
    % THIS IS NOT HOW THESE VARIABLES ARE SUPPOSED TO BE USED, FIX IT
    var p2ViewingAreaTopLeftCornerX : int := 17
    var p2ViewingAreaTopLeftCornerY : int := 0
    var p2ViewingAreaBtmRightCornerX : int := 32
    var p2ViewingAreaBtmRightCornerY : int := 15
    var masterMap : array 0 .. 999, 0 .. 999 of pointer to GameObject
    var grenderer : pointer to GridRenderer
    
    procedure update
        % Draw player 2
        for x : p1ViewingAreaTopLeftCornerX .. p1ViewingAreaBtmRightCornerX
            for y : p1ViewingAreaTopLeftCornerY .. p1ViewingAreaBtmRightCornerY
                grenderer -> drawAt(x+y, x, y)
            end for
        end for
            % Draw seperator
            for x : p1ViewingAreaBtmRightCornerX .. p2ViewingAreaBtmRightCornerX
            for y : p1ViewingAreaTopLeftCornerY .. p1ViewingAreaBtmRightCornerY
                grenderer -> drawAt(black, x, y)
            end for
        end for
            % Draw player 2
        for x : p2ViewingAreaTopLeftCornerX .. p2ViewingAreaBtmRightCornerX
            for y : p2ViewingAreaTopLeftCornerY.. p2ViewingAreaBtmRightCornerY
                grenderer -> drawAt(x+y, x, y)
            end for
        end for
    end update
    
    procedure setup
        for x : 1..upper(masterMap, 1)
            for y : 1..upper(masterMap, 2)
                %masterMap(x, y)
            end for
        end for
            new GridRenderer, grenderer
    end setup
    
end GameMap
put "[LM] GameMap"

var map : pointer to GameMap
new GameMap, map
map -> setup()
map -> update()
put "[LM] Game"