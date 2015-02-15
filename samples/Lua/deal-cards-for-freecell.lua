deck = {}
rank = {"A", "2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K"}
suit = {"C", "D", "H", "S"}
two31, state = bit32.lshift(1, 31), 0

function rng()
    state = (214013 * state + 2531011) % two31
    return bit32.rshift(state, 16)
end

function initdeck()
    for i, r in ipairs(rank) do
        for j, s in ipairs(suit) do
            table.insert(deck, r .. s)
        end
    end
end

function deal(num)
    initdeck()
    state = num
    print("Game #" .. num)
    repeat
        choice = rng(num) % #deck + 1
        deck[choice], deck[#deck] = deck[#deck], deck[choice]
        io.write(" " .. deck[#deck])
        if (#deck % 8 == 5) then
            print()
        end
        deck[#deck] = nil
    until #deck == 0
    print()
end

deal(1)
deal(617)
