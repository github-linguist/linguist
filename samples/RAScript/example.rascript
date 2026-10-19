// Monster Max
// #ID = 4111

function lives() => byte(0x05D4) + 1
function stage() => byte(0x003A)

stages = { 1: "Downtown", 2: "Sewers" }

rich_presence_conditional_display(is_title_screen(), "Title Screen")
rich_presence_display("Playing Battle {0} in {1}", 
    rich_presence_value("Battle", current_level()),
    rich_presence_lookup("Landscape", current_landscape(), landscapes)
)

achievement(
    title = "Dominated Grass Land", description = "Clear every stage and minigame in World 1", points = 10,
    id = 4217, badge = "04848", published = "12/28/2013 4:49:25 AM", modified = "12/28/2013 5:26:10 AM",
    trigger = bit7(0x007D04) == 1 && bit7(0x007D08) == 1 && bit3(0x007D08) == 1 && bit7(0x007D0A) == 1 && 
              bit5(0x007D0A) == 1 && bit5(0x007D0C) == 1 && bit0(0x007D04) == 1 && bit0(0x007D08) == 1 && 
              bit1(0x007D06) == 1 && 
              byte(0x000727) == 0 && 
              repeated(20, word(0x007DFC) == 10800) && word(0x007DFC) == 3879 && 
              byte(0x000079) == 0 && byte(0x000075) == 0 && 
              never(byte(0x000727) != 0)
)