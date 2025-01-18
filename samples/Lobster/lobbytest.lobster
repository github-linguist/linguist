import color
import imgui
import gl

fatal(gl.window("lobby demo", 1280, 1024))

im.init(false, im.config_docking_enable, 3.0)
assert im.add_font("data/fonts/Droid_Sans/DroidSans.ttf", 20.0)

let LOBBY_MAX_MEMBERS = 4

let steamerr = steam.init(0, true, true)
if steamerr == 0:
    print "Steam init failed"
elif steamerr < 0:
    // Not started from Steam, exit this instance, Steam will launch again.
    // This only happens if there wasn't a steam.appid.txt file.
    print "Relaunching from Steam..."
    return
else:
    print "Welcome Steam user {steam.username()}"

var new_key = ""
var new_value = ""

def lobby_tree(label, lobby_ids, in_lobby):
    im.treenode(label):
        for(lobby_ids) id, idx:
            let num_members = steam.lobby_get_num_members(id)
            im.treenode("#{idx}: {id} ({num_members} members)"):
                if in_lobby:
                    if im.button("Leave##{id}"):
                        steam.lobby_leave(id)
                    im.treenode("Modify:"):
                        new_key = im.input_text("Key", new_key)
                        new_value = im.input_text("Value", new_value)
                        if im.button("Add Data"):
                            steam.lobby_set_data(id, new_key, new_value)
                            new_key = ""
                            new_value = ""
                    im.treenode("Server:"):
                        let server_id = steam.lobby_get_game_server(id)
                        im.text("{server_id}")
                else:
                    if im.button("Join##{id}"):
                        steam.lobby_join(id)
                im.treenode("Data:"):
                    let keys, values = steam.lobby_get_all_data(id)
                    assert keys.length == values.length
                    for(keys.length) i:
                        if in_lobby:
                            if im.button("-##{id}_{i}"):
                                steam.lobby_delete_data(id, keys[i])
                            im.same_line()
                        im.text("\"{keys[i]}\": \"{values[i]}\"")
                if in_lobby:
                    let members = steam.lobby_get_members(id)
                    im.treenode("Members (#{members.length}):"):
                        for(members) m, i:
                            im.text("#{i}: {steam.friend_get_username(m)} ({m})")

enum FilterType:
    FT_Numerical
    FT_String
    FT_ResultCount
let filter_type_names = ["Numeric", "String", "Result Count"]

enum CompareType:
	CT_LTE = -2
	CT_LT = -1
	CT_EQ = 0
	CT_GT = 1
	CT_GTE = 2
	CT_NE = 3
let compare_type_names = ["<=", "<", "==", ">", ">=", "!="]

class Filter:
    type: FilterType
    key: string
    int_value = 0
    string_value = ""
    cmp = CT_EQ

let filters = []::Filter

while gl.frame():
    gl.clear(color_grey)

    im.frame():
        im.window("Lobbies", 0):
            // Create Lobby
            if im.button("Create Lobby"):
                steam.lobby_create(LOBBY_MAX_MEMBERS)
            let joined = steam.lobby_get_all_joined()
            lobby_tree("Joined:", joined, true)
            // Filters
            if im.button("Add Filter"):
                filters.push(Filter { FT_Numerical, "<key>" })
            im.treenode("Filters:"):
                var to_remove = -1
                for(filters) f, i:
                    im.treenode("#{i}"):
                        if im.button("Delete"):
                            to_remove = i
                        f.type = FilterType(im.combo("Type", filter_type_names, f.type))
                        switch f.type:
                            case FT_Numerical:
                                f.key = im.input_text("Key", f.key)
                                f.int_value = im.input_int("Value", f.int_value, -10000, 10000)
                                f.cmp = CompareType(im.combo("Compare", compare_type_names, f.cmp - CT_LTE) + CT_LTE)
                            case FT_String:
                                f.key = im.input_text("Key", f.key)
                                f.string_value = im.input_text("Value", f.string_value)
                                f.cmp = CompareType(im.combo("Compare", compare_type_names, f.cmp - CT_LTE) + CT_LTE)
                            case FT_ResultCount:
                                f.int_value = im.input_int("Count", f.int_value, 1, 50)
                if to_remove >= 0:
                    filters.remove(to_remove)
            // Request Lobbies
            if im.button("Request Lobbies"):
                for(filters) f:
                    switch f.type:
                        case FT_Numerical:
                            steam.lobby_request_add_numerical_filter(f.key, f.int_value, f.cmp)
                        case FT_String:
                            steam.lobby_request_add_string_filter(f.key, f.string_value, f.cmp)
                        case FT_ResultCount:
                            steam.lobby_request_add_result_count_filter(f.int_value)
                steam.lobby_request_list()
            let matched = steam.lobby_request_get_lobbies()
            if im.button("Refresh lobby data"):
                for(matched) id:
                    steam.lobby_request_data(id)
            lobby_tree("Matched:", matched, false)
