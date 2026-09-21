get println from std::io
get term_enter, term_leave, term_set_title,
term_poll, term_read_key, term_get_size,
term_move, term_print, term_clear from std::term
get arr_range from std::array
get path_join from std::path
get path_exists from std::fs
get exec_fg from std::process
get len from std
get file_created from std::fs
get format_time from std::time
get result_unwrap from std::res

dec arr[string] notes = []
dec arr[string] notes_display = []
dec arr[int] note_created = []
dec int selected = 0
dec int scroll_offset = 0
dec bool searching = false
dec string search_buf = ""

dec bool in_input = false
dec string input_buf = ""
dec string input_title = ""
dec int input_mode = 0

dec bool in_viewer = false
dec int viewer_scroll = 0
dec arr[string] viewer_lines = []
dec string viewer_title = ""
dec bool showing_help = false

!#[init]
fn directory() {
    dec home = std::process::env("HOME")
    dec dir = home.path_join(".rl").path_join("notes")
    if !path_exists(dir) {
        result_unwrap(std::fs::mkdir_all(dir))
    }
    notes = result_unwrap(std::fs::list_dir(dir))
}

!#[init]
fn prepare_notes_for_display() {
    dec ni = 1
    for item in notes {
        dec cr = result_unwrap(file_created(item))
        notes_display = result_unwrap(notes_display.std::array::arr_push(
        std::str::format(
        "[{}] {}",
        ni,
        std::path::path_filename(item)
        )
        ))
        note_created = result_unwrap(note_created.std::array::arr_push(cr))
        ni += 1
    }
}

fn refresh_notes() {
    dec home = std::process::env("HOME")
    dec dir = home.path_join(".rl").path_join("notes")
    notes = result_unwrap(std::fs::list_dir(dir))
    notes_display = []
    note_created = []
    prepare_notes_for_display()
}

fn format_date_short(int ts) -> string {
    dec parts = result_unwrap(std::time::time_parts(ts))
    dec y = std::str::format("{}", parts[0])
    dec m = std::str::pad_left(std::str::format("{}", parts[1]), 2, '0')
    dec d = std::str::pad_left(std::str::format("{}", parts[2]), 2, '0')
    dec h = std::str::pad_left(std::str::format("{}", parts[3]), 2, '0')
    dec mi = std::str::pad_left(std::str::format("{}", parts[4]), 2, '0')
    return std::str::format("{}-{}-{} {}:{}", y, m, d, h, mi)
}

fn fuzzy_match(string query, string name) -> bool {
    dec q = std::str::to_lower(query)
    dec n = std::str::to_lower(name)
    if result_unwrap(len(q)) == 0 {
        return true
    }
    return std::str::contains(n, q)
}

fn get_filtered_notes() -> arr[string] {
    dec arr[string] filtered = []
    if result_unwrap(len(search_buf)) == 0 {
        return notes
    }
    for item in notes {
        if fuzzy_match(search_buf, std::path::path_filename(item)) {
            filtered = result_unwrap(filtered.std::array::arr_push(item))
        }
    }
    return filtered
}

fn render_ui() {
    term_clear()
    dec size = result_unwrap(term_get_size())
    dec bdr = "\e[38;2;65;72;104m"
    dec r = "\e[0m"

    dec top = size[1] - 1
    dec right = size[0]
    dec visible_rows = top - 3

    for i in result_unwrap(arr_range(0, size[0] + 1, 1)) {
        term_move(i, 0)
        term_print(bdr)
        if i == 0 {
            term_print("\u{256d}")
        } else if i == right {
            term_print("\u{256e}")
        } else {
            term_print("\u{2500}")
        }

        term_move(i, top)
        if i == 0 {
            term_print("\u{2570}")
        } else if i == right {
            term_print("\u{256f}")
        } else {
            term_print("\u{2500}")
        }
    }

    for i in result_unwrap(arr_range(1, top, 1)) {
        term_move(0, i)
        term_print(bdr)
        term_print("\u{2502}")
        term_move(right, i)
        term_print("\u{2502}")
    }

    term_print(r)

    dec filtered = get_filtered_notes()
    dec note_count = result_unwrap(len(filtered))
    dec dim = "\e[38;2;115;124;158m"
    dec blue = "\e[1m\e[38;2;122;162;247m"
    dec purple = "\e[1m\e[38;2;187;154;247m"
    dec cyan = "\e[1m\e[38;2;125;207;255m"

    for i in result_unwrap(arr_range(0, visible_rows, 1)) {
        dec idx = i + scroll_offset
        term_move(2, i + 1)
        if idx < note_count {
            dec cr = note_created[idx]
            dec crt_str = format_date_short(cr)
            dec name = std::path::path_filename(filtered[idx])
            dec name_padded = std::str::pad_right(name, 30, ' ')
            if idx == selected {
                term_print("\e[1m\e[38;2;26;27;38m\e[48;2;125;207;255m")
                term_print("[")
                term_print(std::str::pad_right(std::str::format("{}", idx + 1), 6, ' '))
                term_print("] ")
                term_print(name_padded)
                term_print(dim)
                term_print(crt_str)
            } else {
                term_print(dim)
                term_print("[")
                term_print(blue)
                term_print(std::str::pad_right(std::str::format("{}", idx + 1), 6, ' '))
                term_print(dim)
                term_print("] ")
                term_print(purple)
                term_print(name_padded)
                term_print(dim)
                term_print(crt_str)
            }
            term_print(r)
        } else {
            term_print("                                                                  ")
        }
    }

    dec legend_y = top - 2
    term_move(2, legend_y)
    term_print(dim)
    term_print("j/k:move  v:view  e:edit  n:new  r:rename  d:del  f:search  q:quit")
    term_print(r)

    term_move(2, top - 1)
    if searching {
        term_print(cyan)
        term_print("/ ")
        term_print(search_buf)
        term_print("_ ")
    } else {
        term_print(dim)
        term_print("/")
    }
    term_print(r)
}

fn apply_inline(string text, string base) -> string {
    dec bold_o = "\e[1m\e[38;2;192;202;245m"
    dec ital_o = "\e[3m\e[38;2;192;202;245m"
    dec code_o = "\e[38;2;125;207;255m\e[48;2;40;42;58m"
    dec rst = "\e[0m"
    dec out = ""
    dec rem = text
    loop {
        dec slen = result_unwrap(len(rem))
        if slen == 0 {
            break
        }
        dec bp = std::str::index_of(rem, "**")
        dec cp = std::str::index_of(rem, "`")
        dec ip = std::str::index_of(rem, "_")
        dec pos = -1
        dec int kind = 0
        if bp >= 0 {
            pos = bp
            kind = 1
        }
        if cp >= 0 {
            if pos < 0 {
                pos = cp
                kind = 2
            }
            if pos >= 0 and cp < pos {
                pos = cp
                kind = 2
            }
        }
        if ip >= 0 {
            if pos < 0 {
                pos = ip
                kind = 3
            }
            if pos >= 0 and ip < pos {
                pos = ip
                kind = 3
            }
        }
        if pos < 0 {
            out = std::str::concat(out, rem)
            break
        }
        if pos > 0 {
            out = std::str::concat(out, result_unwrap(std::str::slice(rem, 0, pos)))
        }
        if kind == 1 {
            dec int after = pos + 2
            if after >= slen {
                out = std::str::concat(out, result_unwrap(std::str::slice(rem, pos, slen)))
                break
            }
            dec close_s = result_unwrap(std::str::slice(rem, after, slen))
            dec int close = std::str::index_of(close_s, "**")
            if close < 0 {
                out = std::str::concat(out, result_unwrap(std::str::slice(rem, pos, slen)))
                break
            }
            dec inner = result_unwrap(std::str::slice(close_s, 0, close))
            out = std::str::concat(out, bold_o)
            out = std::str::concat(out, inner)
            out = std::str::concat(out, rst)
            out = std::str::concat(out, base)
            dec int close_slen = result_unwrap(len(close_s))
            if close + 2 >= close_slen {
                rem = ""
            } else {
                rem = result_unwrap(std::str::slice(close_s, close + 2, close_slen))
            }
        } else if kind == 3 {
            dec int after = pos + 1
            if after >= slen {
                out = std::str::concat(out, result_unwrap(std::str::slice(rem, pos, slen)))
                break
            }
            dec close_s = result_unwrap(std::str::slice(rem, after, slen))
            dec int close = std::str::index_of(close_s, "_")
            if close < 0 {
                out = std::str::concat(out, result_unwrap(std::str::slice(rem, pos, slen)))
                break
            }
            dec inner = result_unwrap(std::str::slice(close_s, 0, close))
            out = std::str::concat(out, ital_o)
            out = std::str::concat(out, inner)
            out = std::str::concat(out, rst)
            out = std::str::concat(out, base)
            dec int close_slen = result_unwrap(len(close_s))
            if close + 1 >= close_slen {
                rem = ""
            } else {
                rem = result_unwrap(std::str::slice(close_s, close + 1, close_slen))
            }
        } else {
            dec int after = pos + 1
            if after >= slen {
                out = std::str::concat(out, result_unwrap(std::str::slice(rem, pos, slen)))
                break
            }
            dec close_s = result_unwrap(std::str::slice(rem, after, slen))
            dec int close = std::str::index_of(close_s, "`")
            if close < 0 {
                out = std::str::concat(out, result_unwrap(std::str::slice(rem, pos, slen)))
                break
            }
            dec inner = result_unwrap(std::str::slice(close_s, 0, close))
            out = std::str::concat(out, code_o)
            out = std::str::concat(out, inner)
            out = std::str::concat(out, rst)
            out = std::str::concat(out, base)
            dec int close_slen = result_unwrap(len(close_s))
            if close + 1 >= close_slen {
                rem = ""
            } else {
                rem = result_unwrap(std::str::slice(close_s, close + 1, close_slen))
            }
        }
    }
    return out
}

fn render_markdown_line(string line) -> string {
    dec trimmed = std::str::trim(line)
    dec h1 = "\e[1m\e[38;2;122;162;247m"
    dec h2 = "\e[1m\e[38;2;125;207;255m"
    dec h3 = "\e[1m\e[38;2;187;154;247m"
    dec bold = "\e[1m\e[38;2;192;202;245m"
    dec code = "\e[38;2;125;207;255m"
    dec dim = "\e[38;2;115;124;158m"
    dec bullet = "\e[38;2;187;154;247m"
    dec r = "\e[0m"

    if std::str::starts_with(trimmed, "# ") {
        return std::str::format("{}{}{}{}", h1, result_unwrap(std::str::slice(trimmed, 2, result_unwrap(len(trimmed)))), bold, r)
    }
    if std::str::starts_with(trimmed, "## ") {
        return std::str::format("{}{}{}{}", h2, result_unwrap(std::str::slice(trimmed, 3, result_unwrap(len(trimmed)))), bold, r)
    }
    if std::str::starts_with(trimmed, "### ") {
        return std::str::format("{}{}{}{}", h3, result_unwrap(std::str::slice(trimmed, 4, result_unwrap(len(trimmed)))), bold, r)
    }
    if std::str::starts_with(trimmed, "- ") {
        dec content = result_unwrap(std::str::slice(trimmed, 2, result_unwrap(len(trimmed))))
        return std::str::format("{}  {} {}{}", dim, bullet, apply_inline(content, dim), r)
    }
    if std::str::starts_with(trimmed, "* ") {
        dec content = result_unwrap(std::str::slice(trimmed, 2, result_unwrap(len(trimmed))))
        return std::str::format("{}  {} {}{}", dim, bullet, apply_inline(content, dim), r)
    }
    if std::str::starts_with(trimmed, "> ") {
        dec content = result_unwrap(std::str::slice(trimmed, 2, result_unwrap(len(trimmed))))
        return std::str::format("{}  {}{}", dim, apply_inline(content, dim), r)
    }
    if std::str::starts_with(trimmed, "```") {
        return std::str::format("{}{}{}", code, trimmed, r)
    }
    if std::str::starts_with(trimmed, "---") {
        return std::str::format("{}{}{}", dim, std::str::repeat("-", result_unwrap(len(trimmed))), r)
    }
    if std::str::starts_with(trimmed, "***") {
        return std::str::format("{}{}{}", dim, std::str::repeat("-", result_unwrap(len(trimmed))), r)
    }
    dec text_c = "\e[38;2;192;202;245m"
    return apply_inline(line, text_c)
}

dec bool in_code_block = false

fn render_viewer() {
    term_clear()
    dec size = result_unwrap(term_get_size())
    dec bdr = "\e[38;2;125;207;255m"
    dec title_c = "\e[1m\e[38;2;187;154;247m"
    dec dim = "\e[38;2;115;124;158m"
    dec r = "\e[0m"

    dec top = size[1] - 1
    dec right = size[0]
    dec visible_rows = top - 2

    for i in result_unwrap(arr_range(0, size[0] + 1, 1)) {
        term_move(i, 0)
        term_print(bdr)
        if i == 0 {
            term_print("\u{256d}")
        } else if i == right {
            term_print("\u{256e}")
        } else {
            term_print("\u{2500}")
        }

        term_move(i, top)
        if i == 0 {
            term_print("\u{2570}")
        } else if i == right {
            term_print("\u{256f}")
        } else {
            term_print("\u{2500}")
        }
    }

    for i in result_unwrap(arr_range(1, top, 1)) {
        term_move(0, i)
        term_print(bdr)
        term_print("\u{2502}")
        term_move(right, i)
        term_print("\u{2502}")
    }

    term_print(r)

    term_move(2, 0)
    term_print(bdr)
    term_print("\u{251c}")
    term_print("\u{2500}")
    term_print(title_c)
    term_print(viewer_title)
    term_print(r)

    dec line_count = result_unwrap(len(viewer_lines))
    dec code_color = "\e[38;2;125;207;255m"
    dec code_bg = "\e[48;2;40;42;58m"

    in_code_block = false
    for i in result_unwrap(arr_range(0, visible_rows, 1)) {
        dec idx = i + viewer_scroll
        term_move(2, i + 1)
        if idx < line_count {
            dec line = viewer_lines[idx]
            dec trimmed = std::str::trim(line)

            if std::str::starts_with(trimmed, "```") {
                in_code_block = !in_code_block
                term_print(dim)
                term_print(line)
                term_print(r)
            } else if in_code_block {
                term_print(code_color)
                term_print(code_bg)
                term_print(line)
                term_print(r)
            } else {
                term_print(render_markdown_line(line))
            }
        } else {
            term_print("                                                                  ")
        }
    }

    dec legend_y = top - 1
    term_move(2, legend_y)
    term_print(dim)
    term_print("j/k:scroll  g/G:top/bottom  q/Esc:back")
    term_print(r)
}

fn open_viewer() {
    dec filtered = get_filtered_notes()
    dec content = result_unwrap(std::fs::read_file(filtered[selected]))
    viewer_lines = []
    dec string remaining = content
    while result_unwrap(len(remaining)) > 0 {
        dec int nl = std::str::index_of(remaining, "\n")
        if nl >= 0 {
            dec raw = result_unwrap(std::str::slice(remaining, 0, nl))
            dec clean = std::str::replace(raw, "\r", "")
            viewer_lines = result_unwrap(viewer_lines.std::array::arr_push(clean))
            dec int slen = result_unwrap(len(remaining))
            if nl + 1 >= slen {
                remaining = ""
            } else {
                remaining = result_unwrap(std::str::slice(remaining, nl + 1, slen))
            }
        } else {
            dec clean = std::str::replace(remaining, "\r", "")
            viewer_lines = result_unwrap(viewer_lines.std::array::arr_push(clean))
            remaining = ""
        }
    }
    dec int vl = result_unwrap(len(viewer_lines))
    if vl == 0 {
        viewer_lines = result_unwrap(viewer_lines.std::array::arr_push(""))
    }
    viewer_scroll = 0
    viewer_title = std::path::path_filename(filtered[selected])
    in_code_block = false
    in_viewer = true
}

fn render_modal() {
    dec size = result_unwrap(term_get_size())
    dec mid_x = size[0] / 2
    dec mid_y = size[1] / 2

    dec box_w = 40
    dec box_h = 5
    dec box_x = mid_x - box_w / 2
    dec box_y = mid_y - box_h / 2

    dec bdr = "\e[1m\e[38;2;125;207;255m"
    dec title_c = "\e[1m\e[38;2;122;162;247m"
    dec input_c = "\e[38;2;192;202;245m\e[48;2;40;42;58m"
    dec dim = "\e[38;2;115;124;158m"
    dec r = "\e[0m"

    dec top = box_y + box_h
    dec right = box_x + box_w

    for i in result_unwrap(arr_range(box_x, right + 1, 1)) {
        term_move(i, box_y)
        term_print(bdr)
        if i == box_x {
            term_print("\u{256d}")
        } else if i == right {
            term_print("\u{256e}")
        } else {
            term_print("\u{2500}")
        }

        term_move(i, top)
        if i == box_x {
            term_print("\u{2570}")
        } else if i == right {
            term_print("\u{256f}")
        } else {
            term_print("\u{2500}")
        }
    }

    for i in result_unwrap(arr_range(box_y + 1, top, 1)) {
        term_move(box_x, i)
        term_print(bdr)
        term_print("\u{2502}")
        term_move(right, i)
        term_print("\u{2502}")
    }

    term_move(box_x + 2, box_y + 1)
    term_print(title_c)
    term_print(input_title)
    term_print(r)

    term_move(box_x + 2, box_y + 2)
    term_print(dim)
    term_print(">")
    term_print(input_c)
    term_print(input_buf)
    term_print(" ")
    term_print("\e[0m")

    term_move(box_x + 2, box_y + 3)
    term_print(dim)
    if input_mode == 3 {
        term_print("y:delete  Esc:cancel")
    } else {
        term_print("enter:confirm  Esc:cancel")
    }
    term_print(r)
}

fn handle_input_key(arr[string] key) {
    match key[0] {
        "Enter" => {
            if input_mode == 3 {
                dec k = input_buf
                if k == "y" {
                    dec filtered = get_filtered_notes()
                    dec del_path = filtered[selected]
                    result_unwrap(std::fs::delete_file(del_path))
                    refresh_notes()
                    if selected >= result_unwrap(len(notes)) - 1 {
                        selected = result_unwrap(len(notes)) - 1
                        if selected < 0 {
                            selected = 0
                        }
                    }
                    scroll_offset = 0
                }
                if k == "Y" {
                    dec filtered = get_filtered_notes()
                    dec del_path = filtered[selected]
                    result_unwrap(std::fs::delete_file(del_path))
                    refresh_notes()
                    if selected >= result_unwrap(len(notes)) - 1 {
                        selected = result_unwrap(len(notes)) - 1
                        if selected < 0 {
                            selected = 0
                        }
                    }
                    scroll_offset = 0
                }
                in_input = false
                input_buf = ""
                render_ui()
            } else if result_unwrap(len(input_buf)) > 0 {
                dec home = std::process::env("HOME")
                dec dir = home.path_join(".rl").path_join("notes")
                dec filtered = get_filtered_notes()

                if input_mode == 1 {
                    dec new_file = dir.path_join(input_buf)
                    result_unwrap(std::fs::touch(new_file))
                    refresh_notes()
                    selected = result_unwrap(len(notes)) - 1
                    in_input = false
                    input_buf = ""
                    render_ui()
                    term_leave()
                    std::term::term_show_cursor()
                    result_unwrap(exec_fg(std::str::format("{} {}", std::process::env("EDITOR"), new_file)))
                    std::term::term_hide_cursor()
                    term_enter()
                    render_ui()
                } else if input_mode == 2 {
                    dec old_path = filtered[selected]
                    result_unwrap(std::fs::rename_file(old_path, input_buf))
                    refresh_notes()
                    in_input = false
                    input_buf = ""
                    render_ui()
                }
            }
        }
        "Esc" => {
            in_input = false
            input_buf = ""
            render_ui()
        }
        "Backspace" => {
            if result_unwrap(len(input_buf)) > 0 {
                input_buf = result_unwrap(std::str::slice(input_buf, 0, result_unwrap(len(input_buf)) - 1))
                render_ui()
                render_modal()
            }
        }
        _ => {
            dec k = key[0]
            if result_unwrap(len(k)) > 5 {
                dec prefix = result_unwrap(std::str::slice(k, 0, 5))
                if prefix == "Char:" {
                    dec ch = result_unwrap(std::str::slice(k, 5, result_unwrap(len(k))))
                    input_buf = std::str::concat(input_buf, ch)
                    render_ui()
                    render_modal()
                }
            }
        }
    }
}

fn handle_viewer_key(arr[string] key) {
    match key[0] {
        "Char:q" => {
            in_viewer = false
            render_ui()
        }
        "Esc" => {
            in_viewer = false
            render_ui()
        }
        "Char:j" => {
            dec size = result_unwrap(term_get_size())
            dec visible_rows = size[1] - 3
            dec int vl = result_unwrap(len(viewer_lines))
            if viewer_scroll < vl - visible_rows {
                viewer_scroll += 1
            }
            render_viewer()
        }
        "Down" => {
            dec size = result_unwrap(term_get_size())
            dec visible_rows = size[1] - 3
            dec int vl = result_unwrap(len(viewer_lines))
            if viewer_scroll < vl - visible_rows {
                viewer_scroll += 1
            }
            render_viewer()
        }
        "Char:k" => {
            if viewer_scroll > 0 {
                viewer_scroll -= 1
            }
            render_viewer()
        }
        "Up" => {
            if viewer_scroll > 0 {
                viewer_scroll -= 1
            }
            render_viewer()
        }
        "Char:g" => {
            viewer_scroll = 0
            render_viewer()
        }
        "Char:G" => {
            dec size = result_unwrap(term_get_size())
            dec visible_rows = size[1] - 3
            dec int vl = result_unwrap(len(viewer_lines))
            viewer_scroll = vl - visible_rows
            if viewer_scroll < 0 {
                viewer_scroll = 0
            }
            render_viewer()
        }
        _ => {}
    }
}

fn render_help() {
    term_clear()
    dec size = result_unwrap(term_get_size())
    dec bdr = "\e[38;2;65;72;104m"
    dec title_c = "\e[1m\e[38;2;125;207;255m"
    dec key_c = "\e[1m\e[38;2;122;162;247m"
    dec desc = "\e[38;2;192;202;245m"
    dec dim = "\e[38;2;115;124;158m"
    dec r = "\e[0m"

    dec top = size[1] - 1
    dec right = size[0]

    for i in result_unwrap(arr_range(0, size[0] + 1, 1)) {
        term_move(i, 0)
        term_print(bdr)
        if i == 0 {
            term_print("\u{256d}")
        } else if i == right {
            term_print("\u{256e}")
        } else {
            term_print("\u{2500}")
        }

        term_move(i, top)
        if i == 0 {
            term_print("\u{2570}")
        } else if i == right {
            term_print("\u{256f}")
        } else {
            term_print("\u{2500}")
        }
    }

    for i in result_unwrap(arr_range(1, top, 1)) {
        term_move(0, i)
        term_print(bdr)
        term_print("\u{2502}")
        term_move(right, i)
        term_print("\u{2502}")
    }

    term_print(r)

    term_move(2, 0)
    term_print(bdr)
    term_print("\u{251c}")
    term_print("\u{2500}")
    term_print(title_c)
    term_print(" Help ")
    term_print(r)
    term_print(bdr)
    term_print("\u{2500}")

    dec y = 2
    term_move(3, y)
    term_print(key_c)
    term_print("  Navigation")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    j / Down      ")
    term_print(desc)
    term_print("Move down")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    k / Up        ")
    term_print(desc)
    term_print("Move up")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    PgUp          ")
    term_print(desc)
    term_print("Page up")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    PgDown        ")
    term_print(desc)
    term_print("Page down")
    term_print(r)
    y += 2
    term_move(3, y)
    term_print(key_c)
    term_print("  Actions")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    v             ")
    term_print(desc)
    term_print("View note")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    e             ")
    term_print(desc)
    term_print("Edit note")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    n             ")
    term_print(desc)
    term_print("New note")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    r             ")
    term_print(desc)
    term_print("Rename note")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    d             ")
    term_print(desc)
    term_print("Delete note")
    term_print(r)
    y += 2
    term_move(3, y)
    term_print(key_c)
    term_print("  Search")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    f or /        ")
    term_print(desc)
    term_print("Search notes")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    Esc           ")
    term_print(desc)
    term_print("Clear search")
    term_print(r)
    y += 2
    term_move(3, y)
    term_print(key_c)
    term_print("  Viewer")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    j / k         ")
    term_print(desc)
    term_print("Scroll down/up")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    g / G         ")
    term_print(desc)
    term_print("Go to top/bottom")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    q / Esc       ")
    term_print(desc)
    term_print("Close viewer")
    term_print(r)
    y += 2
    term_move(3, y)
    term_print(key_c)
    term_print("  Formatting")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    **bold**      ")
    term_print(desc)
    term_print("Bold text")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    _italic_      ")
    term_print(desc)
    term_print("Italic text")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    `code`        ")
    term_print(desc)
    term_print("Inline code")
    term_print(r)
    y += 2
    term_move(3, y)
    term_print(key_c)
    term_print("  Other")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    h / ?         ")
    term_print(desc)
    term_print("Toggle this help")
    term_print(r)
    y += 1
    term_move(3, y)
    term_print(dim)
    term_print("    Ctrl+C        ")
    term_print(desc)
    term_print("Quit")
    term_print(r)

    dec legend_y = top - 1
    term_move(2, legend_y)
    term_print(dim)
    term_print("Esc or q to close help")
    term_print(r)
}

fn main() {
    std::term::term_hide_cursor()
    term_enter()
    term_set_title("Notes")
    render_ui()

    loop {
        dec key = result_unwrap(term_read_key())

        if key[0] == "Ctrl:c" {
            break
        }

        if showing_help {
            dec bool dismiss = false
            if key[0] == "Esc" {
                dismiss = true
            }
            if key[0] == "Char:q" {
                dismiss = true
            }
            if key[0] == "Char:h" {
                dismiss = true
            }
            if key[0] == "Char:?" {
                dismiss = true
            }
            if dismiss {
                showing_help = false
                render_ui()
            }
            continue
        }

        if in_viewer {
            handle_viewer_key(key)
            continue
        }

        if in_input {
            handle_input_key(key)
            if in_input {
                render_ui()
                render_modal()
            }
            continue
        }

        if searching {
            match key[0] {
                "Esc" => {
                    searching = false
                    search_buf = ""
                    selected = 0
                    scroll_offset = 0
                    render_ui()
                }
                "Enter" => {
                    searching = false
                    selected = 0
                    scroll_offset = 0
                    render_ui()
                }
                "Backspace" => {
                    if result_unwrap(len(search_buf)) > 0 {
                        search_buf = result_unwrap(std::str::slice(search_buf, 0, result_unwrap(len(search_buf)) - 1))
                        selected = 0
                        scroll_offset = 0
                        render_ui()
                    }
                }
                _ => {
                    dec k = key[0]
                    if result_unwrap(len(k)) > 5 {
                        dec prefix = result_unwrap(std::str::slice(k, 0, 5))
                        if prefix == "Char:" {
                            dec ch = result_unwrap(std::str::slice(k, 5, result_unwrap(len(k))))
                            search_buf = std::str::concat(search_buf, ch)
                            selected = 0
                            scroll_offset = 0
                            render_ui()
                        }
                    }
                }
            }
            continue
        }

        dec filtered = get_filtered_notes()
        match key[0] {
            "Char:q" => { break }
            "Char:j" => {
                if selected < result_unwrap(len(filtered)) - 1 {
                    selected += 1
                    dec size = result_unwrap(term_get_size())
                    dec visible_rows = size[1] - 4
                    if selected >= scroll_offset + visible_rows {
                        scroll_offset = selected - visible_rows + 1
                    }
                }
                render_ui()
            }
            "Down" => {
                if selected < result_unwrap(len(filtered)) - 1 {
                    selected += 1
                    dec size = result_unwrap(term_get_size())
                    dec visible_rows = size[1] - 4
                    if selected >= scroll_offset + visible_rows {
                        scroll_offset = selected - visible_rows + 1
                    }
                }
                render_ui()
            }
            "Char:k" => {
                if selected > 0 {
                    selected -= 1
                    if selected < scroll_offset {
                        scroll_offset = selected
                    }
                }
                render_ui()
            }
            "Up" => {
                if selected > 0 {
                    selected -= 1
                    if selected < scroll_offset {
                        scroll_offset = selected
                    }
                }
                render_ui()
            }
            "Char:v" => {
                if result_unwrap(len(filtered)) > 0 {
                    open_viewer()
                    render_viewer()
                }
            }
            "Char:e" => {
                if result_unwrap(len(filtered)) > 0 {
                    in_viewer = false
                    term_leave()
                    std::term::term_show_cursor()
                    result_unwrap(exec_fg(std::str::format("{} {}", std::process::env("EDITOR"), filtered[selected])))
                    std::term::term_hide_cursor()
                    term_enter()
                    while result_unwrap(term_poll(50)) {
                        result_unwrap(term_read_key())
                    }
                    render_ui()
                }
            }
            "Char:n" => {
                in_input = true
                input_mode = 1
                input_title = "New note"
                input_buf = ""
                render_ui()
                render_modal()
            }
            "Char:r" => {
                if result_unwrap(len(filtered)) > 0 {
                    in_input = true
                    input_mode = 2
                    input_title = "Rename note"
                    input_buf = std::path::path_filename(filtered[selected])
                    render_ui()
                    render_modal()
                }
            }
            "Char:d" => {
                if result_unwrap(len(filtered)) > 0 {
                    in_input = true
                    input_mode = 3
                    input_title = std::str::format("Delete '{}'?", std::path::path_filename(filtered[selected]))
                    input_buf = ""
                    render_ui()
                    render_modal()
                }
            }
            "Char:f" => {
                searching = true
                search_buf = ""
                selected = 0
                scroll_offset = 0
                render_ui()
            }
            "Char:/" => {
                searching = true
                search_buf = ""
                selected = 0
                scroll_offset = 0
                render_ui()
            }
            "PageUp" => {
                dec size = result_unwrap(term_get_size())
                dec visible_rows = size[1] - 4
                selected -= visible_rows
                if selected < 0 {
                    selected = 0
                }
                if selected < scroll_offset {
                    scroll_offset = selected
                }
                render_ui()
            }
            "PageDown" => {
                dec size = result_unwrap(term_get_size())
                dec visible_rows = size[1] - 4
                dec max_idx = result_unwrap(len(filtered)) - 1
                selected += visible_rows
                if selected > max_idx {
                    selected = max_idx
                }
                if selected >= scroll_offset + visible_rows {
                    scroll_offset = selected - visible_rows + 1
                }
                render_ui()
            }
            "Char:h" => {
                showing_help = true
                render_help()
            }
            "Char:?" => {
                showing_help = true
                render_help()
            }
            _ => {}
        }
    }

    term_leave()
    std::term::term_show_cursor()
}
