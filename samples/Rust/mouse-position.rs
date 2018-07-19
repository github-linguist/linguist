// rustc 0.9 (7613b15 2014-01-08 18:04:43 -0800)

use std::libc::{BOOL, HANDLE, LONG};
use std::ptr::mut_null;

type HWND = HANDLE;

#[deriving(Eq)]
struct POINT {
    x: LONG,
    y: LONG
}

#[link_name = "user32"]
extern "system" {
    fn GetCursorPos(lpPoint:&mut POINT) -> BOOL;
    fn GetForegroundWindow() -> HWND;
    fn ScreenToClient(hWnd:HWND, lpPoint:&mut POINT);
}

fn main() {
    let mut pt = POINT{x:0, y:0};
    loop {
        std::io::timer::sleep(100); // sleep duration in milliseconds

        let pt_prev = pt;
        unsafe { GetCursorPos(&mut pt) };
        if pt != pt_prev {
            let h = unsafe { GetForegroundWindow() };
            if h == mut_null() { continue }

            let mut pt_client = pt;
            unsafe { ScreenToClient(h, &mut pt_client) };
            println!("x: {}, y: {}", pt_client.x, pt_client.y);
        }
    }
}
