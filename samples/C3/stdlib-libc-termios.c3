// Source: https://github.com/c3lang/c3c/blob/master/lib/std/libc/termios.c3
// Permalink: https://github.com/c3lang/c3c/blob/2748cf9/lib/std/libc/termios.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module libc::termios @if(env::LIBC &&& env::POSIX);

fn int sendBreak(Fd fd, int duration) => tcsendbreak(fd, duration);
fn int drain(Fd fd) => tcdrain(fd);
fn int flush(Fd fd, int queue_selector) => tcflush(fd, queue_selector);
fn int flow(Fd fd, int action) => tcflow(fd, action);
fn Speed Termios.getOSpeed(Termios* self) => cfgetospeed(self);
fn Speed Termios.getISpeed(Termios* self) => cfgetispeed(self);
fn int Termios.setOSpeed(Termios* self, Speed speed) => cfsetospeed(self, speed);
fn int Termios.setISpeed(Termios* self, Speed speed) => cfsetispeed(self, speed);
fn int Termios.getAttr(Termios* self, Fd fd) => tcgetattr(fd, self);
fn int Termios.setAttr(Termios* self, Fd fd, int optional_actions) => tcsetattr(fd, optional_actions, self);

module libc::termios @if(!env::LIBC ||| !env::POSIX);

distinct Cc = char;
distinct Speed = CUInt;
distinct Tcflags = CUInt;
struct Termios {
        void* dummy;
}

fn CInt tcgetattr(Fd fd, Termios* self)
{
        unreachable("tcgetattr unavailable");
}

fn CInt tcsetattr(Fd fd, CInt optional_actions, Termios* self)
{
        unreachable("tcsetattr unavailable");
}

fn CInt tcsendbreak(Fd fd, CInt duration)
{
        unreachable("tcsendbreak unavailable");
}

fn CInt tcdrain(Fd fd)
{
        unreachable("tcdrain unavailable");
}

fn CInt tcflush(Fd fd, CInt queue_selector)
{
        unreachable("tcflush unavailable");
}

fn CInt tcflow(Fd fd, CInt action)
{
        unreachable("tcflow unavailable");
}

fn Speed cfgetospeed(Termios* self)
{
        unreachable("cfgetospeed unavailable");
}

fn Speed cfgetispeed(Termios* self)
{
        unreachable("cfgetispeed unavailable");
}

fn CInt cfsetospeed(Termios* self, Speed speed)
{
        unreachable("cfsetospeed unavailable");
}

fn CInt cfsetispeed(Termios* self, Speed speed)
{
        unreachable("cfsetispeed unavailable");
}

fn int sendBreak(Fd fd, int duration)
{
        unreachable("sendBreak unavailable");
}

fn int drain(Fd fd)
{
        unreachable("drain unavailable");
}

fn int flush(Fd fd, int queue_selector)
{
        unreachable("flush unavailable");
}

fn int flow(Fd fd, int action)
{
        unreachable("flow unavailable");
}

fn Speed Termios.getOSpeed(Termios* self)
{
        unreachable("Termios.getOSpeed unavailable");
}

fn Speed Termios.getISpeed(Termios* self)
{
        unreachable("Termios.getISpeed unavailable");
}

fn int Termios.setOSpeed(Termios* self, Speed speed)
{
        unreachable("Termios.setOSpeed unavailable");
}

fn int Termios.setISpeed(Termios* self, Speed speed)
{
        unreachable("Termios.setISpeed unavailable");
}

fn int Termios.getAttr(Termios* self, Fd fd)
{
        unreachable("Termios.getAttr unavailable");
}

fn int Termios.setAttr(Termios* self, Fd fd, int optional_actions)
{
        unreachable("Termios.setAttr unavailable");
}

