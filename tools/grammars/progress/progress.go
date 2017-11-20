package progress

import (
	"fmt"
	"strings"
	"sync"
	"syscall"
	"unsafe"
)

type winSize struct {
	Ws_row    uint16 // rows, in characters
	Ws_col    uint16 // columns, in characters
	Ws_xpixel uint16 // horizontal size, pixels
	Ws_ypixel uint16 // vertical size, pixels
}

func getWinSize() (*winSize, error) {
	ws := &winSize{}

	_, _, err := syscall.Syscall(
		uintptr(syscall.SYS_IOCTL),
		uintptr(syscall.Stdout),
		uintptr(syscall.TIOCGWINSZ),
		uintptr(unsafe.Pointer(ws)),
	)

	if err != 0 {
		return nil, err
	}

	return ws, nil
}

type ProgressBar struct {
	total    int
	progress int

	mu sync.Mutex
	ws *winSize
}

func NewProgressBar(total int) (*ProgressBar, error) {
	ws, err := getWinSize()
	if err != nil {
		return nil, err
	}

	return &ProgressBar{
		total:    total,
		progress: 0,
		ws:       ws,
	}, nil
}

func (pb *ProgressBar) Update(count int) {
	pb.mu.Lock()
	defer pb.mu.Unlock()
	pb.progress += count

	pc := float32(pb.progress) / float32(pb.total)
	if pc > 1.0 {
		pc = 1.0
	}

	pb.render(pc)
}

func (*ProgressBar) Done() {
	fmt.Printf("\033[2K\033[0G\n")
}

func (pb *ProgressBar) render(percent float32) {
	const remain = 5
	var (
		pg  string
		pgl int
	)

	// 2 = two |
	// 7 = len(" 99.00%")
	pgl = int(pb.ws.Ws_col) - remain - 2 - 7

	count := percent * float32(pgl)
	pg = strings.Repeat("=", int(count))

	if l := pgl - len(pg); l > 0 {
		pg = pg + strings.Repeat(" ", l)
	}

	fmt.Printf("\033[2K\033[0G|%s| %.2f%%", pg, percent*100)
}
