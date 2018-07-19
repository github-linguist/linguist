package main

import (
    "fmt"
    "strings"
)

func main() {
    level := `
#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######`
    fmt.Printf("level:%s\n", level)
    fmt.Printf("solution:\n%s\n", solve(level))
}

func solve(board string) string {
    buffer = make([]byte, len(board))
    width := strings.Index(board[1:], "\n") + 1
    dirs := []struct {
        move, push string
        dPos       int
    }{
        {"u", "U", -width},
        {"r", "R", 1},
        {"d", "D", width},
        {"l", "L", -1},
    }
    visited := map[string]bool{board: true}
    open := []state{state{board, "", strings.Index(board, "@")}}
    for len(open) > 0 {
        s1 := &open[0]
        open = open[1:]
        for _, dir := range dirs {
            var newBoard, newSol string
            newPos := s1.pos + dir.dPos
            switch s1.board[newPos] {
            case '$', '*':
                newBoard = s1.push(dir.dPos)
                if newBoard == "" || visited[newBoard] {
                    continue
                }
                newSol = s1.cSol + dir.push
                if strings.IndexAny(newBoard, ".+") < 0 {
                    return newSol
                }
            case ' ', '.':
                newBoard = s1.move(dir.dPos)
                if visited[newBoard] {
                    continue
                }
                newSol = s1.cSol + dir.move
            default:
                continue
            }
            open = append(open, state{newBoard, newSol, newPos})
            visited[newBoard] = true
        }
    }
    return "No solution"
}

type state struct {
    board string
    cSol  string
    pos   int
}

var buffer []byte

func (s *state) move(dPos int) string {
    copy(buffer, s.board)
    if buffer[s.pos] == '@' {
        buffer[s.pos] = ' '
    } else {
        buffer[s.pos] = '.'
    }
    newPos := s.pos + dPos
    if buffer[newPos] == ' ' {
        buffer[newPos] = '@'
    } else {
        buffer[newPos] = '+'
    }
    return string(buffer)
}

func (s *state) push(dPos int) string {
    newPos := s.pos + dPos
    boxPos := newPos + dPos
    switch s.board[boxPos] {
    case ' ', '.':
    default:
        return ""
    }
    copy(buffer, s.board)
    if buffer[s.pos] == '@' {
        buffer[s.pos] = ' '
    } else {
        buffer[s.pos] = '.'
    }
    if buffer[newPos] == '$' {
        buffer[newPos] = '@'
    } else {
        buffer[newPos] = '+'
    }
    if buffer[boxPos] == ' ' {
        buffer[boxPos] = '$'
    } else {
        buffer[boxPos] = '*'
    }
    return string(buffer)
}
