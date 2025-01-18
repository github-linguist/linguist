// Adaptation of https://bost.ocks.org/mike/shuffle/compare.html
// Adapted by Joshua T Corbin <jcorbin@wunjo.org>
//
// TODO: would be nice to measure reshuffle time, in particular, adapt better
// to expensive reshuffles that keep blowing the frame time budget; degrade
// gracefully, and scale back the dataset size and/or round count

import color
import gl
import imgui
import std
import vec
import gl

enum shuffle_mode:
    shuffle_none = 0
    shuffle_naive_swap
    shuffle_push_remove
    shuffle_fisher_yates

let mode = shuffle_none

def do_shuffle(array): switch mode:
    case shuffle_none: array
    case shuffle_naive_swap: naive_swap(array)
    case shuffle_push_remove: push_remove(array)
    case shuffle_fisher_yates: fisher_yates(array)

def mode_name(): return switch mode:
    case shuffle_none: "none"
    case shuffle_naive_swap: "naive swap"
    case shuffle_push_remove: "push remove"
    case shuffle_fisher_yates: "fisher yates"

def naive_swap(xs):
    // ala https://blog.codinghorror.com/the-danger-of-naivete/
    for(xs) x, i:
        let j = rnd(xs.length)
        xs[i] = xs[j]
        xs[j] = x

def push_remove(xs):
    for(xs.length) i:
        xs.push(xs.remove(rnd(xs.length - i)))

def fisher_yates(xs):
    for(xs) x, i:
        if i:
            let j = rnd(i + 1)
            xs[i] = xs[j]
            xs[j] = x

let colors = map([
    // from https://github.com/d3/d3-scale-chromatic/blob/master/src/diverging/PuOr.js
    0x2D004B,
    0x542788,
    0x8073AC,
    0xB2ABD2,
    0xD8DAEB,
    0xF7F7F7,
    0xFEE0B6,
    0xFDB863,
    0xE08214,
    0xB35806,
    0x7F3B08,
]): color {
    div( (_ >> 16) & 0xFF, 255),
    div( (_ >> 8)  & 0xFF, 255),
    div(  _        & 0xFF, 255),
    1.0,
}

def spline_value(values, t):
    // uniforem b-spline interpolation adapted from d3-scale-chromactic
    t = clamp(t, 0.0, 1.0)
    let n = values.length - 1
    let i = clamp(floor(t * n), 0, n-1)

    let v1 = values[i]
    let v2 = values[i + 1]
    let v0 = if i > 0:     values[i - 1] else: v1 * 2.0 - v2
    let v3 = if i < n - 1: values[i + 2] else: v2 * 2.0 - v1

    let t1 = (t - div(i, n)) * n
    let t2 = t1 * t1
    let t3 = t2 * t1
    return (
        v0 * (1.0 - 3.0 * t1 + 3.0 * t2 - t3) +
        v1 * (4.0 - 6.0 * t2 + 3.0 * t3) +
        v2 * (1.0 + 3.0 * t1 + 3.0 * t2 - 3.0 * t3) +
        v3 * t3
    ) / 6.0

def spline(t):
    let c = spline_value(colors, t)
    return color {
        clamp(c.red,   0.0, 1.0),
        clamp(c.green, 0.0, 1.0),
        clamp(c.blue,  0.0, 1.0),
        clamp(c.alpha, 0.0, 1.0),
    }

var N = 60
var M = 10000
var matrix = map(N*N): 0
var fills = map(N*N): color_black
var color_domain_lo = log(M / N / 3)
var color_domain_hi = log(M / N * 3)

def reshuffle():
    for(N) i: for(N) j: matrix[N*i + j] = 0
    for M:
        let array = map(N): _
        do_shuffle(array)
        for(array) i, j:
            matrix[N*i + j]++
    for(matrix) n, k:
        let t0 = color_domain_lo
        let t1 = color_domain_hi
        let v = clamp(log(n), t0, t1)
        let t = if t0 == t1: 0.5 else: clamp((v - t0) / (t1 - t0), 0.0, 1.0)
        fills[k] = spline(t)

let shuffling = true

fatal(gl.window("Will It Shuffle?", 640, 480))
check(gl.set_font_name("data/fonts/US101/US101.ttf") and gl.set_font_size(32), "cannot load gl font")

class shuffle_routine:
    last_n = 0
    last_m = 0
    last_mode = mode

    def resume():
        // apply any changes in mode, N, or M
        let changed = N != last_n or M != last_m
        if changed:
            let max_work = 60 * 10000
            if N * M > max_work:
                if N != last_n:
                    M = floor(div(max_work, N) / 100.0) * 100
                else:
                    N = floor(div(max_work, M))
            if N != last_n:
                matrix = map(N*N): 0
                fills = map(N*N): color_black
                last_n = N
            color_domain_lo = log(M / N / 3)
            color_domain_hi = log(M / N * 3)
            last_m = M

        if shuffling or changed or last_mode != mode:
            reshuffle()
            last_mode = mode

let shuffler = shuffle_routine {}

def viz():
    let status = "mode:{mode_name()} rounds:{M}"

    let window_size = gl.window_size()
    let unit = min(floor((float(window_size) / float2 { N + 1, N + 2 })))
    let size = int2 { N + 1, N + 2 } * unit
    gl.set_font_size(unit)

    let status_size = gl.text_size(status)

    for(N) i:
        gl.translate(float2 { unit * (1.0 + i), 0.0 }):
            gl.text("{i}")
        gl.translate(float2 { 0.0, unit * (1.0 + i) }):
            gl.text("{i}")
    gl.scale(unit):
        for(N) i: for(N) j:
            gl.translate(float2 { 1 + i, 1 + j }):
                gl.color(fills[N*i + j]):
                    gl.rect(float2 { 0.95, 0.95 })
    gl.translate(float2 { (size.x - status_size.x) / 2, unit * (N + 1) }):
        gl.text(status)

class ui:
    def init():
        im.init(true)
        let im.fontsize = 32.0
        // check(im.add_font("data/fonts/Droid_Sans/DroidSans.ttf", im.fontsize), "cannot load imgui font")
        check(im.add_font("data/fonts/Inconsolata/Inconsolata-Bold.ttf", im.fontsize), "cannot load imgui font")

    def resume():
        im.frame():
            im.window("ctl", im.window_always_autoresize):
                im.show_engine_stats()
                if not shuffling:
                    if im.button("Reshuffle"):
                        reshuffle()
                im.show_vars()

let uier = ui {}
uier.init()

while gl.frame():
    gl.clear(color_black)
    gl.color(color_white)
    shuffler.resume()
    viz()
    uier.resume()
