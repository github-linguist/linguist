import vapoursynth as vs
import os
import sys

core = vs.core

# When loaded by mpv --vf=vapoursynth:rife.vpy, cwd is the script's directory
# Use absolute path construction from known location
# Try multiple methods to find our directory
script_dir = None

# Method 1: __file__ (works in some VS contexts)
try:
    script_dir = os.path.dirname(os.path.abspath(__file__))
except NameError:
    pass

# Method 2: current working directory (mpv sets cwd when using relative script path)  
if not script_dir:
    script_dir = os.getcwd()

# Method 3: hardcoded fallback
plugin_path = os.path.join(script_dir, "librife.dll")
if not os.path.exists(plugin_path):
    script_dir = r"C:\Users\Kad\Desktop\mpv-RIFE"

core.std.LoadPlugin(os.path.join(script_dir, "librife.dll"))

clip = video_in

# Ensure clip has valid frame rate
if clip.fps.numerator == 0:
    clip = core.std.AssumeFPS(clip, fpsnum=24000, fpsden=1001)

# Detect display refresh rate (Windows)
target_fps = 60
try:
    import ctypes
    dc = ctypes.windll.user32.GetDC(0)
    hz = ctypes.windll.gdi32.GetDeviceCaps(dc, 116)
    ctypes.windll.user32.ReleaseDC(0, dc)
    if hz > 0:
        target_fps = hz
except Exception:
    pass

# Scene detection (needs YUV)
clip = core.misc.SCDetect(clip, threshold=0.1)

# YUV → RGBS
clip = core.resize.Bicubic(clip, format=vs.RGBS, matrix_in_s="709")

# RIFE interpolation
clip = core.rife.RIFE(
    clip,
    model=23,
    fps_num=target_fps,
    fps_den=1,
    model_path=os.path.join(script_dir, "rife-v4.6"),
    gpu_id=0,
    gpu_thread=3,
    sc=True,
    tta=False,
    uhd=False,
)

# RGBS → YUV
clip = core.resize.Bicubic(clip, format=vs.YUV420P8, matrix_s="709")
clip.set_output()
