from cmath import rect, phase
from math import radians, degrees


def mean_angle(deg):
    return degrees(phase(sum(rect(1, radians(d)) for d in deg)/len(deg)))

def mean_time(times):
    t = (time.split(':') for time in times)
    seconds = ((float(s) + int(m) * 60 + int(h) * 3600)
               for h, m, s in t)
    day = 24 * 60 * 60
    to_angles = [s * 360. / day for s in seconds]
    mean_as_angle = mean_angle(to_angles)
    mean_seconds = mean_as_angle * day / 360.
    if mean_seconds < 0:
        mean_seconds += day
    h, m = divmod(mean_seconds, 3600)
    m, s = divmod(m, 60)
    return '%02i:%02i:%02i' % (h, m, s)


if __name__ == '__main__':
    print( mean_time(["23:00:17", "23:40:20", "00:12:45", "00:17:19"]) )
