out, max_out, max_times = 0, -1, []
for job in open('mlijobs.txt'):
    out += 1 if "OUT" in job else -1
    if out > max_out:
        max_out, max_times = out, []
    if out == max_out:
        max_times.append(job.split()[3])

print("Maximum simultaneous license use is %i at the following times:" % max_out)
print('  ' + '\n  '.join(max_times))
