#!/usr/bin/env cwl-runner
# Originally from
# https://github.com/Duke-GCB/GGR-cwl/blob/54e897263a702ff1074c8ac814b4bf7205d140dd/utils/trunk-peak-score.cwl
# Released under the MIT License:
# https://github.com/Duke-GCB/GGR-cwl/blob/54e897263a702ff1074c8ac814b4bf7205d140dd/LICENSE
# Converted to CWL v1.0 syntax using
# https://github.com/common-workflow-language/cwl-upgrader
# and polished by Michael R. Crusoe <mrc@commonwl.org>
# All modifications also released under the MIT License
cwlVersion: v1.0
class: CommandLineTool
doc: Trunk scores in ENCODE bed6+4 files

hints:
  DockerRequirement:
    dockerPull: dukegcb/workflow-utils

inputs:
  peaks:
    type: File
  sep:
    type: string
    default: \t

outputs:
  trunked_scores_peaks:
    type: stdout

baseCommand: awk

arguments:
- -F $(inputs.sep)
- BEGIN{OFS=FS}$5>1000{$5=1000}{print}
- $(inputs.peaks.path)

stdout: $(inputs.peaks.nameroot).trunked_scores$(inputs.peaks.nameext)
