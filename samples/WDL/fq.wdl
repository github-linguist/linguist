## This example is copied from
## https://github.com/stjudecloud/workflows/blob/833b97fb582fe7d4f6df9d29a645d0289f1fc92e/tools/fq.wdl.
##
## It is available under the MIT license.
##
## [Homepage](https://github.com/stjude-rust-labs/fq)

version 1.1

task fqlint {
    meta {
        description: "Performs quality control on the input FASTQs to ensure proper formatting"
        outputs: {
            check: "A string indicating whether the input FASTQs passed validation"
        }
    }

    parameter_meta {
        read_one_fastq: {
            description: "Input FASTQ with read one. Can be gzipped or uncompressed.",
            stream: true
        }
        read_two_fastq: {
            description: "Input FASTQ with read two. Can be gzipped or uncompressed.",
            stream: true
        }
        disable_validator_codes: {
            description: "Array of codes to disable specific validators",
            choices: {
                S001: "Plus line starts with a '+'",
                S002: "All characters in sequence line are one of 'ACGTN', case-insensitive",
                S003: "Name line starts with an '@'",
                S004: "All four record lines (name, sequence, plus line, and quality) are present",
                S005: "Sequence and quality lengths are the same",
                S006: "All characters in quality line are between '!' and '~' (ordinal values)",
                S007: "All record names are unique",
                P001: "Each paired read name is the same, excluding interleave"
            }
        }
        single_read_validation_level: {
            description: "Only use single read validators up to a given level",
            choices: [
                "low",
                "medium",
                "high"
            ]
        }
        paired_read_validation_level: {
            description: "Only use paired read validators up to a given level",
            choices: [
                "low",
                "medium",
                "high"
            ]
        }
        panic: {
            description: "Panic on first error (true) or log all errors (false)?",
            common: true
        }
        modify_memory_gb: "Add to or subtract from dynamic memory allocation. Default memory is determined by the size of the inputs. Specified in GB."
        modify_disk_size_gb: "Add to or subtract from dynamic disk space allocation. Default disk size is determined by the size of the inputs. Specified in GB."
    }

    input {
        File read_one_fastq
        File? read_two_fastq
        Array[String] disable_validator_codes = []
        String single_read_validation_level = "high"
        String paired_read_validation_level = "high"
        Boolean panic = true
        Int modify_memory_gb = 0
        Int modify_disk_size_gb = 0
    }

    Float read1_size = size(read_one_fastq, "GiB")
    Float read2_size = size(read_two_fastq, "GiB")

    Int memory_gb = (
        ceil((read1_size + read2_size) * 0.25) + 4 + modify_memory_gb
    )

    Int disk_size_gb = ceil((read1_size + read2_size) * 2) + modify_disk_size_gb

    command <<<
        fq lint \
            ~{sep(" ", prefix("--disable-validator ", disable_validator_codes))} \
            --single-read-validation-level ~{single_read_validation_level} \
            --paired-read-validation-level ~{paired_read_validation_level} \
            --lint-mode ~{if panic then "panic" else "log"} \
            ~{read_one_fastq} \
            ~{read_two_fastq}
    >>>

    output {
        String check = "passed"
    }

    runtime {
        memory: "~{memory_gb} GB"
        disks: "~{disk_size_gb} GB"
        container: "quay.io/biocontainers/fq:0.11.0--h9ee0642_0"
        maxRetries: 1
    }
}

task subsample {
    meta {
        description: "Subsamples the input FASTQ(s)"
        outputs: {
            subsampled_read1: "Gzipped FASTQ file containing subsampled read1",
            subsampled_read2: "Gzipped FASTQ file containing subsampled read2"
        }
    }

    parameter_meta {
        read_one_fastq: "Input FASTQ with read one. Can be gzipped or uncompressed."
        read_two_fastq: "Input FASTQ with read two. Can be gzipped or uncompressed."
        prefix: "Prefix for the output FASTQ file(s). The extension `_R1.subsampled.fastq.gz` and `_R2.subsampled.fastq.gz` will be added."
        probability: {
            description: "The probability a record is kept, as a decimal (0.0, 1.0). Cannot be used with `record-count`. Any `probability<=0.0` or `probability>=1.0` to disable.",
            common: true
        }
        record_count: {
            description: "The exact number of records to keep. Cannot be used with `probability`. Any `record_count<=0` to disable.",
            common: true
        }
        modify_disk_size_gb: "Add to or subtract from dynamic disk space allocation. Default disk size is determined by the size of the inputs. Specified in GB."
    }

    input {
        File read_one_fastq
        File? read_two_fastq
        String prefix = sub(
            basename(read_one_fastq),
            "([_\\.][rR][12])?(\\.subsampled)?\\.(fastq|fq)(\\.gz)?$",
            ""
        )
        Float probability = 1.0
        Int record_count = -1
        Int modify_disk_size_gb = 0
    }

    Float read1_size = size(read_one_fastq, "GiB")
    Float read2_size = size(read_two_fastq, "GiB")

    Int disk_size_gb = ceil((read1_size + read2_size) * 2) + modify_disk_size_gb

    String probability_arg = if (probability < 1.0 && probability > 0)
        then "-p ~{probability}"
        else ""
    String record_count_arg = if (record_count > 0) then "-n ~{record_count}" else ""

    String r1_dst = prefix + "_R1.subsampled.fastq.gz"
    String r2_dst = prefix + "_R2.subsampled.fastq.gz"

    command <<<
        fq subsample \
            ~{probability_arg} \
            ~{record_count_arg} \
            --r1-dst ~{r1_dst} \
            ~{if defined(read_two_fastq)
                then "--r2-dst " + r2_dst
                else ""
            } \
            ~{read_one_fastq} \
            ~{read_two_fastq}
    >>>

    output {
        File subsampled_read1 = prefix + "_R1.subsampled.fastq.gz"
        File? subsampled_read2 = prefix + "_R2.subsampled.fastq.gz"
    }

    runtime {
        memory: "4 GB"
        disks: "~{disk_size_gb} GB"
        container: "quay.io/biocontainers/fq:0.11.0--h9ee0642_0"
        maxRetries: 1
    }
}
