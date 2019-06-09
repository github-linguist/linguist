#!/usr/bin/env nextflow
/*
 * This is free and unencumbered software released into the public domain.
 * 
 * Anyone is free to copy, modify, publish, use, compile, sell, or
 * distribute this software, either in source code form or as a compiled
 * binary, for any purpose, commercial or non-commercial, and by any
 * means.
 * 
 * In jurisdictions that recognize copyright laws, the author or authors
 * of this software dedicate any and all copyright interest in the
 * software to the public domain. We make this dedication for the benefit
 * of the public at large and to the detriment of our heirs and
 * successors. We intend this dedication to be an overt act of
 * relinquishment in perpetuity of all present and future rights to this
 * software under copyright law.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 * 
 * For more information, please refer to <http://unlicense.org/>
 */
 
  
/* 
 * Proof of concept of a RNAseq pipeline implemented with Nextflow
 * 
 * Authors:
 * - Paolo Di Tommaso <paolo.ditommaso@gmail.com>
 * - Emilio Palumbo <emiliopalumbo@gmail.com> 
 * - Evan Floden <evanfloden@gmail.com> 
 */ 

 
params.reads = "$baseDir/data/ggal/*_{1,2}.fq"
params.transcriptome = "$baseDir/data/ggal/ggal_1_48850000_49020000.Ggal71.500bpflank.fa"
params.outdir = "."
params.multiqc = "$baseDir/multiqc"

log.info """\
         R N A S E Q - N F   P I P E L I N E    
         ===================================
         transcriptome: ${params.transcriptome}
         reads        : ${params.reads}
         outdir       : ${params.outdir}
         """
         .stripIndent()


transcriptome_file = file(params.transcriptome)
multiqc_file = file(params.multiqc)
 

Channel
    .fromFilePairs( params.reads )
    .ifEmpty { error "Cannot find any reads matching: ${params.reads}" }
    .into { read_pairs_ch; read_pairs2_ch } 
 

process index {
    tag "$transcriptome_file.simpleName"
    
    input:
    file transcriptome from transcriptome_file
     
    output:
    file 'index' into index_ch

    script:       
    """
    salmon index --threads $task.cpus -t $transcriptome -i index
    """
}
 
 
process quant {
    tag "$pair_id"
     
    input:
    file index from index_ch
    set pair_id, file(reads) from read_pairs_ch
 
    output:
    file(pair_id) into quant_ch
 
    script:
    """
    salmon quant --threads $task.cpus --libType=U -i index -1 ${reads[0]} -2 ${reads[1]} -o $pair_id
    """
}
  
process fastqc {
    tag "FASTQC on $sample_id"

    input:
    set sample_id, file(reads) from read_pairs2_ch

    output:
    file("fastqc_${sample_id}_logs") into fastqc_ch


    script:
    """
    mkdir fastqc_${sample_id}_logs
    fastqc -o fastqc_${sample_id}_logs -f fastq -q ${reads}
    """  
}  
  
  
process multiqc {
    publishDir params.outdir, mode:'copy'
       
    input:
    file('*') from quant_ch.mix(fastqc_ch).collect()
    file(config) from multiqc_file
    
    output:
    file('multiqc_report.html')  
     
    script:
    """
    cp $config/* .
    echo "custom_logo: \$PWD/logo.png" >> multiqc_config.yaml
    multiqc . 
    """
}
 
workflow.onComplete { 
	println ( workflow.success ? "\nDone! Open the following report in your browser --> $params.outdir/multiqc_report.html\n" : "Oops .. something went wrong" )
}