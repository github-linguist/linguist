meta:
  id: zip
  title: ZIP archive file
  file-extension: zip
  xref:
    iso: 21320-1
    justsolve: ZIP
    loc:
      - fdd000354
      - fdd000355
      - fdd000362
      - fdd000361
    pronom: x-fmt/263
    wikidata: Q136218
  endian: le
  license: CC0-1.0
doc: |
  ZIP is a popular archive file format, introduced in 1989 by Phil Katz
  and originally implemented in PKZIP utility by PKWARE.

  Thanks to solid support of it in most desktop environments and
  operating systems, and algorithms / specs availability in public
  domain, it quickly became tool of choice for implementing file
  containers.

  For example, Java .jar files, OpenDocument, Office Open XML, EPUB files
  are actually ZIP archives.
doc-ref: https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
seq:
  - id: sections
    type: pk_section
    repeat: eos
types:
  pk_section:
    seq:
      - id: magic
        contents: 'PK'
      - id: section_type
        type: u2
      - id: body
        type:
          switch-on: section_type
          cases:
            0x0201: central_dir_entry
            0x0403: local_file
            0x0605: end_of_central_dir
            0x0807: data_descriptor
  data_descriptor:
    seq:
      - id: crc32
        type: u4
      - id: len_body_compressed
        type: u4
      - id: len_body_uncompressed
        type: u4
  local_file:
    seq:
      - id: header
        type: local_file_header
      - id: body
        size: header.len_body_compressed
  local_file_header:
    seq:
      - id: version
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
        enum: compression
      - id: file_mod_time
        type: u2
      - id: file_mod_date
        type: u2
      - id: crc32
        type: u4
      - id: len_body_compressed
        type: u4
      - id: len_body_uncompressed
        type: u4
      - id: len_file_name
        type: u2
      - id: len_extra
        type: u2
      - id: file_name
        type: str
        size: len_file_name
        encoding: UTF-8
      - id: extra
        size: len_extra
        type: extras
  central_dir_entry:
    doc-ref: https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT - 4.3.12
    seq:
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
        enum: compression
      - id: last_mod_file_time
        type: u2
      - id: last_mod_file_date
        type: u2
      - id: crc32
        type: u4
      - id: len_body_compressed
        type: u4
      - id: len_body_uncompressed
        type: u4
      - id: len_file_name
        type: u2
      - id: len_extra
        type: u2
      - id: len_comment
        type: u2
      - id: disk_number_start
        type: u2
      - id: int_file_attr
        type: u2
      - id: ext_file_attr
        type: u4
      - id: ofs_local_header
        type: s4
      - id: file_name
        type: str
        size: len_file_name
        encoding: UTF-8
      - id: extra
        size: len_extra
        type: extras
      - id: comment
        type: str
        size: len_comment
        encoding: UTF-8
    instances:
      local_header:
        pos: ofs_local_header
        type: pk_section
  # https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT - 4.3.16
  end_of_central_dir:
    seq:
      - id: disk_of_end_of_central_dir
        type: u2
      - id: disk_of_central_dir
        type: u2
      - id: num_central_dir_entries_on_disk
        type: u2
      - id: num_central_dir_entries_total
        type: u2
      - id: len_central_dir
        type: u4
      - id: ofs_central_dir
        type: u4
      - id: len_comment
        type: u2
      - id: comment
        type: str
        size: len_comment
        encoding: UTF-8
  extras:
    seq:
      - id: entries
        type: extra_field
        repeat: eos
  extra_field:
    seq:
      - id: code
        type: u2
        enum: extra_codes
      - id: len_body
        type: u2
      - id: body
        size: len_body
        type:
          switch-on: code
          cases:
            'extra_codes::ntfs': ntfs
            'extra_codes::extended_timestamp': extended_timestamp
            'extra_codes::infozip_unix_var_size': infozip_unix_var_size
    types:
      ntfs:
        doc-ref: 'https://github.com/LuaDist/zip/blob/master/proginfo/extrafld.txt#L191'
        seq:
          - id: reserved
            type: u4
          - id: attributes
            type: attribute
            repeat: eos
        types:
          attribute:
            seq:
              - id: tag
                type: u2
              - id: len_body
                type: u2
              - id: body
                size: len_body
                type:
                  switch-on: tag
                  cases:
                    1: attribute_1
          attribute_1:
            seq:
              - id: last_mod_time
                type: u8
              - id: last_access_time
                type: u8
              - id: creation_time
                type: u8
      extended_timestamp:
        doc-ref: 'https://github.com/LuaDist/zip/blob/master/proginfo/extrafld.txt#L817'
        seq:
          - id: flags
            type: u1
          - id: mod_time
            type: u4
          - id: access_time
            type: u4
            if: not _io.eof
          - id: create_time
            type: u4
            if: not _io.eof
      infozip_unix_var_size:
        doc-ref: 'https://github.com/LuaDist/zip/blob/master/proginfo/extrafld.txt#L1339'
        seq:
          - id: version
            type: u1
            doc: Version of this extra field, currently 1
          - id: len_uid
            type: u1
            doc: Size of UID field
          - id: uid
            size: len_uid
            doc: UID (User ID) for a file
          - id: len_gid
            type: u1
            doc: Size of GID field
          - id: gid
            size: len_gid
            doc: GID (Group ID) for a file
enums:
  compression:
    0: none
    1: shrunk
    2: reduced_1
    3: reduced_2
    4: reduced_3
    5: reduced_4
    6: imploded
    8: deflated
    9: enhanced_deflated
    10: pkware_dcl_imploded
    12: bzip2
    14: lzma
    18: ibm_terse
    19: ibm_lz77_z
    98: ppmd
  extra_codes:
    # https://github.com/LuaDist/zip/blob/master/proginfo/extrafld.txt
    0x0001: zip64
    0x0007: av_info
#    0x0008: reserved for extended language encoding data (PFS) (see APPENDIX D)
    0x0009: os2
    0x000a: ntfs
    0x000c: openvms
    0x000d: pkware_unix
    0x000e: file_stream_and_fork_descriptors
    0x000f: patch_descriptor
    0x0014: pkcs7
    0x0015: x509_cert_id_and_signature_for_file
    0x0016: x509_cert_id_for_central_dir
    0x0017: strong_encryption_header
    0x0018: record_management_controls
    0x0019: pkcs7_enc_recip_cert_list
    0x0065: ibm_s390_uncomp
    0x0066: ibm_s390_comp
    0x4690: poszip_4690
    0x5455: extended_timestamp
    0x7855: infozip_unix
    0x7875: infozip_unix_var_size
