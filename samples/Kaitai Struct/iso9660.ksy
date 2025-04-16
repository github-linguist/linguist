meta:
  id: iso9660
  title: ISO9660 CD filesystem
  file-extension: iso
  xref:
    wikidata: Q815645
  license: CC0-1.0
doc: |
  ISO9660 is standard filesystem used on read-only optical discs
  (mostly CD-ROM). The standard was based on earlier High Sierra
  Format (HSF), proposed for CD-ROMs in 1985, and, after several
  revisions, it was accepted as ISO9960:1998.

  The format emphasizes portability (thus having pretty minimal
  features and very conservative file names standards) and sequential
  access (which favors disc devices with relatively slow rotation
  speed).
types:
  vol_desc:
    seq:
      - id: type
        type: u1
      - id: magic
        contents: "CD001"
      - id: version
        type: u1
      - id: vol_desc_boot_record
        type: vol_desc_boot_record
        if: type == 0
      - id: vol_desc_primary
        type: vol_desc_primary
        if: type == 1
  vol_desc_boot_record:
    seq:
      - id: boot_system_id
        type: str
        size: 32
        encoding: UTF-8
      - id: boot_id
        type: str
        size: 32
        encoding: UTF-8
  vol_desc_primary:
    doc-ref: 'http://wiki.osdev.org/ISO_9660#The_Primary_Volume_Descriptor'
    seq:
      - id: unused1
        contents: [0]
      - id: system_id
        type: str
        size: 32
        encoding: UTF-8
      - id: volume_id
        type: str
        size: 32
        encoding: UTF-8
      - id: unused2
        contents: [0, 0, 0, 0, 0, 0, 0, 0]
      - id: vol_space_size
        type: u4bi
      - id: unused3
        contents: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      - id: vol_set_size
        type: u2bi
      - id: vol_seq_num
        type: u2bi
      - id: logical_block_size
        type: u2bi
      - id: path_table_size
        type: u4bi
      - id: lba_path_table_le
        type: u4le
      - id: lba_opt_path_table_le
        type: u4le
      - id: lba_path_table_be
        type: u4be
      - id: lba_opt_path_table_be
        type: u4be
      - id: root_dir
        type: dir_entry
        size: 34
      - id: vol_set_id
        type: str
        size: 128
        encoding: UTF-8
      - id: publisher_id
        type: str
        size: 128
        encoding: UTF-8
      - id: data_preparer_id
        type: str
        size: 128
        encoding: UTF-8
      - id: application_id
        type: str
        size: 128
        encoding: UTF-8
      - id: copyright_file_id
        type: str
        size: 38
        encoding: UTF-8
      - id: abstract_file_id
        type: str
        size: 36
        encoding: UTF-8
      - id: bibliographic_file_id
        type: str
        size: 37
        encoding: UTF-8
      - id: vol_create_datetime
        type: dec_datetime
      - id: vol_mod_datetime
        type: dec_datetime
      - id: vol_expire_datetime
        type: dec_datetime
      - id: vol_effective_datetime
        type: dec_datetime
      - id: file_structure_version
        type: u1
      - id: unused4
        type: u1
      - id: application_area
        size: 512
    instances:
      path_table:
        pos: lba_path_table_le * _root.sector_size
        size: path_table_size.le
        type: path_table_le
  dir_entries:
    seq:
      - id: entries
        type: dir_entry
        repeat: until
        repeat-until: _.len == 0
  dir_entry:
    seq:
      - id: len
        type: u1
      - id: body
        type: dir_entry_body
        size: len - 1
        if: len > 0
  dir_entry_body:
    seq:
      - id: len_ext_attr_rec
        type: u1
      - id: lba_extent
        type: u4bi
      - id: size_extent
        type: u4bi
      - id: datetime
        type: datetime
      - id: file_flags
        type: u1
      - id: file_unit_size
        type: u1
      - id: interleave_gap_size
        type: u1
      - id: vol_seq_num
        type: u2bi
      - id: len_file_name
        type: u1
      - id: file_name
        type: str
        encoding: UTF-8
        size: len_file_name
      - id: padding
        type: u1
        if: len_file_name % 2 == 0
      - id: rest
        size-eos: true
    instances:
      extent_as_dir:
        io: _root._io
        pos: lba_extent.le * _root.sector_size
        size: size_extent.le
        type: dir_entries
        if: file_flags & 2 != 0
      extent_as_file:
        io: _root._io
        pos: lba_extent.le * _root.sector_size
        size: size_extent.le
        if: file_flags & 2 == 0
  ## AKA "Path Table Entry"
  path_table_le:
    doc-ref: 'http://wiki.osdev.org/ISO_9660#The_Path_Table'
    seq:
      - id: entries
        type: path_table_entry_le
        repeat: eos
  path_table_entry_le:
    seq:
      - id: len_dir_name
        type: u1
      - id: len_ext_attr_rec
        type: u1
      - id: lba_extent
        type: u4le
      - id: parent_dir_idx
        type: u2le
      - id: dir_name
        type: str
        encoding: UTF-8
        size: len_dir_name
      - id: padding
        type: u1
        if: len_dir_name % 2 == 1
  datetime:
    seq:
      - id: year
        type: u1
      - id: month
        type: u1
      - id: day
        type: u1
      - id: hour
        type: u1
      - id: minute
        type: u1
      - id: sec
        type: u1
      - id: timezone
        type: u1
  dec_datetime:
    doc-ref: 'http://wiki.osdev.org/ISO_9660#Date.2Ftime_format'
    seq:
      - id: year
        type: str
        size: 4
        encoding: ASCII
      - id: month
        type: str
        size: 2
        encoding: ASCII
      - id: day
        type: str
        size: 2
        encoding: ASCII
      - id: hour
        type: str
        size: 2
        encoding: ASCII
      - id: minute
        type: str
        size: 2
        encoding: ASCII
      - id: sec
        type: str
        size: 2
        encoding: ASCII
      - id: sec_hundreds
        type: str
        size: 2
        encoding: ASCII
      - id: timezone
        type: u1
  u2bi:
    seq:
      - id: le
        type: u2le
      - id: be
        type: u2be
  u4bi:
    seq:
      - id: le
        type: u4le
      - id: be
        type: u4be
instances:
  sector_size:
    value: 2048
  primary_vol_desc:
    type: vol_desc
    pos: 0x010 * sector_size
