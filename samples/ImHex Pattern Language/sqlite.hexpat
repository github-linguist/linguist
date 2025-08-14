#pragma description SQLite 3 database
#pragma author WerWolv
#pragma MIME application/vnd.sqlite3

import type.magic;

enum FileFormatVersion : u8 {
    Legacy = 1,
    WAL = 2
};

enum TextEncoding : u32 {
    UTF8 = 1,
    UTF16LE = 2,
    UTF16BE = 3
};

struct SQLiteVersion {
    u32 rawValue;
    
    u8 major = (rawValue / 1000000) % 1000 [[export]]; 
    u8 minor = (rawValue / 1000) % 1000 [[export]]; 
    u8 patch = (rawValue / 1) % 1000 [[export]]; 
} [[sealed, format("format_sqlite_version")]];

fn format_sqlite_version(ref auto version) {
    return std::format("v{}.{}.{} [{}]", version.major, version.minor, version.patch, version.rawValue);
};

u16 globalPageSize = 0;

enum PageType : u8 {
    InteriorIndexBTreePage = 0x02,
    InteriorTableBTreePage = 0x05,
    LeafIndexBTreePage     = 0x0A,
    LeafTableBTreePage     = 0x0D
};

struct Page {
    PageType pageType;
    u16 freeBlockStart;
    u16 numCells;
    u16 cellContentAreaStart;
    u8 fragmentedFreeBytesCount;
    
    if (pageType == PageType::InteriorIndexBTreePage || pageType == PageType::InteriorTableBTreePage)
        u32 rightMostPointer;
} [[fixed_size(le u16(globalPageSize))]];

struct DatabaseHeader {
    type::Magic<"SQLite format 3\x00"> magic;
    u16 pageSizeValue;
    u16 pageSize = pageSizeValue == 1 ? 65536 : le u16(pageSizeValue) [[export]];
    globalPageSize = pageSize;
    
    FileFormatVersion readVersion, writeVersion;
    padding[1];
    u8 maxEmbeddedPayloadFraction;
    if (maxEmbeddedPayloadFraction != 64)
        std::warning("Unexpected Max Embedded Payload Fraction");
    
    u8 minEmbeddedPayloadFraction;
    if (minEmbeddedPayloadFraction != 32)
        std::warning("Unexpected Min Embedded Payload Fraction");
        
    u8 leafPayloadFraction;
    if (leafPayloadFraction != 32)
        std::warning("Unexpected Leaf Payload Fraction");
        
    u32 fileChangeCounter;
    u32 totalPageCount;
    
    u32 freelistTrunkPageNumber;
    u32 totalFreeListPageCount;
    
    u32 schemaCookie;
    u32 schemaFormatNumber;
    u32 defaultPageCacheSize;
    u32 largestRootBTreePage;
    TextEncoding textEncoding;
    u32 userVersion;
    u32 incrementalVacuumMode;
    u32 applicationId;
    padding[20];
    u32 versionValidForNumber;
    SQLiteVersion sqliteVersionNumber;
};

struct SQLite3 {
    be DatabaseHeader header;
    
    be Page pages[le u16(header.totalPageCount) - 1] @ le u16(header.pageSize);
};

SQLite3 sqlite3 @ 0x00;