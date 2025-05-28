#pragma author WerWolv
#pragma description JPEG Image

import std.io;
import std.mem;
import type.magic;
#pragma endian big

#pragma MIME image/jpeg

enum Marker : u8 {
    TEM     = 0x01,
    SOF0    = 0xC0,
    SOF1    = 0xC1,
    SOF2    = 0xC2,
    SOF3    = 0xC3,
    DHT     = 0xC4,
    SOF5    = 0xC5,
    SOF6    = 0xC6,
    SOF7    = 0xC7,
    SOI     = 0xD8,
    EOI     = 0xD9,
    SOS     = 0xDA,
    DQT     = 0xDB,
    DNL     = 0xDC,
    DRI     = 0xDD,
    DHP     = 0xDE,
    APP0    = 0xE0,
    APP1    = 0xE1,
    APP2    = 0xE2,
    APP3    = 0xE3,
    APP4    = 0xE4,
    APP5    = 0xE5,
    APP6    = 0xE6,
    APP7    = 0xE7,
    APP8    = 0xE8,
    APP9    = 0xE9,
    APP10   = 0xEA,
    APP11   = 0xEB,
    APP12   = 0xEC,
    APP13   = 0xED,
    APP14   = 0xEE,
    APP15   = 0xEF,
    COM     = 0xFE,
    UNKNOWN = 0x00
};

enum DensityUnit : u8 {
    NoUnit = 0x00,
    PixelsPerInch = 0x01,
    PixelsPerCm = 0x02
};

enum ColorTransform : u8 {
    Unknown = 0x00,
    YCbCr = 0x01,
    YCCK = 0x02
};

struct Pixel {
    u8 r, g, b;
} [[sealed, transform("transform_pixel")]];

fn transform_pixel(Pixel pixel) {
    return (0xFF << 24) | (pixel.b << 16) | (pixel.g << 8) | (pixel.r << 0);
};


struct APP0 {
    type::Magic<"JFIF\x00"> magic;
    u8 versionMajor, versionMinor;
    DensityUnit densityUnit;
    u16 densityX, densityY;
    u8 thumbnailX, thumbnailY;
    Pixel thumbnail[thumbnailX * thumbnailY] [[sealed]];
};

struct APP14 {
    type::Magic<"Adobe"> magic;
    u16 version;
    u16 flags0;
    u16 flags1;
    ColorTransform transform;
};

enum ComponentId : u8 {
    Y   = 1,
    CB  = 2,
    CR  = 3,
    I   = 4,
    Q   = 5,
    B   = 66,
    G   = 71,
    R   = 82
};

struct SOF0Component {
    ComponentId componentId;
    u8 samplingFactors;
    u8 quantizationTableId;
} [[format_read("sof0_component_read")]];

fn sof0_component_read(SOF0Component c) {
    return std::format("({}, {}, {})", c.componentId, c.samplingFactors, c.quantizationTableId);
};

fn get_eoi_marker_position() {
    u32 pos = std::mem::find_sequence_in_range(0, $, std::mem::size(), 0xFF, 0xD9);
    return pos;

};

struct SOF0 {
    u8 bitsPerSample;
    u16 imageHeight, imageWidth;
    u8 numComponents;
    SOF0Component components[numComponents];
};

struct SOSComponent {
    ComponentId componentId;
    u8 huffmanTable;
};

struct SOS {
    u8 numComponents;
    SOSComponent components[numComponents];
    u8 startSpectralSelection;
    u8 endSpectral;
    u8 apprBitPos;
    u8 image_data[get_eoi_marker_position() - $] [[sealed]];
};

struct Segment {
    if (std::mem::read_unsigned($, 1) != 0xFF) {
        Marker marker = Marker::UNKNOWN;
        u8 data[get_eoi_marker_position() - $];
    } else {
        type::Magic<"\xff"> magic;
        Marker marker;

        if (marker == Marker::SOI || marker == Marker::EOI) {

        } else {
            u16 length;
            if (marker == Marker::APP0) {
                APP0 data;
            } else if (marker == Marker::APP14) {
                APP14 data;
            } else if (marker == Marker::COM) {
                char data[length - sizeof(length)];
            } else if (marker == Marker::SOF0) {
                SOF0 data;
            } else if (marker == Marker::SOS) {
                SOS data;
            } else {
                u8 data[length - sizeof(length)] [[sealed]];
            }
        }
    }
} [[format_read("segment_read")]];

fn segment_read(Segment s) {
    return std::format("{}", s.marker);
};

Segment segments[while(!std::mem::eof())] @ 0x00 [[hex::visualize("image", this)]];
