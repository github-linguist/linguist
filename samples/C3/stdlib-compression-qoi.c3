// Source: https://github.com/c3lang/c3c/blob/master/lib/std/compression/qoi.c3
// Permalink: https://github.com/c3lang/c3c/blob/d4fb5b7/lib/std/compression/qoi.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::compression::qoi;

const uint PIXELS_MAX = 400000000;

/**
 * Colorspace.
 * Purely informative. It will be saved to the file header,
 * but does not affect how chunks are en-/decoded.
 */
enum QOIColorspace : char (char id)
{
    SRGB = 0, 	// sRGB with linear alpha
    LINEAR = 1 	// all channels linear
}

/**
 * Channels.
 * The channels used in an image.
 * AUTO can be used when decoding to automatically determine
 * the channels from the file's header.
 */
enum QOIChannels : char (char id)
{
	AUTO = 0,
    RGB = 3,
    RGBA = 4
}

/**
 * Descriptor.
 * Contains information about an image.
 */
struct QOIDesc
{
	uint width;
	uint height;
	QOIChannels channels;
	QOIColorspace colorspace;
}

/**
 * QOI Errors.
 * These are all the possible bad outcomes.
 */
fault QOIError
{
	INVALID_PARAMETERS,
	FILE_OPEN_FAILED,
	FILE_WRITE_FAILED,
	INVALID_DATA,
	TOO_MANY_PIXELS
}


// Let the user decide if they want to use std::io
module std::compression::qoi @if(!$feature(QOI_NO_STDIO));
import std::io;

/**
 * Encode raw RGB or RGBA pixels into a QOI image and write it to the
 * file system.
 * 
 * The desc struct must be filled with the image width, height, the
 * used channels (QOIChannels.RGB or RGBA) and the colorspace
 * (QOIColorspace.SRGB or LINEAR).
 * 
 * The function returns an optional, which can either be a QOIError
 * or the number of bytes written on success.
 * 
 * @param [in] filename `The file's name to write the image to`
 * @param [in] input `The raw RGB or RGBA pixels to encode`
 * @param [&in] desc `The descriptor of the image`
 */
fn usz! write(String filename, char[] input, QOIDesc* desc)
{
	@pool() {
		// encode data
		char[] output = encode(input, desc)!;

		// open file
		File! f = file::open(filename, "wb");
		if (catch f) { return QOIError.FILE_OPEN_FAILED?; }

		// write data to file and close it
		usz! written = f.write(output);
		if (catch written) { return QOIError.FILE_WRITE_FAILED?; }
		if (catch f.close()) { return QOIError.FILE_WRITE_FAILED?; }

		return written;
	};
}


/**
 * Read and decode a QOI image from the file system.
 * 
 * If channels is set to QOIChannels.AUTO, the function will
 * automatically determine the channels from the file's header.
 * However, if channels is RGB or RGBA, the output format will be
 * forced into this number of channels.
 * 
 * The desc struct will be filled with the width, height,
 * channels and colorspace of the image.
 * 
 * The function returns an optional, which can either be a QOIError
 * or a char[] pointing to the decoded pixels on success.
 * 
 * The returned pixel data should be free()d after use, or the decoding
 * and use of the data should be wrapped in a @pool() { ... }; block.
 * 
 * @param [in] filename `The file's name to read the image from`
 * @param [&out] desc `The descriptor to fill with the image's info`
 * @param channels `The channels to be used`
 */
fn char[]! read(String filename, QOIDesc* desc, QOIChannels channels = AUTO, Allocator allocator = allocator::heap())
{
	// read file
	char[]! data = file::load_new(filename);
	if (catch data) return QOIError.FILE_OPEN_FAILED?;
	defer mem::free(data);

	// pass data to decode function
	return decode(data, desc, channels, allocator);
}


// Back to basic non-stdio mode
module std::compression::qoi;
import std::bits;

/**
 * Encode raw RGB or RGBA pixels into a QOI image in memory.
 * 
 * The function returns an optional, which can either be a QOIError
 * or a char[] pointing to the encoded data on success.
 * 
 * The returned qoi data should be free()d after use, or the encoding
 * and use of the data should be wrapped in a @pool() { ... }; block.
 * See the write() function for an example.
 * 
 * @param [in] input `The raw RGB or RGBA pixels to encode`
 * @param [&in] desc `The descriptor of the image`
 */
fn char[]! encode(char[] input, QOIDesc* desc, Allocator allocator = allocator::heap())
{
	// check info in desc
	if (desc.width == 0 || desc.height == 0) return QOIError.INVALID_PARAMETERS?;
	if (desc.channels == AUTO) return QOIError.INVALID_PARAMETERS?;
	uint pixels = desc.width * desc.height;
	if (pixels > PIXELS_MAX) return QOIError.TOO_MANY_PIXELS?;
	
	// check input data size
	uint image_size = pixels * desc.channels.id;
	if (image_size != input.len) return QOIError.INVALID_DATA?;

	// allocate memory for encoded data (output)
	// header + chunk tag and RGB(A) data for each pixel + end of stream
	uint max_size = Header.sizeof + pixels + image_size + END_OF_STREAM.len;
	char[] output = allocator::alloc_array(allocator, char, max_size); // no need to init
	defer catch allocator::free(allocator, output);

	// write header
	*(Header*)output.ptr = {
		.be_magic = bswap('qoif'),
		.be_width = bswap(desc.width),
		.be_height = bswap(desc.height),
		.channels = desc.channels.id,
		.colorspace = desc.colorspace.id
	};

	uint pos = Header.sizeof; 	// Current position in output
	uint loc;					// Current position in image (top-left corner)
	uint loc_end = image_size - desc.channels.id; // End of image data
	char run_length = 0; 		// Length of the current run
	
	Pixel[64] palette; // Zero-initialized by default
	Pixel prev = { 0, 0, 0, 255 };
	Pixel p = { 0, 0, 0, 255 };

	ichar[<3>] diff; // pre-allocate for diff
	ichar[<3>] luma; // ...and luma

	// write chunks
	for (loc = 0; loc < image_size; loc += desc.channels.id)
	{
		// set previous pixel
		prev = p;

		// get current pixel
		p[:3] = input[loc:3]; // cutesy slices :3
		if (desc.channels == RGBA) p.a = input[loc + 3];

		// check if we can run the previous pixel
		if (prev == p) {
			run_length++;
			if (run_length == 62 || loc == loc_end) {
				*@extract(OpRun, output, &pos) = { OP_RUN, run_length - 1 };
				run_length = 0;
			}
		} else {
			// end last run if there was one
			if (run_length > 0) {
				*@extract(OpRun, output, &pos) = { OP_RUN, run_length - 1 };
				run_length = 0;
			}

			switch {
				// check if we can index the palette
				case (palette[p.hash()] == p):
					*@extract(OpIndex, output, &pos) = {
						OP_INDEX,
						p.hash()
					};

				// check if we can use diff or luma
				case (prev != p && prev.a == p.a):
					// diff the pixels
					diff = p.rgb - prev.rgb;
					if (
						diff.r > -3 && diff.r < 2 &&
						diff.g > -3 && diff.g < 2 &&
						diff.b > -3 && diff.b < 2
					) {
						*@extract(OpDiff, output, &pos) = {
							OP_DIFF,
							(char)diff.r + 2,
							(char)diff.g + 2,
							(char)diff.b + 2
						};
						palette[p.hash()] = p;
					} else {
						// check luma eligibility
						luma = { diff.r - diff.g, diff.g, diff.b - diff.g };
						if (
							luma.r >= -8 && luma.r <= 7 &&
							luma.g >= -32 && luma.g <= 31 &&
							luma.b >= -8 && luma.b <= 7
						) {
							*@extract(OpLuma, output, &pos) = {
								OP_LUMA,
								(char)luma.g + 32,
								(char)luma.r + 8,
								(char)luma.b + 8
							};
							palette[p.hash()] = p;
						} else { nextcase; }
					}

				// worst case scenario: just encode the raw pixel
				default:
					if (prev.a != p.a) {
						*@extract(OpRGBA, output, &pos) = { OP_RGBA, p.r, p.g, p.b, p.a };
					} else {
						*@extract(OpRGB, output, &pos) = { OP_RGB, p.r, p.g, p.b };
					}
					palette[p.hash()] = p;
			}
		}
	}

	// write end of stream
	output[pos:END_OF_STREAM.len] = END_OF_STREAM;
	pos += END_OF_STREAM.len;
	
	return output[:pos];
}


/**
 * Decode a QOI image from memory.
 * 
 * If channels is set to QOIChannels.AUTO, the function will
 * automatically determine the channels from the file's header.
 * However, if channels is RGB or RGBA, the output format will be
 * forced into this number of channels.
 * 
 * The desc struct will be filled with the width, height,
 * channels and colorspace of the image.
 * 
 * The function returns an optional, which can either be a QOIError
 * or a char[] pointing to the decoded pixels on success.
 * 
 * The returned pixel data should be free()d after use, or the decoding
 * and use of the data should be wrapped in a @pool() { ... }; block.
 * 
 * @param [in] data `The QOI image data to decode`
 * @param [&out] desc `The descriptor to fill with the image's info`
 * @param channels `The channels to be used`
 */
fn char[]! decode(char[] data, QOIDesc* desc, QOIChannels channels = AUTO, Allocator allocator = allocator::heap())
{
	// check input data
	if (data.len < Header.sizeof + END_OF_STREAM.len) return QOIError.INVALID_DATA?;

	// get header
	Header* header = (Header*)data.ptr;

	// check magic bytes (FourCC)
	if (bswap(header.be_magic) != 'qoif') return QOIError.INVALID_DATA?;

	// copy header data to desc
	desc.width = bswap(header.be_width);
	desc.height = bswap(header.be_height);
	desc.channels = @enumcast(QOIChannels, header.channels)!; 			// Rethrow if invalid
	desc.colorspace = @enumcast(QOIColorspace, header.colorspace)!;	// Rethrow if invalid
	if (desc.channels == AUTO) return QOIError.INVALID_DATA?; // Channels must be specified in the header

	// check width and height
	if (desc.width == 0 || desc.height == 0) return QOIError.INVALID_DATA?;

	// check pixel count
	ulong pixels = (ulong)desc.width * (ulong)desc.height;
	if (pixels > PIXELS_MAX) return QOIError.TOO_MANY_PIXELS?;

	uint pos = Header.sizeof; 	// Current position in data
	uint loc;					// Current position in image (top-left corner)
	char run_length = 0; 		// Length of the current run
	char tag;					// Current chunk tag

	Pixel[64] palette; // Zero-initialized by default
	Pixel p = { 0, 0, 0, 255 };

	if (channels == AUTO) channels = desc.channels;
	
	// allocate memory for image data
	usz image_size = (usz)pixels * channels.id;
	char[] image = allocator::alloc_array(allocator, char, image_size);
	defer catch allocator::free(allocator, image);

	for (loc = 0; loc < image_size; loc += channels.id)
	{
		// get chunk tag
		tag = data[pos];

		// check for chunk type
		switch
		{
			case run_length > 0:
				run_length--;

			case tag == OP_RGB:
				OpRGB* op = @extract(OpRGB, data, &pos);
				p = { op.red, op.green, op.blue, p.a };
				palette[p.hash()] = p;

			case tag == OP_RGBA:
				OpRGBA* op = @extract(OpRGBA, data, &pos);
				p = { op.red, op.green, op.blue, op.alpha };
				palette[p.hash()] = p;
				
			case tag >> 6 == OP_INDEX:
				OpIndex* op = @extract(OpIndex, data, &pos);
				p = palette[op.index];

			case tag >> 6 == OP_DIFF:
				OpDiff* op = @extract(OpDiff, data, &pos);
				p.r += op.diff_red - 2;
				p.g += op.diff_green - 2;
				p.b += op.diff_blue - 2;
				palette[p.hash()] = p;

			case tag >> 6 == OP_LUMA:
				OpLuma* op = @extract(OpLuma, data, &pos);
				int diff_green = op.diff_green - 32;
				p.r += (char)(op.diff_red_minus_green - 8 + diff_green);
				p.g += (char)(diff_green);
				p.b += (char)(op.diff_blue_minus_green - 8 + diff_green);
				palette[p.hash()] = p;

			case tag >> 6 == OP_RUN:
				OpRun* op = @extract(OpRun, data, &pos);
				run_length = op.run;
		}

		// draw the pixel
		if (channels == RGBA) { image[loc:4] = p.rgba; } else { image[loc:3] = p.rgb; }
	}
	
	return image;
}



// ***************************************************************************
// ***                                                                     ***
// ***    Main functions are at the top to make the file more readable.    ***
// ***        From here on, helper functions and types are defined.        ***
// ***                                                                     ***
// ***************************************************************************
module std::compression::qoi @private;

// 8-bit opcodes
const OP_RGB = 0b11111110;
const OP_RGBA = 0b11111111;
// 2-bit opcodes
const OP_INDEX = 0b00;
const OP_DIFF = 0b01;
const OP_LUMA = 0b10;
const OP_RUN = 0b11;

struct Header @packed
{
	uint be_magic;	// magic bytes "qoif"
	uint be_width;	// image width in pixels (BE)
	uint be_height;	// image height in pixels (BE)

	// informative fields
	char channels;	// 3 = RGB, 4 = RGB
	char colorspace;	// 0 = sRGB with linear alpha, 1 = all channels linear
}

const char[*] END_OF_STREAM = {0, 0, 0, 0, 0, 0, 0, 1};

// inefficient, but it's only run once at a time
macro @enumcast($Type, raw)
{
	foreach (value : $Type.values) {
		if (value.id == raw) return value;
	}
	return QOIError.INVALID_DATA?;
}

distinct Pixel = inline char[<4>];
macro char Pixel.hash(Pixel p) {
	return (p.r * 3 + p.g * 5 + p.b * 7 + p.a * 11) % 64;
}

struct OpRGB // No need to use @packed here, the alignment is 1 anyways.
{
	char tag;
	char red;
	char green;
	char blue;
}
struct OpRGBA @packed
{
	char tag;
	char red;
	char green;
	char blue;
	char alpha;
}
bitstruct OpIndex : char
{
	char tag : 6..7;
	char index : 0..5;
}
bitstruct OpDiff : char
{
	char tag : 6..7;
	char diff_red : 4..5;
	char diff_green : 2..3;
	char diff_blue : 0..1;
}
bitstruct OpLuma : ushort
{
	char tag : 6..7;
	char diff_green : 0..5;
	char diff_red_minus_green : 12..15;
	char diff_blue_minus_green : 8..11;
}
bitstruct OpRun : char
{
	char tag : 6..7;
	char run : 0..5;
}

// Macro used to locate chunks in data buffers.
// Can be used both for reading and writing.
macro @extract($Type, char[] data, uint* pos)
{
	// slice data, then double cast
	$Type* chunk = ($Type*)data[*pos : $Type.sizeof].ptr;
	*pos += $Type.sizeof;
	return chunk;
}
