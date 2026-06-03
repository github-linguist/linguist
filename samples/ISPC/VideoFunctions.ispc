export struct VideoData {
    uint16 width;
    uint16 height;
    uint16 buffer_width;
    uint16 buffer_height;
    int16 yoffset;
    int16 xoffset;
    int16 ytail;
    int16 xtail;
    bool transparentBlack;
    int16 transparentBlackLevel;
    int16 image_width;
    int16 image_height;
    int32 startx;
    int32 starty;
    int32 sampleSpacing;

    int8 ch;
    uint8* image;
    void* bufferData;
};

export void VideoEffectProcess(const uniform VideoData &data,
                               uniform int startIdx, uniform int endIdx,
                               uniform uint8<4> result[])
{
    uniform int ch = data.ch;


    foreach (index = startIdx...endIdx) {
        float fidx = index;
        float y = (floor)(fidx / data.buffer_width);
        float x = fidx - (y * data.buffer_width);
        int newIndex = x + data.startx + ((y + data.starty) * data.buffer_width);

        uint8* ptr = data.image + (data.image_height - 1 - y - data.yoffset) * (data.image_width * ch) + (data.xoffset * ch) + (x * ch);
        uint8<4> c;
        c.x = *(ptr);
        c.y = *(ptr + 1);
        c.z = *(ptr + 2);
        c.w = ch == 3 ? 255 : *(ptr + 3);
        uint16 level = (uint16)c[0] + (uint16)c[1] + (uint16)c[2];
        if (data.transparentBlack) {
            if (level > data.transparentBlackLevel) {
                result[newIndex] = c;
            }
        } else {
            result[newIndex] = c;
        }
    }
}

export void VideoEffectProcessSample(const uniform VideoData &data,
                                        uniform int startIdx, uniform int endIdx, 
                                        uniform uint8<4> result[]) {
    uniform float width = data.width;
    uniform float rszw = 1.0 / width;
    uniform float height = data.height;
    uniform int ch = data.ch;

    foreach (index = startIdx ... endIdx) {
        float fidx = index;
        float y = (floor)(fidx * rszw + 0.00001);
        float x = fidx - (y * width);
        float newIndex = (y * width) + x;

        int curx = data.startx + x * data.sampleSpacing;
        int cury = data.starty + y * data.sampleSpacing;

        if (curx >= 0 && curx < data.image_width && cury >= 0 && cury < data.image_height) {

            uint8* ptr = data.image + (data.image_height - 1 - cury) * data.image_width * ch + curx * ch;
            uint8<4> c;
            c.x = *(ptr);
            c.y = *(ptr + 1);
            c.z = *(ptr + 2);
            c.w = ch == 3 ? 255 : *(ptr + 3);
            if (data.transparentBlack) {
                uint16 level = (uint16)c[0] + (uint16)c[1] + (uint16)c[2];
                if (level > data.transparentBlackLevel) {
                    result[newIndex] = c;
                }
            } else {
                result[newIndex] = c;
            }
        }
    }
}
