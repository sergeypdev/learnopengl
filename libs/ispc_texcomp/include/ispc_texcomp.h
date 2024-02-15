////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2016-2019, Intel Corporation
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.
////////////////////////////////////////////////////////////////////////////////

#include <stdint.h>
#include <stdbool.h>

struct rgba_surface
{
    uint8_t* ptr;
    int32_t width;
    int32_t height;
    int32_t stride; // in bytes
};

struct bc7_enc_settings
{
    bool mode_selection[4];
    int refineIterations[8];

    bool skip_mode2;
    int fastSkipTreshold_mode1;
    int fastSkipTreshold_mode3;
    int fastSkipTreshold_mode7;

    int mode45_channel0;
    int refineIterations_channel;

    int channels;
};

struct bc6h_enc_settings
{
    bool slow_mode;
    bool fast_mode;
    int refineIterations_1p;
    int refineIterations_2p;
    int fastSkipTreshold;
};

struct etc_enc_settings
{
    int fastSkipTreshold;
};

struct astc_enc_settings
{
    int block_width;
    int block_height;
    int channels;

    int fastSkipTreshold;
    int refineIterations;
};

// profiles for RGB data (alpha channel will be ignored)
void GetProfile_ultrafast(struct bc7_enc_settings* settings);
void GetProfile_veryfast(struct bc7_enc_settings* settings);
void GetProfile_fast(struct bc7_enc_settings* settings);
void GetProfile_basic(struct bc7_enc_settings* settings);
void GetProfile_slow(struct bc7_enc_settings* settings);

// profiles for RGBA inputs
void GetProfile_alpha_ultrafast(struct bc7_enc_settings* settings);
void GetProfile_alpha_veryfast(struct bc7_enc_settings* settings);
void GetProfile_alpha_fast(struct bc7_enc_settings* settings);
void GetProfile_alpha_basic(struct bc7_enc_settings* settings);
void GetProfile_alpha_slow(struct bc7_enc_settings* settings);

// profiles for BC6H (RGB HDR)
void GetProfile_bc6h_veryfast(struct bc6h_enc_settings* settings);
void GetProfile_bc6h_fast(struct bc6h_enc_settings* settings);
void GetProfile_bc6h_basic(struct bc6h_enc_settings* settings);
void GetProfile_bc6h_slow(struct bc6h_enc_settings* settings);
void GetProfile_bc6h_veryslow(struct bc6h_enc_settings* settings);

// profiles for ETC
void GetProfile_etc_slow(struct etc_enc_settings* settings);

// profiles for ASTC
void GetProfile_astc_fast(struct astc_enc_settings* settings, int block_width, int block_height);
void GetProfile_astc_alpha_fast(struct astc_enc_settings* settings, int block_width, int block_height);
void GetProfile_astc_alpha_slow(struct astc_enc_settings* settings, int block_width, int block_height);

// helper function to replicate border pixels for the desired block sizes (bpp = 32 or 64)
void ReplicateBorders(struct rgba_surface* dst_slice, const struct rgba_surface* src_tex, int x, int y, int bpp);

/*
Notes:
    - input width and height need to be a multiple of block size
    - LDR input is 32 bit/pixel (sRGB), HDR is 64 bit/pixel (half float)
        - for BC4 input is 8bit/pixel (R8), for BC5 input is 16bit/pixel (RG8)
    - dst buffer must be allocated with enough space for the compressed texture:
        - 8 bytes/block for BC1/BC4/ETC1,
        - 16 bytes/block for BC3/BC5/BC6H/BC7/ASTC
    - the blocks are stored in raster scan order (natural CPU texture layout)
    - use the GetProfile_* functions to select various speed/quality tradeoffs
    - the RGB profiles are slightly faster as they ignore the alpha channel
*/

void CompressBlocksBC1(const struct rgba_surface* src, uint8_t* dst);
void CompressBlocksBC3(const struct rgba_surface* src, uint8_t* dst);
void CompressBlocksBC4(const struct rgba_surface* src, uint8_t* dst);
void CompressBlocksBC5(const struct rgba_surface* src, uint8_t* dst);
void CompressBlocksBC6H(const struct rgba_surface* src, uint8_t* dst, struct bc6h_enc_settings* settings);
void CompressBlocksBC7(const struct rgba_surface* src, uint8_t* dst, struct bc7_enc_settings* settings);
void CompressBlocksETC1(const struct rgba_surface* src, uint8_t* dst, struct etc_enc_settings* settings);
void CompressBlocksASTC(const struct rgba_surface* src, uint8_t* dst, struct astc_enc_settings* settings);
