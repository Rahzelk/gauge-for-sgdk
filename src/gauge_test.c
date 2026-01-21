#include "gauge_test.h"

/* ============================================================
   Color indices (PAL0)
   ============================================================ */
#define COL_EMPTY      0
#define COL_BG         1

#define Y_COL_VALUE    8   /* yellow */
#define Y_COL_TRAIL    5   /* red */

#define B_COL_VALUE    6   /* light cyan */
#define B_COL_TRAIL    7   /* cyan (slightly darker) */

/* ============================================================
   Yellow segments (2 segment IDs)
   ============================================================ */
#define YSEG_BOTH_LANES      0
#define YSEG_LANE0_ONLY      1
#define YSEG_COUNT           2

/* Blue has only one segment */
#define BSEG_ONLY            0
#define BSEG_COUNT           1

/* ============================================================
   Strips: 45 tiles * 8 u32 each = 360 u32 per strip
   ============================================================ */
static u32 s_yStripH[GAUGE_FILL_TILE_COUNT * GAUGE_TILE_U32_COUNT];
static u32 s_yStripV[GAUGE_FILL_TILE_COUNT * GAUGE_TILE_U32_COUNT];

static u32 s_bStripH_thin[GAUGE_FILL_TILE_COUNT * GAUGE_TILE_U32_COUNT];
static u32 s_bStripV_thin[GAUGE_FILL_TILE_COUNT * GAUGE_TILE_U32_COUNT];

/* Background tile data (1 tile) */
static u32 s_bgTileData[GAUGE_TILE_U32_COUNT];

/* ROM containers */
static GaugeRom s_yRomH, s_yRomV;
static GaugeRom s_bRomH, s_bRomV;

/* Layouts per orientation */
static GaugeLayout s_ex1H, s_ex2H, s_ex3YellowH, s_ex3BlueH;
static GaugeLayout s_ex1V, s_ex2V, s_ex3YellowV, s_ex3BlueV;

/* Blue percent LUT: 0..100 -> pixels 0..(blueLength*8) */
static u16 s_bluePercentToPixelsLUT[101];

/* ------------------------------------------------------------
   Helpers to build packed 4bpp rows in u32
   Pixel 0 is highest nibble (leftmost), pixel 7 is lowest nibble.
   ------------------------------------------------------------ */
static inline u32 buildSolidRow_u32(u8 col)
{
    u32 v = (u32)(col & 0x0F);
    return (v << 28) | (v << 24) | (v << 20) | (v << 16) | (v << 12) | (v << 8) | (v << 4) | v;
}

/* Full 8 pixels: [0..valuePx-1]=value, [valuePx..trailPx-1]=trail, [trailPx..7]=empty */
static inline u32 buildRowHorizontal_u32(u8 valuePx, u8 trailPx, u8 valueCol, u8 trailCol, u8 emptyCol)
{
    u32 row = 0;
    for (u8 p = 0; p < 8; p++)
    {
        u8 col;
        if (p < valuePx) col = valueCol;
        else if (p < trailPx) col = trailCol;
        else col = emptyCol;

        row = (row << 4) | (u32)(col & 0x0F);
    }
    return row;
}

/* Left 4 pixels empty, right 4 pixels 'col' (for thin vertical blue gauge) */
static inline u32 buildRowRightHalf_u32(u8 col, u8 emptyCol)
{
    /* pixels 0..3 empty, 4..7 col */
    const u32 e = (u32)(emptyCol & 0x0F);
    const u32 c = (u32)(col & 0x0F);

    /* build nibble-by-nibble (clear and explicit) */
    u32 row = 0;
    row = (row << 4) | e;
    row = (row << 4) | e;
    row = (row << 4) | e;
    row = (row << 4) | e;
    row = (row << 4) | c;
    row = (row << 4) | c;
    row = (row << 4) | c;
    row = (row << 4) | c;
    return row;
}

/* ------------------------------------------------------------
   Build strip: Horizontal (full height)
   Each tile has 8 identical rows.
   ------------------------------------------------------------ */
static void buildFillStripHorizontal_full(u32 *dstStrip, u8 valueCol, u8 trailCol, u8 emptyCol)
{
    u16 tileIdx = 0;

    for (u8 valuePx = 0; valuePx <= 8; valuePx++)
    {
        for (u8 trailPx = valuePx; trailPx <= 8; trailPx++)
        {
            const u32 row = buildRowHorizontal_u32(valuePx, trailPx, valueCol, trailCol, emptyCol);
            u32 *dst = &dstStrip[(u16)tileIdx * GAUGE_TILE_U32_COUNT];

            for (u8 r = 0; r < 8; r++) dst[r] = row;
            tileIdx++;
        }
    }
}

/* ------------------------------------------------------------
   Build strip: Horizontal (thin = bottom 4 rows only)
   Top 4 rows empty, bottom 4 rows use the horizontal fill row.
   ------------------------------------------------------------ */
static void buildFillStripHorizontal_thinBottom4(u32 *dstStrip, u8 valueCol, u8 trailCol, u8 emptyCol)
{
    u16 tileIdx = 0;
    const u32 emptyRow = buildSolidRow_u32(emptyCol);

    for (u8 valuePx = 0; valuePx <= 8; valuePx++)
    {
        for (u8 trailPx = valuePx; trailPx <= 8; trailPx++)
        {
            const u32 fillRow = buildRowHorizontal_u32(valuePx, trailPx, valueCol, trailCol, emptyCol);
            u32 *dst = &dstStrip[(u16)tileIdx * GAUGE_TILE_U32_COUNT];

            /* rows 0..3 empty, rows 4..7 filled */
            dst[0] = emptyRow;
            dst[1] = emptyRow;
            dst[2] = emptyRow;
            dst[3] = emptyRow;
            dst[4] = fillRow;
            dst[5] = fillRow;
            dst[6] = fillRow;
            dst[7] = fillRow;

            tileIdx++;
        }
    }
}

/* ------------------------------------------------------------
   Build strip: Vertical (full width)
   Fill is from bottom to top inside the tile:
     bottom valuePx rows => value
     then (trailPx-valuePx) rows => trail
     remaining top rows => empty
   Each row is a solid color across 8 pixels.
   ------------------------------------------------------------ */
static void buildFillStripVertical_full(u32 *dstStrip, u8 valueCol, u8 trailCol, u8 emptyCol)
{
    u16 tileIdx = 0;

    for (u8 valuePx = 0; valuePx <= 8; valuePx++)
    {
        for (u8 trailPx = valuePx; trailPx <= 8; trailPx++)
        {
            u32 *dst = &dstStrip[(u16)tileIdx * GAUGE_TILE_U32_COUNT];

            for (u8 rowTop = 0; rowTop < 8; rowTop++)
            {
                const u8 fromBottom = (u8)(7 - rowTop);
                u8 col;

                if (fromBottom < valuePx) col = valueCol;
                else if (fromBottom < trailPx) col = trailCol;
                else col = emptyCol;

                dst[rowTop] = buildSolidRow_u32(col);
            }

            tileIdx++;
        }
    }
}

/* ------------------------------------------------------------
   Build strip: Vertical thin (right 4 columns only)
   Still fills from bottom to top.
   Each row: left half empty, right half colored (value/trail/empty).
   ------------------------------------------------------------ */
static void buildFillStripVertical_thinRight4(u32 *dstStrip, u8 valueCol, u8 trailCol, u8 emptyCol)
{
    u16 tileIdx = 0;

    for (u8 valuePx = 0; valuePx <= 8; valuePx++)
    {
        for (u8 trailPx = valuePx; trailPx <= 8; trailPx++)
        {
            u32 *dst = &dstStrip[(u16)tileIdx * GAUGE_TILE_U32_COUNT];

            for (u8 rowTop = 0; rowTop < 8; rowTop++)
            {
                const u8 fromBottom = (u8)(7 - rowTop);
                u8 col;

                if (fromBottom < valuePx) col = valueCol;
                else if (fromBottom < trailPx) col = trailCol;
                else col = emptyCol;

                dst[rowTop] = buildRowRightHalf_u32(col, emptyCol);
            }

            tileIdx++;
        }
    }
}

/* ------------------------------------------------------------
   Background tile (checker)
   ------------------------------------------------------------ */
static void buildBackgroundTile(void)
{
    for (u8 r = 0; r < 8; r++)
    {
        /* Checker between COL_BG and COL_EMPTY */
        s_bgTileData[r] = (r & 1) ? 0x10101010 : 0x01010101;
    }
    VDP_loadTileData(s_bgTileData, GAUGE_TEST_VRAM_BG_TILE, 1, DMA);
}

/* ------------------------------------------------------------
   Palette setup (PAL0)
   ------------------------------------------------------------ */
static void setPal0(void)
{
    u16 pal[16];
    for (u8 i = 0; i < 16; i++) pal[i] = RGB24_TO_VDPCOLOR(0x000000);

    pal[COL_EMPTY]   = RGB24_TO_VDPCOLOR(0x000000);
    pal[COL_BG]      = RGB24_TO_VDPCOLOR(0x404040);

    pal[Y_COL_TRAIL] = RGB24_TO_VDPCOLOR(0xFF0000);
    pal[Y_COL_VALUE] = RGB24_TO_VDPCOLOR(0xFFFF00);

    pal[B_COL_VALUE] = RGB24_TO_VDPCOLOR(0x80FFFF);
    pal[B_COL_TRAIL] = RGB24_TO_VDPCOLOR(0x00A0FF);

    PAL_setPalette(PAL0, pal, DMA);
}

/* ------------------------------------------------------------
   Build LUT for blue percent -> pixels
   percent in [0..100], pixels in [0..blueMaxPixels]
   Done once at init (multiplication/division acceptable here).
   ------------------------------------------------------------ */
static void buildBluePercentLUT(void)
{
    const u16 blueMaxPixels = (u16)(GAUGE_TEST_BLUE_LENGTH_TILES * 8);

    for (u16 p = 0; p <= 100; p++)
    {
        /* Rounded mapping: (p * blueMaxPixels + 50) / 100 */
        const u32 v = (u32)p * (u32)blueMaxPixels + 50;
        s_bluePercentToPixelsLUT[p] = (u16)(v / 100);
    }
}

/* ------------------------------------------------------------
   Build ROM objects
   ------------------------------------------------------------ */
static void buildRoms(void)
{
    /* Yellow horizontal ROM */
    s_yRomH.segmentCount = YSEG_COUNT;

    s_yRomH.segmentLaneMask[YSEG_BOTH_LANES] = (GAUGE_LANE_MASK_0 | GAUGE_LANE_MASK_1);
    s_yRomH.segmentLaneMask[YSEG_LANE0_ONLY] = (GAUGE_LANE_MASK_0);

    s_yRomH.fillTileStrips[YSEG_BOTH_LANES][GAUGE_LANE_0] = s_yStripH;
    s_yRomH.fillTileStrips[YSEG_BOTH_LANES][GAUGE_LANE_1] = s_yStripH; /* same art on both lanes */
    s_yRomH.fillTileStrips[YSEG_LANE0_ONLY][GAUGE_LANE_0] = s_yStripH;
    s_yRomH.fillTileStrips[YSEG_LANE0_ONLY][GAUGE_LANE_1] = NULL;

    /* Yellow vertical ROM */
    s_yRomV.segmentCount = YSEG_COUNT;

    s_yRomV.segmentLaneMask[YSEG_BOTH_LANES] = (GAUGE_LANE_MASK_0 | GAUGE_LANE_MASK_1);
    s_yRomV.segmentLaneMask[YSEG_LANE0_ONLY] = (GAUGE_LANE_MASK_0);

    s_yRomV.fillTileStrips[YSEG_BOTH_LANES][GAUGE_LANE_0] = s_yStripV;
    s_yRomV.fillTileStrips[YSEG_BOTH_LANES][GAUGE_LANE_1] = s_yStripV;
    s_yRomV.fillTileStrips[YSEG_LANE0_ONLY][GAUGE_LANE_0] = s_yStripV;
    s_yRomV.fillTileStrips[YSEG_LANE0_ONLY][GAUGE_LANE_1] = NULL;

    /* Blue horizontal ROM (1 lane only) */
    s_bRomH.segmentCount = BSEG_COUNT;

    s_bRomH.segmentLaneMask[BSEG_ONLY] = (GAUGE_LANE_MASK_0);
    s_bRomH.fillTileStrips[BSEG_ONLY][GAUGE_LANE_0] = s_bStripH_thin;
    s_bRomH.fillTileStrips[BSEG_ONLY][GAUGE_LANE_1] = NULL;

    /* Blue vertical ROM (1 lane only) */
    s_bRomV.segmentCount = BSEG_COUNT;

    s_bRomV.segmentLaneMask[BSEG_ONLY] = (GAUGE_LANE_MASK_0);
    s_bRomV.fillTileStrips[BSEG_ONLY][GAUGE_LANE_0] = s_bStripV_thin;
    s_bRomV.fillTileStrips[BSEG_ONLY][GAUGE_LANE_1] = NULL;
}

/* ------------------------------------------------------------
   Build layouts for examples (base P1 layouts)
   Orientation differences are ONLY fill direction:
     - Horizontal: fill forward (left->right)
     - Vertical  : fill reverse (bottom->top) because cell0 is drawn at the top
   ------------------------------------------------------------ */
static void buildLayoutsHorizontal(void)
{
    /* Example 1: yellow 1 lane => all cells lane0-only segment */
    s_ex1H.lengthTiles = GAUGE_TEST_YELLOW_LENGTH_TILES;
    for (u8 c = 0; c < s_ex1H.lengthTiles; c++) s_ex1H.segmentIdByCell[c] = YSEG_LANE0_ONLY;
    GaugeLayout_setFillForward(&s_ex1H);
    GaugeLayout_buildLaneListsFromRom(&s_ex1H, &s_yRomH);

    /* Example 2: yellow 2 lanes => all cells both-lanes segment */
    s_ex2H.lengthTiles = GAUGE_TEST_YELLOW_LENGTH_TILES;
    for (u8 c = 0; c < s_ex2H.lengthTiles; c++) s_ex2H.segmentIdByCell[c] = YSEG_BOTH_LANES;
    GaugeLayout_setFillForward(&s_ex2H);
    GaugeLayout_buildLaneListsFromRom(&s_ex2H, &s_yRomH);

    /* Example 3 yellow: first 4 cells both lanes, rest lane0-only */
    s_ex3YellowH.lengthTiles = GAUGE_TEST_YELLOW_LENGTH_TILES;
    for (u8 c = 0; c < s_ex3YellowH.lengthTiles; c++)
    {
        if (c < GAUGE_TEST_EX3_HEAD_2LANE_TILES) s_ex3YellowH.segmentIdByCell[c] = YSEG_BOTH_LANES;
        else s_ex3YellowH.segmentIdByCell[c] = YSEG_LANE0_ONLY;
    }
    GaugeLayout_setFillForward(&s_ex3YellowH);
    GaugeLayout_buildLaneListsFromRom(&s_ex3YellowH, &s_yRomH);

    /* Example 3 blue: length 8, 1 lane, aligned with the tail part (start offset handled in main) */
    s_ex3BlueH.lengthTiles = GAUGE_TEST_BLUE_LENGTH_TILES;
    for (u8 c = 0; c < s_ex3BlueH.lengthTiles; c++) s_ex3BlueH.segmentIdByCell[c] = BSEG_ONLY;
    GaugeLayout_setFillForward(&s_ex3BlueH);
    GaugeLayout_buildLaneListsFromRom(&s_ex3BlueH, &s_bRomH);
}

static void buildLayoutsVertical(void)
{
    /* Same segment distributions, but fill reverse for bottom->top */
    s_ex1V = s_ex1H;
    GaugeLayout_setFillReverse(&s_ex1V);
    GaugeLayout_buildLaneListsFromRom(&s_ex1V, &s_yRomV);

    s_ex2V = s_ex2H;
    GaugeLayout_setFillReverse(&s_ex2V);
    GaugeLayout_buildLaneListsFromRom(&s_ex2V, &s_yRomV);

    s_ex3YellowV = s_ex3YellowH;
    GaugeLayout_setFillReverse(&s_ex3YellowV);
    GaugeLayout_buildLaneListsFromRom(&s_ex3YellowV, &s_yRomV);

    s_ex3BlueV = s_ex3BlueH;
    GaugeLayout_setFillReverse(&s_ex3BlueV);
    GaugeLayout_buildLaneListsFromRom(&s_ex3BlueV, &s_bRomV);
}

void GaugeTest_init(void)
{
    setPal0();

    /* Yellow strips */
    buildFillStripHorizontal_full(s_yStripH, Y_COL_VALUE, Y_COL_TRAIL, COL_EMPTY);
    buildFillStripVertical_full(s_yStripV, Y_COL_VALUE, Y_COL_TRAIL, COL_EMPTY);

    /* Blue strips (thin) */
    buildFillStripHorizontal_thinBottom4(s_bStripH_thin, B_COL_VALUE, B_COL_TRAIL, COL_EMPTY);
    buildFillStripVertical_thinRight4(s_bStripV_thin, B_COL_VALUE, B_COL_TRAIL, COL_EMPTY);

    buildBackgroundTile();
    buildBluePercentLUT();
    buildRoms();

    buildLayoutsHorizontal();
    buildLayoutsVertical();
}

const u16* GaugeTest_getBluePercentToPixelsLUT(void)
{
    return s_bluePercentToPixelsLUT;
}

const GaugeRom* GaugeTest_getYellowRom(GaugeOrientation orientation)
{
    return (orientation == GAUGE_ORIENT_VERTICAL) ? &s_yRomV : &s_yRomH;
}

const GaugeRom* GaugeTest_getBlueRom(GaugeOrientation orientation)
{
    return (orientation == GAUGE_ORIENT_VERTICAL) ? &s_bRomV : &s_bRomH;
}

const GaugeLayout* GaugeTest_getLayoutExample1(GaugeOrientation orientation)
{
    return (orientation == GAUGE_ORIENT_VERTICAL) ? &s_ex1V : &s_ex1H;
}

const GaugeLayout* GaugeTest_getLayoutExample2(GaugeOrientation orientation)
{
    return (orientation == GAUGE_ORIENT_VERTICAL) ? &s_ex2V : &s_ex2H;
}

const GaugeLayout* GaugeTest_getLayoutExample3Yellow(GaugeOrientation orientation)
{
    return (orientation == GAUGE_ORIENT_VERTICAL) ? &s_ex3YellowV : &s_ex3YellowH;
}

const GaugeLayout* GaugeTest_getLayoutExample3Blue(GaugeOrientation orientation)
{
    return (orientation == GAUGE_ORIENT_VERTICAL) ? &s_ex3BlueV : &s_ex3BlueH;
}
