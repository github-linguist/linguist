.class public Lcom/tdq/game/shootbubble/sprite/PenguinSprite;
.super Lcom/tdq/game/shootbubble/sprite/Sprite;
.source "PenguinSprite.java"


# static fields
.field public static final LOST_SEQUENCE:[[I

.field public static final STATE_FIRE:I = 0x2

.field public static final STATE_GAME_LOST:I = 0x5

.field public static final STATE_GAME_WON:I = 0x4

.field public static final STATE_TURN_LEFT:I = 0x0

.field public static final STATE_TURN_RIGHT:I = 0x1

.field public static final STATE_VOID:I = 0x3

.field public static final WON_SEQUENCE:[[I


# instance fields
.field private count:I

.field private currentPenguin:I

.field private finalState:I

.field private nextPosition:I

.field private rand:Ljava/util/Random;

.field private spritesImage:Lcom/tdq/game/shootbubble/sprite/BmpWrap;


# direct methods
.method static constructor <clinit>()V
    .locals 8

    .prologue
    const/4 v7, 0x4

    const/4 v6, 0x3

    const/4 v5, 0x1

    const/4 v4, 0x0

    const/4 v3, 0x2

    .line 67
    const/16 v0, 0x8

    new-array v0, v0, [[I

    new-array v1, v3, [I

    fill-array-data v1, :array_0

    aput-object v1, v0, v4

    new-array v1, v3, [I

    fill-array-data v1, :array_1

    aput-object v1, v0, v5

    new-array v1, v3, [I

    fill-array-data v1, :array_2

    aput-object v1, v0, v3

    new-array v1, v3, [I

    fill-array-data v1, :array_3

    aput-object v1, v0, v6

    new-array v1, v3, [I

    fill-array-data v1, :array_4

    aput-object v1, v0, v7

    const/4 v1, 0x5

    new-array v2, v3, [I

    fill-array-data v2, :array_5

    aput-object v2, v0, v1

    const/4 v1, 0x6

    new-array v2, v3, [I

    fill-array-data v2, :array_6

    aput-object v2, v0, v1

    const/4 v1, 0x7

    new-array v2, v3, [I

    fill-array-data v2, :array_7

    aput-object v2, v0, v1

    sput-object v0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->LOST_SEQUENCE:[[I

    .line 69
    const/16 v0, 0x8

    new-array v0, v0, [[I

    new-array v1, v3, [I

    fill-array-data v1, :array_8

    aput-object v1, v0, v4

    new-array v1, v3, [I

    fill-array-data v1, :array_9

    aput-object v1, v0, v5

    new-array v1, v3, [I

    fill-array-data v1, :array_a

    aput-object v1, v0, v3

    new-array v1, v3, [I

    fill-array-data v1, :array_b

    aput-object v1, v0, v6

    new-array v1, v3, [I

    fill-array-data v1, :array_c

    aput-object v1, v0, v7

    const/4 v1, 0x5

    new-array v2, v3, [I

    fill-array-data v2, :array_d

    aput-object v2, v0, v1

    const/4 v1, 0x6

    new-array v2, v3, [I

    fill-array-data v2, :array_e

    aput-object v2, v0, v1

    const/4 v1, 0x7

    new-array v2, v3, [I

    fill-array-data v2, :array_f

    aput-object v2, v0, v1

    sput-object v0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->WON_SEQUENCE:[[I

    return-void

    .line 67
    :array_0
    .array-data 4
        0x1
        0x0
    .end array-data

    :array_1
    .array-data 4
        0x2
        0x8
    .end array-data

    :array_2
    .array-data 4
        0x3
        0x9
    .end array-data

    :array_3
    .array-data 4
        0x4
        0xa
    .end array-data

    :array_4
    .array-data 4
        0x5
        0xb
    .end array-data

    :array_5
    .array-data 4
        0x6
        0xc
    .end array-data

    :array_6
    .array-data 4
        0x7
        0xd
    .end array-data

    :array_7
    .array-data 4
        0x5
        0xe
    .end array-data

    .line 69
    :array_8
    .array-data 4
        0x1
        0x0
    .end array-data

    :array_9
    .array-data 4
        0x2
        0x7
    .end array-data

    :array_a
    .array-data 4
        0x3
        0x6
    .end array-data

    :array_b
    .array-data 4
        0x4
        0xf
    .end array-data

    :array_c
    .array-data 4
        0x5
        0x10
    .end array-data

    :array_d
    .array-data 4
        0x6
        0x11
    .end array-data

    :array_e
    .array-data 4
        0x7
        0x12
    .end array-data

    :array_f
    .array-data 4
        0x4
        0x13
    .end array-data
.end method

.method public constructor <init>(Lcom/tdq/game/shootbubble/sprite/BmpWrap;Ljava/util/Random;)V
    .locals 6
    .param p1, "sprites"    # Lcom/tdq/game/shootbubble/sprite/BmpWrap;
    .param p2, "rand"    # Ljava/util/Random;

    .prologue
    const/4 v5, 0x0

    .line 85
    new-instance v0, Landroid/graphics/Rect;

    const/16 v1, 0x169

    const/16 v2, 0x1b4

    const/16 v3, 0x1a0

    const/16 v4, 0x1df

    invoke-direct {v0, v1, v2, v3, v4}, Landroid/graphics/Rect;-><init>(IIII)V

    invoke-direct {p0, v0}, Lcom/tdq/game/shootbubble/sprite/Sprite;-><init>(Landroid/graphics/Rect;)V

    .line 87
    iput-object p1, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->spritesImage:Lcom/tdq/game/shootbubble/sprite/BmpWrap;

    .line 88
    iput-object p2, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->rand:Ljava/util/Random;

    .line 90
    iput v5, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    .line 92
    const/4 v0, 0x3

    iput v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->finalState:I

    .line 93
    iput v5, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->nextPosition:I

    .line 94
    return-void
.end method

.method public constructor <init>(Lcom/tdq/game/shootbubble/sprite/BmpWrap;Ljava/util/Random;IIII)V
    .locals 5
    .param p1, "sprites"    # Lcom/tdq/game/shootbubble/sprite/BmpWrap;
    .param p2, "rand"    # Ljava/util/Random;
    .param p3, "currentPenguin"    # I
    .param p4, "count"    # I
    .param p5, "finalState"    # I
    .param p6, "nextPosition"    # I

    .prologue
    .line 100
    new-instance v0, Landroid/graphics/Rect;

    const/16 v1, 0x169

    const/16 v2, 0x1b4

    const/16 v3, 0x1a0

    const/16 v4, 0x1df

    invoke-direct {v0, v1, v2, v3, v4}, Landroid/graphics/Rect;-><init>(IIII)V

    invoke-direct {p0, v0}, Lcom/tdq/game/shootbubble/sprite/Sprite;-><init>(Landroid/graphics/Rect;)V

    .line 102
    iput-object p1, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->spritesImage:Lcom/tdq/game/shootbubble/sprite/BmpWrap;

    .line 103
    iput-object p2, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->rand:Ljava/util/Random;

    .line 104
    iput p3, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    .line 105
    iput p4, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    .line 106
    iput p5, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->finalState:I

    .line 107
    iput p6, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->nextPosition:I

    .line 108
    return-void
.end method


# virtual methods
.method public getTypeId()I
    .locals 1

    .prologue
    .line 124
    sget v0, Lcom/tdq/game/shootbubble/sprite/Sprite;->TYPE_PENGUIN:I

    return v0
.end method

.method public paint(Landroid/graphics/Canvas;DII)V
    .locals 9
    .param p1, "c"    # Landroid/graphics/Canvas;
    .param p2, "scale"    # D
    .param p4, "dx"    # I
    .param p5, "dy"    # I

    .prologue
    .line 183
    invoke-virtual {p0}, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->getSpriteArea()Landroid/graphics/Rect;

    move-result-object v3

    .line 184
    .local v3, "r":Landroid/graphics/Rect;
    iget-object v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->spritesImage:Lcom/tdq/game/shootbubble/sprite/BmpWrap;

    const/16 v1, 0x168

    iget v2, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    rem-int/lit8 v2, v2, 0x4

    mul-int/lit8 v2, v2, 0x39

    sub-int/2addr v1, v2

    const/16 v2, 0x1b3

    iget v4, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    div-int/lit8 v4, v4, 0x4

    mul-int/lit8 v4, v4, 0x2d

    sub-int/2addr v2, v4

    move-object v4, p1

    move-wide v5, p2

    move v7, p4

    move v8, p5

    invoke-static/range {v0 .. v8}, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->drawImageClipped(Lcom/tdq/game/shootbubble/sprite/BmpWrap;IILandroid/graphics/Rect;Landroid/graphics/Canvas;DII)V

    .line 188
    return-void
.end method

.method public saveState(Landroid/os/Bundle;Ljava/util/Vector;)V
    .locals 5
    .param p1, "map"    # Landroid/os/Bundle;
    .param p2, "saved_sprites"    # Ljava/util/Vector;

    .prologue
    const/4 v4, 0x1

    const/4 v3, 0x0

    .line 111
    invoke-virtual {p0}, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->getSavedId()I

    move-result v0

    const/4 v1, -0x1

    if-eq v0, v1, :cond_0

    .line 120
    :goto_0
    return-void

    .line 114
    :cond_0
    invoke-super {p0, p1, p2}, Lcom/tdq/game/shootbubble/sprite/Sprite;->saveState(Landroid/os/Bundle;Ljava/util/Vector;)V

    .line 115
    const-string v0, "%d-currentPenguin"

    new-array v1, v4, [Ljava/lang/Object;

    invoke-virtual {p0}, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->getSavedId()I

    move-result v2

    invoke-static {v2}, Ljava/lang/Integer;->valueOf(I)Ljava/lang/Integer;

    move-result-object v2

    aput-object v2, v1, v3

    invoke-static {v0, v1}, Ljava/lang/String;->format(Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;

    move-result-object v0

    iget v1, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    invoke-virtual {p1, v0, v1}, Landroid/os/Bundle;->putInt(Ljava/lang/String;I)V

    .line 117
    const-string v0, "%d-count"

    new-array v1, v4, [Ljava/lang/Object;

    invoke-virtual {p0}, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->getSavedId()I

    move-result v2

    invoke-static {v2}, Ljava/lang/Integer;->valueOf(I)Ljava/lang/Integer;

    move-result-object v2

    aput-object v2, v1, v3

    invoke-static {v0, v1}, Ljava/lang/String;->format(Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;

    move-result-object v0

    iget v1, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    invoke-virtual {p1, v0, v1}, Landroid/os/Bundle;->putInt(Ljava/lang/String;I)V

    .line 118
    const-string v0, "%d-finalState"

    new-array v1, v4, [Ljava/lang/Object;

    invoke-virtual {p0}, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->getSavedId()I

    move-result v2

    invoke-static {v2}, Ljava/lang/Integer;->valueOf(I)Ljava/lang/Integer;

    move-result-object v2

    aput-object v2, v1, v3

    invoke-static {v0, v1}, Ljava/lang/String;->format(Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;

    move-result-object v0

    iget v1, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->finalState:I

    invoke-virtual {p1, v0, v1}, Landroid/os/Bundle;->putInt(Ljava/lang/String;I)V

    .line 119
    const-string v0, "%d-nextPosition"

    new-array v1, v4, [Ljava/lang/Object;

    invoke-virtual {p0}, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->getSavedId()I

    move-result v2

    invoke-static {v2}, Ljava/lang/Integer;->valueOf(I)Ljava/lang/Integer;

    move-result-object v2

    aput-object v2, v1, v3

    invoke-static {v0, v1}, Ljava/lang/String;->format(Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;

    move-result-object v0

    iget v1, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->nextPosition:I

    invoke-virtual {p1, v0, v1}, Landroid/os/Bundle;->putInt(Ljava/lang/String;I)V

    goto :goto_0
.end method

.method public updateState(I)V
    .locals 6
    .param p1, "state"    # I

    .prologue
    const/4 v5, 0x7

    const/4 v1, 0x3

    const/4 v4, 0x4

    const/4 v3, 0x1

    const/4 v2, 0x0

    .line 129
    iget v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->finalState:I

    if-eq v0, v1, :cond_2

    .line 130
    iget v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    add-int/lit8 v0, v0, 0x1

    iput v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    .line 132
    iget v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    rem-int/lit8 v0, v0, 0x6

    if-nez v0, :cond_0

    .line 133
    iget v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->finalState:I

    const/4 v1, 0x5

    if-ne v0, v1, :cond_1

    .line 134
    sget-object v0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->LOST_SEQUENCE:[[I

    iget v1, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->nextPosition:I

    aget-object v0, v0, v1

    aget v0, v0, v3

    iput v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    .line 135
    sget-object v0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->LOST_SEQUENCE:[[I

    iget v1, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->nextPosition:I

    aget-object v0, v0, v1

    aget v0, v0, v2

    iput v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->nextPosition:I

    .line 179
    :cond_0
    :goto_0
    return-void

    .line 136
    :cond_1
    iget v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->finalState:I

    if-ne v0, v4, :cond_0

    .line 137
    sget-object v0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->WON_SEQUENCE:[[I

    iget v1, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->nextPosition:I

    aget-object v0, v0, v1

    aget v0, v0, v3

    iput v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    .line 138
    sget-object v0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->WON_SEQUENCE:[[I

    iget v1, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->nextPosition:I

    aget-object v0, v0, v1

    aget v0, v0, v2

    iput v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->nextPosition:I

    goto :goto_0

    .line 142
    :cond_2
    iget v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    add-int/lit8 v0, v0, 0x1

    iput v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    .line 144
    packed-switch p1, :pswitch_data_0

    .line 170
    :cond_3
    :goto_1
    iget v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    const/16 v1, 0x64

    if-le v0, v1, :cond_5

    .line 171
    iput v5, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    goto :goto_0

    .line 146
    :pswitch_0
    iput v2, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    .line 147
    iput v1, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    goto :goto_1

    .line 150
    :pswitch_1
    iput v2, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    .line 151
    const/4 v0, 0x2

    iput v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    goto :goto_1

    .line 154
    :pswitch_2
    iput v2, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    .line 155
    iput v3, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    goto :goto_1

    .line 158
    :pswitch_3
    iget v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    if-lt v0, v4, :cond_4

    iget v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    if-le v0, v5, :cond_3

    .line 159
    :cond_4
    iput v2, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    goto :goto_1

    .line 164
    :pswitch_4
    iput v2, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    .line 165
    iput p1, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->finalState:I

    .line 166
    iput v2, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    goto :goto_0

    .line 172
    :cond_5
    iget v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    rem-int/lit8 v0, v0, 0xf

    if-nez v0, :cond_0

    iget v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->count:I

    const/16 v1, 0x19

    if-le v0, v1, :cond_0

    .line 173
    iget-object v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->rand:Ljava/util/Random;

    invoke-virtual {v0}, Ljava/util/Random;->nextInt()I

    move-result v0

    rem-int/lit8 v0, v0, 0x3

    add-int/lit8 v0, v0, 0x4

    iput v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    .line 174
    iget v0, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    if-ge v0, v4, :cond_0

    .line 175
    iput v2, p0, Lcom/tdq/game/shootbubble/sprite/PenguinSprite;->currentPenguin:I

    goto :goto_0

    .line 144
    nop

    :pswitch_data_0
    .packed-switch 0x0
        :pswitch_0
        :pswitch_1
        :pswitch_2
        :pswitch_3
        :pswitch_4
        :pswitch_4
    .end packed-switch
.end method
