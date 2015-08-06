.class public Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;
.super Ljava/lang/Object;
.source "DoodleMobileAnaylise.java"


# annotations
.annotation system Ldalvik/annotation/MemberClasses;
    value = {
        Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$Sync;,
        Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$SessionPolling;,
        Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$MobclixHttpClient;,
        Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$FetchRemoteConfig;,
        Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$LogEvent;
    }
.end annotation


# static fields
.field static final DEBUG:Z = false

.field public static final LOG_LEVEL_DEBUG:I = 0x1

.field public static final LOG_LEVEL_ERROR:I = 0x8

.field public static final LOG_LEVEL_FATAL:I = 0x10

.field public static final LOG_LEVEL_INFO:I = 0x2

.field public static final LOG_LEVEL_WARN:I = 0x4

.field private static MC_ANALYTICS_DIRECTORY:Ljava/lang/String; = null

.field private static MC_DIRECTORY:Ljava/lang/String; = null

.field private static MC_MAX_ANALYTICS_FILES:I = 0x0

.field private static MC_MAX_EVENTS_PER_FILE:I = 0x0

.field static final PREFS_CONFIG:Ljava/lang/String; = ".DMConfig"

.field public static final PUSH_MESSAGE_INTERVAL:I = 0xf0

.field static final PUSH_MESSAGE_TO_SERVER:I = 0x186a1

.field private static SYNC_ERROR:I

.field private static SYNC_READY:I

.field private static SYNC_RUNNING:I

.field private static applicationInfo:Landroid/content/pm/ApplicationInfo;

.field private static final controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

.field private static currentFile:Ljava/io/File;

.field private static fileCreated:Z

.field private static isInitialized:Z

.field private static loggingEvent:Z

.field static mSyncHandler:Landroid/os/Handler;

.field private static numLinesWritten:I

.field private static packageName:Ljava/lang/String;

.field private static syncContents:Ljava/lang/String;

.field private static syncStatus:I


# instance fields
.field private analyticsServer:Ljava/lang/String;

.field private androidId:Ljava/lang/String;

.field private androidVersion:Ljava/lang/String;

.field private applicationId:Ljava/lang/String;

.field private applicationVersion:Ljava/lang/String;

.field private autoplay:Ljava/util/HashMap;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Ljava/util/HashMap",
            "<",
            "Ljava/lang/String;",
            "Ljava/lang/Boolean;",
            ">;"
        }
    .end annotation
.end field

.field private configServer:Ljava/lang/String;

.field private connectionType:Ljava/lang/String;

.field private context:Landroid/content/Context;

.field private deviceHardwareModel:Ljava/lang/String;

.field private deviceId:Ljava/lang/String;

.field private deviceModel:Ljava/lang/String;

.field private enabled:Ljava/util/HashMap;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Ljava/util/HashMap",
            "<",
            "Ljava/lang/String;",
            "Ljava/lang/Boolean;",
            ">;"
        }
    .end annotation
.end field

.field private haveLocationPermission:Z

.field private haveNetworkStatePermission:Z

.field private idleTimeout:I

.field private isInSession:Z

.field private isNewUser:Z

.field private isOfflineSession:Z

.field private isTopTask:Z

.field private language:Ljava/lang/String;

.field private latitude:Ljava/lang/String;

.field private locale:Ljava/lang/String;

.field location:Lcom/doodlemobile/gamecenter/DoodleMobileLocation;

.field private locationCriteria:Landroid/location/Criteria;

.field private locationHandler:Landroid/os/Handler;

.field private logLevel:I

.field private longitude:Ljava/lang/String;

.field private mcc:Ljava/lang/String;

.field private mnc:Ljava/lang/String;

.field private pollTime:I

.field private previousDeviceId:Ljava/lang/String;

.field private refreshTime:Ljava/util/HashMap;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Ljava/util/HashMap",
            "<",
            "Ljava/lang/String;",
            "Ljava/lang/Long;",
            ">;"
        }
    .end annotation
.end field

.field private remoteConfigSet:I

.field private session:Lorg/json/JSONObject;

.field private sessionEndTime:J

.field private sessionPollingTimer:Ljava/util/Timer;

.field private sessionStartTime:J

.field private sharedPrefs:Landroid/content/SharedPreferences;

.field private totalIdleTime:J

.field private userAgent:Ljava/lang/String;


# direct methods
.method static constructor <clinit>()V
    .locals 3

    .prologue
    const/4 v2, 0x0

    const/4 v1, 0x0

    .line 70
    const-string v0, "doodlemobile"

    sput-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->MC_DIRECTORY:Ljava/lang/String;

    .line 72
    const-string v0, "analytics"

    sput-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->MC_ANALYTICS_DIRECTORY:Ljava/lang/String;

    .line 74
    const/16 v0, 0x64

    sput v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->MC_MAX_ANALYTICS_FILES:I

    .line 76
    const/4 v0, 0x5

    sput v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->MC_MAX_EVENTS_PER_FILE:I

    .line 104
    sput-object v2, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->currentFile:Ljava/io/File;

    .line 106
    sput-boolean v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->fileCreated:Z

    .line 108
    sput-boolean v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->loggingEvent:Z

    .line 110
    sput v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->numLinesWritten:I

    .line 112
    sput-object v2, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->syncContents:Ljava/lang/String;

    .line 114
    sput v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->syncStatus:I

    .line 116
    sput v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->SYNC_READY:I

    .line 118
    const/4 v0, 0x1

    sput v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->SYNC_RUNNING:I

    .line 120
    const/4 v0, -0x1

    sput v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->SYNC_ERROR:I

    .line 184
    new-instance v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    invoke-direct {v0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;-><init>()V

    sput-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    .line 186
    sput-boolean v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isInitialized:Z

    .line 188
    const-string v0, ""

    sput-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->packageName:Ljava/lang/String;

    .line 190
    sput-object v2, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationInfo:Landroid/content/pm/ApplicationInfo;

    .line 771
    new-instance v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$3;

    invoke-direct {v0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$3;-><init>()V

    sput-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->mSyncHandler:Landroid/os/Handler;

    return-void
.end method

.method public constructor <init>()V
    .locals 7

    .prologue
    const/4 v5, 0x0

    const-wide/16 v3, 0x0

    const/4 v2, 0x0

    const-string v6, "http://data.doodlemobile.com:8080/dmdata/ReceiveServlet"

    const-string v1, "null"

    .line 56
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    .line 78
    iput-object v5, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sharedPrefs:Landroid/content/SharedPreferences;

    .line 80
    new-instance v0, Lorg/json/JSONObject;

    invoke-direct {v0}, Lorg/json/JSONObject;-><init>()V

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->session:Lorg/json/JSONObject;

    .line 88
    new-instance v0, Ljava/util/HashMap;

    invoke-direct {v0}, Ljava/util/HashMap;-><init>()V

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->enabled:Ljava/util/HashMap;

    .line 90
    new-instance v0, Ljava/util/HashMap;

    invoke-direct {v0}, Ljava/util/HashMap;-><init>()V

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->refreshTime:Ljava/util/HashMap;

    .line 92
    new-instance v0, Ljava/util/HashMap;

    invoke-direct {v0}, Ljava/util/HashMap;-><init>()V

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->autoplay:Ljava/util/HashMap;

    .line 94
    const-string v0, "http://data.doodlemobile.com:8080/dmdata/ReceiveServlet"

    iput-object v6, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->configServer:Ljava/lang/String;

    .line 96
    const-string v0, "http://data.doodlemobile.com:8080/dmdata/ReceiveServlet"

    iput-object v6, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->analyticsServer:Ljava/lang/String;

    .line 98
    const/16 v0, 0x7530

    iput v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->pollTime:I

    .line 100
    const v0, 0x1d4c0

    iput v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->idleTimeout:I

    .line 102
    iput v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->remoteConfigSet:I

    .line 122
    iput-boolean v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isOfflineSession:Z

    .line 124
    iput-boolean v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isInSession:Z

    .line 126
    iput-boolean v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isTopTask:Z

    .line 128
    iput-wide v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sessionStartTime:J

    .line 130
    iput-wide v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sessionEndTime:J

    .line 132
    iput-wide v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->totalIdleTime:J

    .line 134
    new-instance v0, Ljava/util/Timer;

    invoke-direct {v0}, Ljava/util/Timer;-><init>()V

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sessionPollingTimer:Ljava/util/Timer;

    .line 144
    iput-object v5, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->previousDeviceId:Ljava/lang/String;

    .line 150
    const-string v0, "null"

    iput-object v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->connectionType:Ljava/lang/String;

    .line 154
    new-instance v0, Lcom/doodlemobile/gamecenter/DoodleMobileLocation;

    invoke-direct {v0}, Lcom/doodlemobile/gamecenter/DoodleMobileLocation;-><init>()V

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->location:Lcom/doodlemobile/gamecenter/DoodleMobileLocation;

    .line 156
    const-string v0, "null"

    iput-object v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->latitude:Ljava/lang/String;

    .line 158
    const-string v0, "null"

    iput-object v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->longitude:Ljava/lang/String;

    .line 160
    const-string v0, "null"

    iput-object v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->locale:Ljava/lang/String;

    .line 162
    const-string v0, "null"

    iput-object v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->language:Ljava/lang/String;

    .line 164
    const-string v0, "null"

    iput-object v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->mcc:Ljava/lang/String;

    .line 166
    const-string v0, "null"

    iput-object v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->mnc:Ljava/lang/String;

    .line 168
    const-string v0, ""

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->userAgent:Ljava/lang/String;

    .line 170
    const-string v0, "null"

    iput-object v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationId:Ljava/lang/String;

    .line 172
    const/16 v0, 0x10

    iput v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->logLevel:I

    .line 174
    iput-boolean v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->haveLocationPermission:Z

    .line 178
    iput-boolean v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->haveNetworkStatePermission:Z

    .line 180
    iput-boolean v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isNewUser:Z

    .line 1045
    return-void
.end method

.method private OpenAnalyticsFile()Z
    .locals 7

    .prologue
    const/4 v6, 0x1

    const/4 v5, 0x0

    const-string v0, "DoodleMobileAnaylise"

    const-string v0, "/"

    const-string v0, "UTF-8"

    .line 720
    sput v6, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->numLinesWritten:I

    .line 721
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    invoke-virtual {v0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->updateSession()V

    .line 724
    :try_start_0
    new-instance v0, Lorg/json/JSONObject;

    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    iget-object v1, v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->session:Lorg/json/JSONObject;

    const/4 v2, 0x3

    new-array v2, v2, [Ljava/lang/String;

    const/4 v3, 0x0

    const-string v4, "ll"

    aput-object v4, v2, v3

    const/4 v3, 0x1

    const-string v4, "g"

    aput-object v4, v2, v3

    const/4 v3, 0x2

    const-string v4, "id"

    aput-object v4, v2, v3

    invoke-direct {v0, v1, v2}, Lorg/json/JSONObject;-><init>(Lorg/json/JSONObject;[Ljava/lang/String;)V

    .line 726
    const-string v1, "a"

    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getApplicationId()Ljava/lang/String;

    move-result-object v2

    const-string v3, "UTF-8"

    invoke-static {v2, v3}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 727
    const-string v1, "p"

    const-string v2, "android"

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 728
    const-string v1, "m"

    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getMobclixVersion()Ljava/lang/String;

    move-result-object v2

    invoke-static {v2}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 729
    const-string v1, "v"

    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getApplicationVersion()Ljava/lang/String;

    move-result-object v2

    const-string v3, "UTF-8"

    invoke-static {v2, v3}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 730
    const-string v1, "d"

    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getDeviceId()Ljava/lang/String;

    move-result-object v2

    const-string v3, "UTF-8"

    invoke-static {v2, v3}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 731
    const-string v1, "dm"

    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getDeviceModel()Ljava/lang/String;

    move-result-object v2

    const-string v3, "UTF-8"

    invoke-static {v2, v3}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 732
    const-string v1, "dv"

    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getAndroidVersion()Ljava/lang/String;

    move-result-object v2

    const-string v3, "UTF-8"

    invoke-static {v2, v3}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 733
    const-string v1, "hwdm"

    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getDeviceHardwareModel()Ljava/lang/String;

    move-result-object v2

    const-string v3, "UTF-8"

    invoke-static {v2, v3}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 735
    const-string v1, "m"

    const-string v2, "2.3"

    const-string v3, "UTF-8"

    invoke-static {v2, v3}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 736
    const-string v1, "lg"

    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getLanguage()Ljava/lang/String;

    move-result-object v2

    const-string v3, "UTF-8"

    invoke-static {v2, v3}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 737
    const-string v1, "lo"

    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getLocale()Ljava/lang/String;

    move-result-object v2

    const-string v3, "UTF-8"

    invoke-static {v2, v3}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 738
    const-string v1, "pn"

    sget-object v2, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->packageName:Ljava/lang/String;

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 740
    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    iget-object v1, v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->context:Landroid/content/Context;

    sget-object v2, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->MC_DIRECTORY:Ljava/lang/String;

    const/4 v3, 0x0

    invoke-virtual {v1, v2, v3}, Landroid/content/Context;->getDir(Ljava/lang/String;I)Ljava/io/File;

    move-result-object v1

    .line 741
    new-instance v2, Ljava/io/File;

    new-instance v3, Ljava/lang/StringBuilder;

    invoke-direct {v3}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {v1}, Ljava/io/File;->getAbsolutePath()Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v3, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v3, "/"

    invoke-virtual {v1, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    sget-object v3, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->MC_ANALYTICS_DIRECTORY:Ljava/lang/String;

    invoke-virtual {v1, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-direct {v2, v1}, Ljava/io/File;-><init>(Ljava/lang/String;)V

    .line 743
    invoke-virtual {v2}, Ljava/io/File;->mkdir()Z

    .line 744
    const-string v1, "DoodleMobileAnaylise"

    const-string v3, "mkdir"

    invoke-static {v1, v3}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    .line 745
    invoke-virtual {v2}, Ljava/io/File;->listFiles()[Ljava/io/File;

    move-result-object v1

    array-length v1, v1

    sget v3, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->MC_MAX_ANALYTICS_FILES:I

    if-lt v1, v3, :cond_0

    .line 746
    const-string v0, "DoodleMobileAnaylise"

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {v2}, Ljava/io/File;->listFiles()[Ljava/io/File;

    move-result-object v2

    array-length v2, v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v2, "  "

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    sget v2, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->MC_MAX_ANALYTICS_FILES:I

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-static {v0, v1}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    move v0, v5

    .line 768
    :goto_0
    return v0

    .line 751
    :cond_0
    new-instance v1, Ljava/io/File;

    new-instance v3, Ljava/lang/StringBuilder;

    invoke-direct {v3}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {v2}, Ljava/io/File;->getAbsoluteFile()Ljava/io/File;

    move-result-object v2

    invoke-virtual {v3, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object v2

    const-string v3, "/"

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-static {}, Ljava/lang/System;->currentTimeMillis()J

    move-result-wide v3

    invoke-virtual {v2, v3, v4}, Ljava/lang/StringBuilder;->append(J)Ljava/lang/StringBuilder;

    move-result-object v2

    const-string v3, ".log"

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v2

    invoke-direct {v1, v2}, Ljava/io/File;-><init>(Ljava/lang/String;)V

    sput-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->currentFile:Ljava/io/File;

    .line 754
    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->currentFile:Ljava/io/File;

    invoke-virtual {v1}, Ljava/io/File;->createNewFile()Z

    .line 756
    new-instance v1, Ljava/io/FileOutputStream;

    sget-object v2, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->currentFile:Ljava/io/File;

    invoke-direct {v1, v2}, Ljava/io/FileOutputStream;-><init>(Ljava/io/File;)V

    .line 758
    const-string v2, "[{\"hb\":"

    invoke-virtual {v2}, Ljava/lang/String;->getBytes()[B

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/io/FileOutputStream;->write([B)V

    .line 759
    invoke-virtual {v0}, Lorg/json/JSONObject;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/String;->getBytes()[B

    move-result-object v0

    invoke-virtual {v1, v0}, Ljava/io/FileOutputStream;->write([B)V

    .line 760
    const-string v0, ",\"ev\":["

    invoke-virtual {v0}, Ljava/lang/String;->getBytes()[B

    move-result-object v0

    invoke-virtual {v1, v0}, Ljava/io/FileOutputStream;->write([B)V

    .line 761
    invoke-virtual {v1}, Ljava/io/FileOutputStream;->close()V
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    .line 767
    sput-boolean v6, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->fileCreated:Z

    move v0, v6

    .line 768
    goto :goto_0

    .line 763
    :catch_0
    move-exception v0

    .line 764
    invoke-virtual {v0}, Ljava/lang/Exception;->printStackTrace()V

    move v0, v5

    .line 765
    goto :goto_0
.end method

.method static synthetic access$002(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;Ljava/lang/String;)Ljava/lang/String;
    .locals 0

    .prologue
    .line 56
    iput-object p1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->latitude:Ljava/lang/String;

    return-object p1
.end method

.method static synthetic access$1000()Ljava/io/File;
    .locals 1

    .prologue
    .line 56
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->currentFile:Ljava/io/File;

    return-object v0
.end method

.method static synthetic access$102(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;Ljava/lang/String;)Ljava/lang/String;
    .locals 0

    .prologue
    .line 56
    iput-object p1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->longitude:Ljava/lang/String;

    return-object p1
.end method

.method static synthetic access$1100()I
    .locals 1

    .prologue
    .line 56
    sget v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->numLinesWritten:I

    return v0
.end method

.method static synthetic access$1102(I)I
    .locals 0

    .prologue
    .line 56
    sput p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->numLinesWritten:I

    return p0
.end method

.method static synthetic access$1112(I)I
    .locals 1

    .prologue
    .line 56
    sget v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->numLinesWritten:I

    add-int/2addr v0, p0

    sput v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->numLinesWritten:I

    return v0
.end method

.method static synthetic access$1200()I
    .locals 1

    .prologue
    .line 56
    sget v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->MC_MAX_EVENTS_PER_FILE:I

    return v0
.end method

.method static synthetic access$1400(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;)Landroid/content/Context;
    .locals 1

    .prologue
    .line 56
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->context:Landroid/content/Context;

    return-object v0
.end method

.method static synthetic access$1500(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;)Ljava/lang/String;
    .locals 1

    .prologue
    .line 56
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationVersion:Ljava/lang/String;

    return-object v0
.end method

.method static synthetic access$1502(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;Ljava/lang/String;)Ljava/lang/String;
    .locals 0

    .prologue
    .line 56
    iput-object p1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationVersion:Ljava/lang/String;

    return-object p1
.end method

.method static synthetic access$1600(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;)Landroid/location/Criteria;
    .locals 1

    .prologue
    .line 56
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->locationCriteria:Landroid/location/Criteria;

    return-object v0
.end method

.method static synthetic access$1602(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;Landroid/location/Criteria;)Landroid/location/Criteria;
    .locals 0

    .prologue
    .line 56
    iput-object p1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->locationCriteria:Landroid/location/Criteria;

    return-object p1
.end method

.method static synthetic access$1702(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;Z)Z
    .locals 0

    .prologue
    .line 56
    iput-boolean p1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->haveLocationPermission:Z

    return p1
.end method

.method static synthetic access$1802(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;Z)Z
    .locals 0

    .prologue
    .line 56
    iput-boolean p1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->haveNetworkStatePermission:Z

    return p1
.end method

.method static synthetic access$1900(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;Z)V
    .locals 0

    .prologue
    .line 56
    invoke-direct {p0, p1}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->handleSessionStatus(Z)V

    return-void
.end method

.method static synthetic access$200(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;)V
    .locals 0

    .prologue
    .line 56
    invoke-direct {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->updateLocation()V

    return-void
.end method

.method static synthetic access$2000()I
    .locals 1

    .prologue
    .line 56
    sget v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->SYNC_RUNNING:I

    return v0
.end method

.method static synthetic access$2100()Ljava/lang/String;
    .locals 1

    .prologue
    .line 56
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->MC_DIRECTORY:Ljava/lang/String;

    return-object v0
.end method

.method static synthetic access$2200()Ljava/lang/String;
    .locals 1

    .prologue
    .line 56
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->MC_ANALYTICS_DIRECTORY:Ljava/lang/String;

    return-object v0
.end method

.method static synthetic access$2300()Ljava/lang/String;
    .locals 1

    .prologue
    .line 56
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->syncContents:Ljava/lang/String;

    return-object v0
.end method

.method static synthetic access$2302(Ljava/lang/String;)Ljava/lang/String;
    .locals 0

    .prologue
    .line 56
    sput-object p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->syncContents:Ljava/lang/String;

    return-object p0
.end method

.method static synthetic access$2400(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;)Ljava/lang/String;
    .locals 1

    .prologue
    .line 56
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->analyticsServer:Ljava/lang/String;

    return-object v0
.end method

.method static synthetic access$2500()I
    .locals 1

    .prologue
    .line 56
    sget v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->SYNC_ERROR:I

    return v0
.end method

.method static synthetic access$400()Z
    .locals 1

    .prologue
    .line 56
    sget-boolean v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->loggingEvent:Z

    return v0
.end method

.method static synthetic access$402(Z)Z
    .locals 0

    .prologue
    .line 56
    sput-boolean p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->loggingEvent:Z

    return p0
.end method

.method static synthetic access$500()I
    .locals 1

    .prologue
    .line 56
    sget v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->syncStatus:I

    return v0
.end method

.method static synthetic access$502(I)I
    .locals 0

    .prologue
    .line 56
    sput p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->syncStatus:I

    return p0
.end method

.method static synthetic access$600()I
    .locals 1

    .prologue
    .line 56
    sget v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->SYNC_READY:I

    return v0
.end method

.method static synthetic access$700()Z
    .locals 1

    .prologue
    .line 56
    sget-boolean v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->fileCreated:Z

    return v0
.end method

.method static synthetic access$702(Z)Z
    .locals 0

    .prologue
    .line 56
    sput-boolean p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->fileCreated:Z

    return p0
.end method

.method static synthetic access$800(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;)Z
    .locals 1

    .prologue
    .line 56
    invoke-direct {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->OpenAnalyticsFile()Z

    move-result v0

    return v0
.end method

.method static synthetic access$900()Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;
    .locals 1

    .prologue
    .line 56
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    return-object v0
.end method

.method static addPref(Ljava/lang/String;Ljava/lang/String;)V
    .locals 1

    .prologue
    .line 333
    :try_start_0
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    iget-object v0, v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sharedPrefs:Landroid/content/SharedPreferences;

    invoke-interface {v0}, Landroid/content/SharedPreferences;->edit()Landroid/content/SharedPreferences$Editor;

    move-result-object v0

    .line 334
    invoke-interface {v0, p0, p1}, Landroid/content/SharedPreferences$Editor;->putString(Ljava/lang/String;Ljava/lang/String;)Landroid/content/SharedPreferences$Editor;

    .line 335
    invoke-interface {v0}, Landroid/content/SharedPreferences$Editor;->commit()Z
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    .line 338
    :goto_0
    return-void

    .line 336
    :catch_0
    move-exception v0

    goto :goto_0
.end method

.method static addPref(Ljava/util/Map;)V
    .locals 3
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Ljava/util/Map",
            "<",
            "Ljava/lang/String;",
            "Ljava/lang/String;",
            ">;)V"
        }
    .end annotation

    .prologue
    .line 342
    :try_start_0
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    iget-object v0, v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sharedPrefs:Landroid/content/SharedPreferences;

    invoke-interface {v0}, Landroid/content/SharedPreferences;->edit()Landroid/content/SharedPreferences$Editor;

    move-result-object v1

    .line 343
    invoke-interface {p0}, Ljava/util/Map;->entrySet()Ljava/util/Set;

    move-result-object v0

    invoke-interface {v0}, Ljava/util/Set;->iterator()Ljava/util/Iterator;

    move-result-object v2

    .line 344
    :goto_0
    invoke-interface {v2}, Ljava/util/Iterator;->hasNext()Z

    move-result v0

    if-eqz v0, :cond_0

    .line 345
    invoke-interface {v2}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object p0

    check-cast p0, Ljava/util/Map$Entry;

    .line 346
    invoke-interface {p0}, Ljava/util/Map$Entry;->getKey()Ljava/lang/Object;

    move-result-object v0

    check-cast v0, Ljava/lang/String;

    invoke-interface {p0}, Ljava/util/Map$Entry;->getValue()Ljava/lang/Object;

    move-result-object p0

    check-cast p0, Ljava/lang/String;

    invoke-interface {v1, v0, p0}, Landroid/content/SharedPreferences$Editor;->putString(Ljava/lang/String;Ljava/lang/String;)Landroid/content/SharedPreferences$Editor;

    goto :goto_0

    .line 349
    :catch_0
    move-exception v0

    .line 351
    :goto_1
    return-void

    .line 348
    :cond_0
    invoke-interface {v1}, Landroid/content/SharedPreferences$Editor;->commit()Z
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    goto :goto_1
.end method

.method static clearPref()V
    .locals 1

    .prologue
    .line 364
    :try_start_0
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    iget-object v0, v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sharedPrefs:Landroid/content/SharedPreferences;

    invoke-interface {v0}, Landroid/content/SharedPreferences;->edit()Landroid/content/SharedPreferences$Editor;

    move-result-object v0

    .line 365
    invoke-interface {v0}, Landroid/content/SharedPreferences$Editor;->clear()Landroid/content/SharedPreferences$Editor;

    .line 366
    invoke-interface {v0}, Landroid/content/SharedPreferences$Editor;->commit()Z
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    .line 369
    :goto_0
    return-void

    .line 367
    :catch_0
    move-exception v0

    goto :goto_0
.end method

.method private createNewSession()V
    .locals 7

    .prologue
    const-wide/16 v5, 0x0

    const/4 v4, 0x1

    .line 804
    invoke-static {}, Ljava/lang/System;->currentTimeMillis()J

    move-result-wide v0

    .line 805
    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    iget-object v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceId:Ljava/lang/String;

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, v0, v1}, Ljava/lang/StringBuilder;->append(J)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v2

    invoke-static {v2}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sha1(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    .line 807
    iput-boolean v4, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isTopTask:Z

    .line 808
    iput-wide v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sessionStartTime:J

    .line 809
    iput-wide v5, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sessionEndTime:J

    .line 810
    iput-wide v5, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->totalIdleTime:J

    .line 811
    iput-boolean v4, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isInSession:Z

    .line 813
    :try_start_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->session:Lorg/json/JSONObject;

    const-string v1, "id"

    const-string v3, "UTF-8"

    invoke-static {v2, v3}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    .line 817
    :goto_0
    const/4 v0, 0x0

    iput v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->remoteConfigSet:I

    .line 818
    new-instance v0, Ljava/lang/Thread;

    new-instance v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$FetchRemoteConfig;

    const/4 v2, 0x0

    invoke-direct {v1, p0, v2}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$FetchRemoteConfig;-><init>(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$1;)V

    invoke-direct {v0, v1}, Ljava/lang/Thread;-><init>(Ljava/lang/Runnable;)V

    .line 819
    invoke-virtual {v0}, Ljava/lang/Thread;->start()V

    .line 820
    return-void

    .line 814
    :catch_0
    move-exception v0

    .line 815
    const-string v0, "DoodleMobileAnaylise"

    const-string v1, "static sync 2"

    invoke-static {v0, v1}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    goto :goto_0
.end method

.method private endSession()V
    .locals 6

    .prologue
    const-string v0, "totalSessionTime"

    const-string v0, "totalIdleTime"

    const-string v0, "offlineSessions"

    .line 852
    :try_start_0
    iget-boolean v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isInSession:Z

    if-nez v0, :cond_0

    .line 893
    :goto_0
    return-void

    .line 854
    :cond_0
    iget-wide v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sessionEndTime:J

    iget-wide v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sessionStartTime:J

    sub-long/2addr v0, v2

    .line 856
    const-string v2, "totalSessionTime"

    invoke-static {v2}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->hasPref(Ljava/lang/String;)Z
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    move-result v2

    if-eqz v2, :cond_1

    .line 858
    :try_start_1
    const-string v2, "totalSessionTime"

    invoke-static {v2}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getPref(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-static {v2}, Ljava/lang/Long;->parseLong(Ljava/lang/String;)J
    :try_end_1
    .catch Ljava/lang/Exception; {:try_start_1 .. :try_end_1} :catch_3

    move-result-wide v2

    add-long/2addr v0, v2

    .line 861
    :cond_1
    :goto_1
    :try_start_2
    const-string v2, "totalIdleTime"

    invoke-static {v2}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->hasPref(Ljava/lang/String;)Z
    :try_end_2
    .catch Ljava/lang/Exception; {:try_start_2 .. :try_end_2} :catch_0

    move-result v2

    if-eqz v2, :cond_2

    .line 863
    :try_start_3
    iget-wide v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->totalIdleTime:J

    const-string v4, "totalIdleTime"

    invoke-static {v4}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getPref(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-static {v4}, Ljava/lang/Long;->parseLong(Ljava/lang/String;)J

    move-result-wide v4

    add-long/2addr v2, v4

    iput-wide v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->totalIdleTime:J
    :try_end_3
    .catch Ljava/lang/Exception; {:try_start_3 .. :try_end_3} :catch_2

    .line 867
    :cond_2
    :goto_2
    :try_start_4
    new-instance v2, Ljava/util/HashMap;

    invoke-direct {v2}, Ljava/util/HashMap;-><init>()V

    .line 868
    const-string v3, "totalSessionTime"

    invoke-static {v0, v1}, Ljava/lang/Long;->toString(J)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {v2, v3, v0}, Ljava/util/HashMap;->put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;

    .line 869
    const-string v0, "totalIdleTime"

    iget-wide v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->totalIdleTime:J

    invoke-static {v3, v4}, Ljava/lang/Long;->toString(J)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v2, v0, v1}, Ljava/util/HashMap;->put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;

    .line 872
    iget-boolean v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isOfflineSession:Z

    if-eqz v0, :cond_4

    .line 873
    const/4 v0, 0x1

    .line 874
    const-string v1, "offlineSessions"

    invoke-static {v1}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->hasPref(Ljava/lang/String;)Z
    :try_end_4
    .catch Ljava/lang/Exception; {:try_start_4 .. :try_end_4} :catch_0

    move-result v1

    if-eqz v1, :cond_3

    .line 876
    :try_start_5
    const-string v1, "offlineSessions"

    invoke-static {v1}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getPref(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-static {v1}, Ljava/lang/Integer;->parseInt(Ljava/lang/String;)I
    :try_end_5
    .catch Ljava/lang/Exception; {:try_start_5 .. :try_end_5} :catch_1

    move-result v1

    add-int/2addr v0, v1

    .line 880
    :cond_3
    :goto_3
    :try_start_6
    const-string v1, "offlineSessions"

    int-to-long v3, v0

    invoke-static {v3, v4}, Ljava/lang/Long;->toString(J)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {v2, v1, v0}, Ljava/util/HashMap;->put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;

    .line 884
    :cond_4
    invoke-static {v2}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->addPref(Ljava/util/Map;)V

    .line 886
    const/4 v0, 0x0

    iput-boolean v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isInSession:Z

    .line 887
    const/4 v0, 0x0

    iput-boolean v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isTopTask:Z

    .line 888
    const-wide/16 v0, 0x0

    iput-wide v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sessionStartTime:J

    .line 889
    const-wide/16 v0, 0x0

    iput-wide v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sessionEndTime:J

    .line 890
    const-wide/16 v0, 0x0

    iput-wide v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->totalIdleTime:J
    :try_end_6
    .catch Ljava/lang/Exception; {:try_start_6 .. :try_end_6} :catch_0

    goto :goto_0

    .line 891
    :catch_0
    move-exception v0

    goto/16 :goto_0

    .line 878
    :catch_1
    move-exception v1

    goto :goto_3

    .line 865
    :catch_2
    move-exception v2

    goto :goto_2

    .line 859
    :catch_3
    move-exception v2

    goto :goto_1
.end method

.method static getAllPref()Ljava/util/HashMap;
    .locals 1
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "()",
            "Ljava/util/HashMap",
            "<",
            "Ljava/lang/String;",
            "Ljava/lang/String;",
            ">;"
        }
    .end annotation

    .prologue
    .line 309
    :try_start_0
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    iget-object v0, v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sharedPrefs:Landroid/content/SharedPreferences;

    invoke-interface {v0}, Landroid/content/SharedPreferences;->getAll()Ljava/util/Map;

    move-result-object v0

    check-cast v0, Ljava/util/HashMap;
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    .line 312
    :goto_0
    return-object v0

    .line 310
    :catch_0
    move-exception v0

    .line 312
    new-instance v0, Ljava/util/HashMap;

    invoke-direct {v0}, Ljava/util/HashMap;-><init>()V

    goto :goto_0
.end method

.method static getCookieStringFromCookieManager(Ljava/lang/String;)Ljava/lang/String;
    .locals 1

    .prologue
    .line 901
    :try_start_0
    invoke-static {}, Landroid/webkit/CookieManager;->getInstance()Landroid/webkit/CookieManager;

    move-result-object v0

    .line 902
    invoke-virtual {v0, p0}, Landroid/webkit/CookieManager;->getCookie(Ljava/lang/String;)Ljava/lang/String;
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    move-result-object v0

    .line 905
    :goto_0
    return-object v0

    .line 903
    :catch_0
    move-exception v0

    .line 905
    const-string v0, ""

    goto :goto_0
.end method

.method public static getInstance()Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;
    .locals 1

    .prologue
    .line 515
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    return-object v0
.end method

.method static getPref(Ljava/lang/String;)Ljava/lang/String;
    .locals 3

    .prologue
    const-string v2, ""

    .line 317
    :try_start_0
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    iget-object v0, v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sharedPrefs:Landroid/content/SharedPreferences;

    const-string v1, ""

    invoke-interface {v0, p0, v1}, Landroid/content/SharedPreferences;->getString(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    move-result-object v0

    .line 320
    :goto_0
    return-object v0

    .line 318
    :catch_0
    move-exception v0

    .line 320
    const-string v0, ""

    move-object v0, v2

    goto :goto_0
.end method

.method private declared-synchronized handleSessionStatus(Z)V
    .locals 6

    .prologue
    .line 824
    monitor-enter p0

    :try_start_0
    invoke-static {}, Ljava/lang/System;->currentTimeMillis()J

    move-result-wide v0

    .line 825
    if-eqz p1, :cond_3

    .line 826
    iget-boolean v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isTopTask:Z
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    if-eqz v2, :cond_1

    .line 848
    :cond_0
    :goto_0
    monitor-exit p0

    return-void

    .line 829
    :cond_1
    :try_start_1
    iget-boolean v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isInSession:Z

    if-nez v2, :cond_2

    .line 830
    invoke-direct {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->createNewSession()V
    :try_end_1
    .catch Ljava/lang/Exception; {:try_start_1 .. :try_end_1} :catch_0
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    goto :goto_0

    .line 845
    :catch_0
    move-exception v0

    .line 846
    :try_start_2
    const-string v0, "DoodleMobileAnaylise"

    const-string v1, "static sync 3"

    invoke-static {v0, v1}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    goto :goto_0

    .line 824
    :catchall_0
    move-exception v0

    monitor-exit p0

    throw v0

    .line 832
    :cond_2
    :try_start_3
    iget-wide v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->totalIdleTime:J

    iget-wide v4, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sessionEndTime:J

    sub-long/2addr v0, v4

    add-long/2addr v0, v2

    iput-wide v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->totalIdleTime:J

    .line 833
    const/4 v0, 0x1

    iput-boolean v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isTopTask:Z

    goto :goto_0

    .line 836
    :cond_3
    iget-boolean v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isTopTask:Z

    if-nez v2, :cond_4

    .line 837
    iget-wide v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sessionEndTime:J

    sub-long/2addr v0, v2

    iget v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->idleTimeout:I

    int-to-long v2, v2

    cmp-long v0, v0, v2

    if-lez v0, :cond_0

    iget-boolean v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isInSession:Z

    if-eqz v0, :cond_0

    .line 839
    invoke-direct {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->endSession()V

    goto :goto_0

    .line 841
    :cond_4
    iput-wide v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sessionEndTime:J

    .line 842
    const/4 v0, 0x0

    iput-boolean v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isTopTask:Z

    .line 843
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->location:Lcom/doodlemobile/gamecenter/DoodleMobileLocation;

    invoke-virtual {v0}, Lcom/doodlemobile/gamecenter/DoodleMobileLocation;->stopLocation()V
    :try_end_3
    .catch Ljava/lang/Exception; {:try_start_3 .. :try_end_3} :catch_0
    .catchall {:try_start_3 .. :try_end_3} :catchall_0

    goto :goto_0
.end method

.method static hasPref(Ljava/lang/String;)Z
    .locals 1

    .prologue
    .line 325
    :try_start_0
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    iget-object v0, v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sharedPrefs:Landroid/content/SharedPreferences;

    invoke-interface {v0, p0}, Landroid/content/SharedPreferences;->contains(Ljava/lang/String;)Z
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    move-result v0

    .line 328
    :goto_0
    return v0

    .line 326
    :catch_0
    move-exception v0

    .line 328
    const/4 v0, 0x0

    goto :goto_0
.end method

.method private initialize(Landroid/app/Activity;Ljava/lang/String;I)V
    .locals 7

    .prologue
    const/4 v6, 0x0

    const/4 v5, 0x0

    const-string v4, ""

    const-string v3, "null"

    .line 455
    iput-object p1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->context:Landroid/content/Context;

    .line 456
    iput-object p2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationId:Ljava/lang/String;

    .line 457
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationId:Ljava/lang/String;

    if-eqz v0, :cond_0

    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationId:Ljava/lang/String;

    const-string v1, ""

    invoke-virtual {v0, v4}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v0

    if-eqz v0, :cond_1

    .line 458
    :cond_0
    const-string v0, "null"

    iput-object v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationId:Ljava/lang/String;

    .line 459
    :cond_1
    sget-object v0, Landroid/os/Build$VERSION;->RELEASE:Ljava/lang/String;

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->androidVersion:Ljava/lang/String;

    .line 460
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->androidVersion:Ljava/lang/String;

    if-eqz v0, :cond_2

    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->androidVersion:Ljava/lang/String;

    const-string v1, ""

    invoke-virtual {v0, v4}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v0

    if-eqz v0, :cond_3

    .line 461
    :cond_2
    const-string v0, "null"

    iput-object v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->androidVersion:Ljava/lang/String;

    .line 465
    :cond_3
    :try_start_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->context:Landroid/content/Context;

    const-string v1, "phone"

    invoke-virtual {v0, v1}, Landroid/content/Context;->getSystemService(Ljava/lang/String;)Ljava/lang/Object;

    move-result-object v0

    check-cast v0, Landroid/telephony/TelephonyManager;
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    .line 469
    :goto_0
    invoke-virtual {p1}, Landroid/app/Activity;->getContentResolver()Landroid/content/ContentResolver;

    move-result-object v1

    const-string v2, "android_id"

    invoke-static {v1, v2}, Landroid/provider/Settings$System;->getString(Landroid/content/ContentResolver;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    iput-object v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->androidId:Ljava/lang/String;

    .line 471
    iget-object v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->androidId:Ljava/lang/String;

    if-eqz v1, :cond_4

    iget-object v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->androidId:Ljava/lang/String;

    const-string v2, ""

    invoke-virtual {v1, v4}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v1

    if-eqz v1, :cond_5

    .line 472
    :cond_4
    const-string v1, "null"

    iput-object v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->androidId:Ljava/lang/String;

    .line 474
    :cond_5
    invoke-virtual {v0}, Landroid/telephony/TelephonyManager;->getDeviceId()Ljava/lang/String;

    move-result-object v0

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceId:Ljava/lang/String;

    .line 475
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceId:Ljava/lang/String;

    if-eqz v0, :cond_6

    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceId:Ljava/lang/String;

    const-string v1, ""

    invoke-virtual {v0, v4}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v0

    if-eqz v0, :cond_7

    .line 476
    :cond_6
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->androidId:Ljava/lang/String;

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceId:Ljava/lang/String;

    .line 478
    :cond_7
    sget-object v0, Landroid/os/Build;->MODEL:Ljava/lang/String;

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceModel:Ljava/lang/String;

    .line 479
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceModel:Ljava/lang/String;

    if-eqz v0, :cond_8

    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceModel:Ljava/lang/String;

    const-string v1, ""

    invoke-virtual {v0, v4}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v0

    if-eqz v0, :cond_9

    .line 480
    :cond_8
    const-string v0, "null"

    iput-object v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceModel:Ljava/lang/String;

    .line 482
    :cond_9
    sget-object v0, Landroid/os/Build;->DEVICE:Ljava/lang/String;

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceHardwareModel:Ljava/lang/String;

    .line 483
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceHardwareModel:Ljava/lang/String;

    if-eqz v0, :cond_a

    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceHardwareModel:Ljava/lang/String;

    const-string v1, ""

    invoke-virtual {v0, v4}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v0

    if-eqz v0, :cond_b

    .line 485
    :cond_a
    const-string v0, "null"

    iput-object v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceHardwareModel:Ljava/lang/String;

    .line 488
    :cond_b
    iput p3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->logLevel:I

    .line 490
    const-string v0, "null"

    iput-object v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationVersion:Ljava/lang/String;

    .line 491
    const-string v0, "null"

    iput-object v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->longitude:Ljava/lang/String;

    .line 492
    const-string v0, "null"

    iput-object v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->latitude:Ljava/lang/String;

    .line 493
    iput-boolean v5, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->haveLocationPermission:Z

    .line 494
    iput-object v6, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->locationCriteria:Landroid/location/Criteria;

    .line 495
    iput-boolean v5, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->haveNetworkStatePermission:Z

    .line 496
    invoke-static {}, Ljava/util/Locale;->getDefault()Ljava/util/Locale;

    move-result-object v0

    invoke-virtual {v0}, Ljava/util/Locale;->getCountry()Ljava/lang/String;

    move-result-object v0

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->locale:Ljava/lang/String;

    .line 497
    invoke-static {}, Ljava/util/Locale;->getDefault()Ljava/util/Locale;

    move-result-object v0

    invoke-virtual {v0}, Ljava/util/Locale;->getLanguage()Ljava/lang/String;

    move-result-object v0

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->language:Ljava/lang/String;

    .line 499
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->context:Landroid/content/Context;

    invoke-static {v0}, Landroid/webkit/CookieSyncManager;->createInstance(Landroid/content/Context;)Landroid/webkit/CookieSyncManager;

    .line 501
    new-instance v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$2;

    invoke-direct {v0, p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$2;-><init>(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;)V

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->locationHandler:Landroid/os/Handler;

    .line 506
    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {p1}, Landroid/app/Activity;->getPackageName()Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v1, ".MCConfig"

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p1, v0, v5}, Landroid/app/Activity;->getSharedPreferences(Ljava/lang/String;I)Landroid/content/SharedPreferences;

    move-result-object v0

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sharedPrefs:Landroid/content/SharedPreferences;

    .line 509
    iget v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->pollTime:I

    iget v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->idleTimeout:I

    invoke-static {v0, v1}, Ljava/lang/Math;->min(II)I

    move-result v0

    iput v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->pollTime:I

    .line 510
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sessionPollingTimer:Ljava/util/Timer;

    new-instance v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$SessionPolling;

    invoke-direct {v1, p0, v6}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$SessionPolling;-><init>(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$1;)V

    const-wide/16 v2, 0x0

    iget v4, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->pollTime:I

    int-to-long v4, v4

    invoke-virtual/range {v0 .. v5}, Ljava/util/Timer;->scheduleAtFixedRate(Ljava/util/TimerTask;JJ)V

    .line 512
    return-void

    .line 467
    :catch_0
    move-exception v0

    move-object v0, v6

    goto/16 :goto_0
.end method

.method public static final logEvent(ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;Z)V
    .locals 5

    .prologue
    const-string v0, "UTF-8"

    const-string v2, "DoodleMobile"

    .line 672
    sget-boolean v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isInitialized:Z

    if-nez v0, :cond_1

    .line 673
    const-string v0, "mobclix-controller"

    const-string v1, "logEvent failed - You must initialize DoodleMobileAnaylise by calling DoodleMobileAnaylise.onCreate(this)."

    invoke-static {v0, v1}, Landroid/util/Log;->v(Ljava/lang/String;Ljava/lang/String;)I

    .line 717
    :cond_0
    :goto_0
    return-void

    .line 678
    :cond_1
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    iget v0, v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->logLevel:I

    if-lt p0, v0, :cond_0

    .line 682
    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v1, ", "

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, p2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v1, ": "

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    .line 684
    sparse-switch p0, :sswitch_data_0

    .line 702
    :goto_1
    :try_start_0
    new-instance v0, Lorg/json/JSONObject;

    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    iget-object v1, v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->session:Lorg/json/JSONObject;

    const/4 v2, 0x4

    new-array v2, v2, [Ljava/lang/String;

    const/4 v3, 0x0

    const-string v4, "ts"

    aput-object v4, v2, v3

    const/4 v3, 0x1

    const-string v4, "ll"

    aput-object v4, v2, v3

    const/4 v3, 0x2

    const-string v4, "g"

    aput-object v4, v2, v3

    const/4 v3, 0x3

    const-string v4, "id"

    aput-object v4, v2, v3

    invoke-direct {v0, v1, v2}, Lorg/json/JSONObject;-><init>(Lorg/json/JSONObject;[Ljava/lang/String;)V

    .line 704
    const-string v1, "el"

    invoke-static {p0}, Ljava/lang/Integer;->toString(I)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 705
    const-string v1, "ep"

    const-string v2, "UTF-8"

    invoke-static {p1, v2}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 706
    const-string v1, "en"

    const-string v2, "UTF-8"

    invoke-static {p2, v2}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 707
    const-string v1, "ed"

    const-string v2, "UTF-8"

    invoke-static {p3, v2}, Ljava/net/URLEncoder;->encode(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 708
    const-string v1, "et"

    invoke-static {}, Ljava/lang/Thread;->currentThread()Ljava/lang/Thread;

    move-result-object v2

    invoke-virtual {v2}, Ljava/lang/Thread;->getId()J

    move-result-wide v2

    invoke-static {v2, v3}, Ljava/lang/Long;->toString(J)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 709
    const-string v1, "es"

    if-eqz p4, :cond_2

    const-string v2, "1"

    :goto_2
    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 710
    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    .line 711
    invoke-virtual {v1}, Ljava/lang/Object;->getClass()Ljava/lang/Class;

    .line 712
    new-instance v2, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$LogEvent;

    invoke-virtual {v1}, Ljava/lang/Object;->getClass()Ljava/lang/Class;

    invoke-direct {v2, v1, v0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$LogEvent;-><init>(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;Lorg/json/JSONObject;)V

    .line 713
    new-instance v0, Ljava/lang/Thread;

    invoke-direct {v0, v2}, Ljava/lang/Thread;-><init>(Ljava/lang/Runnable;)V

    .line 714
    invoke-virtual {v0}, Ljava/lang/Thread;->start()V
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    goto/16 :goto_0

    .line 715
    :catch_0
    move-exception v0

    goto/16 :goto_0

    .line 686
    :sswitch_0
    const-string v1, "DoodleMobile"

    invoke-static {v2, v0}, Landroid/util/Log;->d(Ljava/lang/String;Ljava/lang/String;)I

    goto/16 :goto_1

    .line 689
    :sswitch_1
    const-string v1, "DoodleMobile"

    invoke-static {v2, v0}, Landroid/util/Log;->i(Ljava/lang/String;Ljava/lang/String;)I

    goto/16 :goto_1

    .line 692
    :sswitch_2
    const-string v1, "DoodleMobile"

    invoke-static {v2, v0}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    goto/16 :goto_1

    .line 695
    :sswitch_3
    const-string v1, "DoodleMobile"

    invoke-static {v2, v0}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;)I

    goto/16 :goto_1

    .line 698
    :sswitch_4
    const-string v1, "DoodleMobile"

    invoke-static {v2, v0}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;)I

    goto/16 :goto_1

    .line 709
    :cond_2
    :try_start_1
    const-string v2, "0"
    :try_end_1
    .catch Ljava/lang/Exception; {:try_start_1 .. :try_end_1} :catch_0

    goto :goto_2

    .line 684
    :sswitch_data_0
    .sparse-switch
        0x1 -> :sswitch_0
        0x2 -> :sswitch_1
        0x4 -> :sswitch_2
        0x8 -> :sswitch_3
        0x10 -> :sswitch_4
    .end sparse-switch
.end method

.method public static final declared-synchronized onCreate(Landroid/app/Activity;)V
    .locals 7

    .prologue
    const/4 v5, 0x0

    const-string v0, "ssc"

    const-string v0, "DoodleMobileAnaylise"

    const-string v0, "doodle_mobile_appid"

    .line 520
    const-class v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    monitor-enter v0

    if-eqz p0, :cond_0

    .line 521
    :try_start_0
    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    invoke-virtual {p0}, Landroid/app/Activity;->getApplicationContext()Landroid/content/Context;

    move-result-object v2

    iput-object v2, v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->context:Landroid/content/Context;
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_8
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    .line 525
    :cond_0
    :goto_0
    :try_start_1
    sget-boolean v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isInitialized:Z
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    if-nez v1, :cond_4

    .line 528
    :try_start_2
    invoke-virtual {p0}, Landroid/app/Activity;->getPackageName()Ljava/lang/String;

    move-result-object v1

    sput-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->packageName:Ljava/lang/String;
    :try_end_2
    .catch Ljava/lang/Exception; {:try_start_2 .. :try_end_2} :catch_7
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    .line 531
    :goto_1
    const/4 v1, 0x0

    :try_start_3
    sput-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationInfo:Landroid/content/pm/ApplicationInfo;
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_0

    .line 533
    :try_start_4
    invoke-virtual {p0}, Landroid/app/Activity;->getPackageManager()Landroid/content/pm/PackageManager;

    move-result-object v1

    sget-object v2, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->packageName:Ljava/lang/String;

    const/16 v3, 0x80

    invoke-virtual {v1, v2, v3}, Landroid/content/pm/PackageManager;->getApplicationInfo(Ljava/lang/String;I)Landroid/content/pm/ApplicationInfo;

    move-result-object v1

    sput-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationInfo:Landroid/content/pm/ApplicationInfo;
    :try_end_4
    .catch Landroid/content/pm/PackageManager$NameNotFoundException; {:try_start_4 .. :try_end_4} :catch_0
    .catchall {:try_start_4 .. :try_end_4} :catchall_0

    .line 541
    :goto_2
    :try_start_5
    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationInfo:Landroid/content/pm/ApplicationInfo;

    iget-object v1, v1, Landroid/content/pm/ApplicationInfo;->metaData:Landroid/os/Bundle;

    const-string v2, "doodle_mobile_appid"

    invoke-virtual {v1, v2}, Landroid/os/Bundle;->getString(Ljava/lang/String;)Ljava/lang/String;
    :try_end_5
    .catch Ljava/lang/Exception; {:try_start_5 .. :try_end_5} :catch_1
    .catchall {:try_start_5 .. :try_end_5} :catchall_0

    move-result-object v1

    .line 543
    :try_start_6
    const-string v2, "ssc"

    new-instance v3, Ljava/lang/StringBuilder;

    invoke-direct {v3}, Ljava/lang/StringBuilder;-><init>()V

    const-string v4, "applicationID  1 is "

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v3

    invoke-static {v2, v3}, Landroid/util/Log;->i(Ljava/lang/String;Ljava/lang/String;)I
    :try_end_6
    .catch Ljava/lang/Exception; {:try_start_6 .. :try_end_6} :catch_6
    .catchall {:try_start_6 .. :try_end_6} :catchall_0

    .line 554
    :goto_3
    if-nez v1, :cond_1

    .line 556
    :try_start_7
    sget-object v2, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationInfo:Landroid/content/pm/ApplicationInfo;

    iget-object v2, v2, Landroid/content/pm/ApplicationInfo;->metaData:Landroid/os/Bundle;

    const-string v3, "doodle_mobile_appid"

    invoke-virtual {v2, v3}, Landroid/os/Bundle;->getInt(Ljava/lang/String;)I

    move-result v2

    invoke-static {v2}, Ljava/lang/String;->valueOf(I)Ljava/lang/String;
    :try_end_7
    .catch Ljava/lang/Exception; {:try_start_7 .. :try_end_7} :catch_3
    .catchall {:try_start_7 .. :try_end_7} :catchall_0

    move-result-object v1

    .line 562
    :cond_1
    :goto_4
    if-nez v1, :cond_2

    .line 565
    :try_start_8
    new-instance v1, Landroid/content/res/Resources$NotFoundException;

    const-string v2, "doodle_mobile_appid not found in the Android Manifest xml."

    invoke-direct {v1, v2}, Landroid/content/res/Resources$NotFoundException;-><init>(Ljava/lang/String;)V

    throw v1
    :try_end_8
    .catchall {:try_start_8 .. :try_end_8} :catchall_0

    .line 520
    :catchall_0
    move-exception v1

    monitor-exit v0

    throw v1

    .line 535
    :catch_0
    move-exception v1

    .line 536
    :try_start_9
    const-string v1, "mobclix-controller"

    const-string v2, "Application Key Started"

    invoke-static {v1, v2}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;)I

    goto :goto_2

    .line 544
    :catch_1
    move-exception v1

    move-object v2, v5

    .line 545
    :goto_5
    invoke-virtual {v1}, Ljava/lang/Exception;->printStackTrace()V
    :try_end_9
    .catchall {:try_start_9 .. :try_end_9} :catchall_0

    .line 547
    :try_start_a
    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationInfo:Landroid/content/pm/ApplicationInfo;

    iget-object v1, v1, Landroid/content/pm/ApplicationInfo;->metaData:Landroid/os/Bundle;

    const-string v3, "doodle_mobile_appid"

    invoke-virtual {v1, v3}, Landroid/os/Bundle;->getInt(Ljava/lang/String;)I

    move-result v1

    invoke-static {v1}, Ljava/lang/String;->valueOf(I)Ljava/lang/String;
    :try_end_a
    .catch Ljava/lang/Exception; {:try_start_a .. :try_end_a} :catch_2
    .catchall {:try_start_a .. :try_end_a} :catchall_0

    move-result-object v1

    goto :goto_3

    .line 549
    :catch_2
    move-exception v1

    .line 550
    :try_start_b
    invoke-virtual {v1}, Ljava/lang/Exception;->printStackTrace()V

    move-object v1, v2

    goto :goto_3

    .line 558
    :catch_3
    move-exception v2

    .line 559
    invoke-virtual {v2}, Ljava/lang/Exception;->printStackTrace()V

    goto :goto_4

    .line 568
    :cond_2
    const-string v2, "DoodleMobileAnaylise"

    new-instance v3, Ljava/lang/StringBuilder;

    invoke-direct {v3}, Ljava/lang/StringBuilder;-><init>()V

    const-string v4, "applicationId = "

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-static {v2, v1}, Landroid/util/Log;->i(Ljava/lang/String;Ljava/lang/String;)I
    :try_end_b
    .catchall {:try_start_b .. :try_end_b} :catchall_0

    .line 571
    :try_start_c
    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationInfo:Landroid/content/pm/ApplicationInfo;

    iget-object v1, v1, Landroid/content/pm/ApplicationInfo;->metaData:Landroid/os/Bundle;

    const-string v2, "doodle_mobile_loglevel"

    invoke-virtual {v1, v2}, Landroid/os/Bundle;->getString(Ljava/lang/String;)Ljava/lang/String;
    :try_end_c
    .catch Ljava/lang/Exception; {:try_start_c .. :try_end_c} :catch_4
    .catchall {:try_start_c .. :try_end_c} :catchall_0

    move-result-object v1

    .line 577
    :goto_6
    if-eqz v1, :cond_3

    .line 578
    :try_start_d
    const-string v2, "debug"

    invoke-virtual {v1, v2}, Ljava/lang/String;->equalsIgnoreCase(Ljava/lang/String;)Z
    :try_end_d
    .catchall {:try_start_d .. :try_end_d} :catchall_0

    move-result v2

    if-eqz v2, :cond_5

    .line 593
    :cond_3
    :goto_7
    :try_start_e
    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    invoke-virtual {v1}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->updateSession()V

    .line 595
    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    invoke-static {}, Ljava/util/Locale;->getDefault()Ljava/util/Locale;

    move-result-object v2

    invoke-virtual {v2}, Ljava/util/Locale;->getCountry()Ljava/lang/String;

    move-result-object v2

    iput-object v2, v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->locale:Ljava/lang/String;

    .line 596
    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    invoke-static {}, Ljava/util/Locale;->getDefault()Ljava/util/Locale;

    move-result-object v2

    invoke-virtual {v2}, Ljava/util/Locale;->getLanguage()Ljava/lang/String;

    move-result-object v2

    iput-object v2, v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->language:Ljava/lang/String;

    .line 597
    const-string v1, "ssc"

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v3, "doolemobilea 585 "

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    sget-object v3, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    iget-object v3, v3, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->locale:Ljava/lang/String;

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    sget-object v3, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    iget-object v3, v3, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->language:Ljava/lang/String;

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v2

    invoke-static {v1, v2}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    .line 599
    sget v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->SYNC_READY:I

    sput v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->syncStatus:I

    .line 601
    const/4 v1, 0x1

    sput-boolean v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isInitialized:Z
    :try_end_e
    .catch Ljava/lang/Exception; {:try_start_e .. :try_end_e} :catch_5
    .catchall {:try_start_e .. :try_end_e} :catchall_0

    .line 606
    :cond_4
    :goto_8
    :try_start_f
    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    const/4 v2, 0x1

    invoke-direct {v1, v2}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->handleSessionStatus(Z)V
    :try_end_f
    .catchall {:try_start_f .. :try_end_f} :catchall_0

    .line 607
    monitor-exit v0

    return-void

    .line 573
    :catch_4
    move-exception v1

    move-object v1, v5

    goto :goto_6

    .line 580
    :cond_5
    :try_start_10
    const-string v2, "info"

    invoke-virtual {v1, v2}, Ljava/lang/String;->equalsIgnoreCase(Ljava/lang/String;)Z

    move-result v2

    if-nez v2, :cond_3

    .line 582
    const-string v2, "warn"

    invoke-virtual {v1, v2}, Ljava/lang/String;->equalsIgnoreCase(Ljava/lang/String;)Z

    move-result v2

    if-nez v2, :cond_3

    .line 584
    const-string v2, "error"

    invoke-virtual {v1, v2}, Ljava/lang/String;->equalsIgnoreCase(Ljava/lang/String;)Z

    move-result v2

    if-nez v2, :cond_3

    .line 586
    const-string v2, "fatal"

    invoke-virtual {v1, v2}, Ljava/lang/String;->equalsIgnoreCase(Ljava/lang/String;)Z

    move-result v1

    if-eqz v1, :cond_3

    goto :goto_7

    .line 602
    :catch_5
    move-exception v1

    .line 603
    const-string v1, "DoodleMobileAnaylise"

    const-string v2, "has exception"

    invoke-static {v1, v2}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I
    :try_end_10
    .catchall {:try_start_10 .. :try_end_10} :catchall_0

    goto :goto_8

    .line 544
    :catch_6
    move-exception v2

    move-object v6, v2

    move-object v2, v1

    move-object v1, v6

    goto/16 :goto_5

    .line 529
    :catch_7
    move-exception v1

    goto/16 :goto_1

    .line 522
    :catch_8
    move-exception v1

    goto/16 :goto_0
.end method

.method public static final declared-synchronized onStop(Landroid/app/Activity;)V
    .locals 3

    .prologue
    .line 610
    const-class v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    monitor-enter v0

    :try_start_0
    sget-object v1, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    const/4 v2, 0x0

    invoke-direct {v1, v2}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->handleSessionStatus(Z)V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    .line 612
    monitor-exit v0

    return-void

    .line 610
    :catchall_0
    move-exception v1

    monitor-exit v0

    throw v1
.end method

.method static removePref(Ljava/lang/String;)V
    .locals 1

    .prologue
    .line 355
    :try_start_0
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->controller:Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;

    iget-object v0, v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->sharedPrefs:Landroid/content/SharedPreferences;

    invoke-interface {v0}, Landroid/content/SharedPreferences;->edit()Landroid/content/SharedPreferences$Editor;

    move-result-object v0

    .line 356
    invoke-interface {v0, p0}, Landroid/content/SharedPreferences$Editor;->remove(Ljava/lang/String;)Landroid/content/SharedPreferences$Editor;

    .line 357
    invoke-interface {v0}, Landroid/content/SharedPreferences$Editor;->commit()Z
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    .line 360
    :goto_0
    return-void

    .line 358
    :catch_0
    move-exception v0

    goto :goto_0
.end method

.method private static sha1(Ljava/lang/String;)Ljava/lang/String;
    .locals 4

    .prologue
    const/4 v3, 0x0

    .line 372
    const/16 v0, 0x28

    new-array v0, v0, [B

    .line 375
    :try_start_0
    const-string v0, "SHA-1"

    invoke-static {v0}, Ljava/security/MessageDigest;->getInstance(Ljava/lang/String;)Ljava/security/MessageDigest;
    :try_end_0
    .catch Ljava/security/NoSuchAlgorithmException; {:try_start_0 .. :try_end_0} :catch_0

    move-result-object v0

    .line 380
    invoke-virtual {p0}, Ljava/lang/String;->getBytes()[B

    move-result-object v1

    invoke-virtual {p0}, Ljava/lang/String;->length()I

    move-result v2

    invoke-virtual {v0, v1, v3, v2}, Ljava/security/MessageDigest;->update([BII)V

    .line 381
    invoke-virtual {v0}, Ljava/security/MessageDigest;->digest()[B

    move-result-object v0

    .line 382
    new-instance v1, Ljava/lang/StringBuffer;

    invoke-direct {v1}, Ljava/lang/StringBuffer;-><init>()V

    move v2, v3

    .line 383
    :goto_0
    array-length v3, v0

    if-ge v2, v3, :cond_0

    .line 384
    aget-byte v3, v0, v2

    and-int/lit16 v3, v3, 0xff

    invoke-static {v3}, Ljava/lang/Integer;->toHexString(I)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v1, v3}, Ljava/lang/StringBuffer;->append(Ljava/lang/String;)Ljava/lang/StringBuffer;

    .line 383
    add-int/lit8 v2, v2, 0x1

    goto :goto_0

    .line 376
    :catch_0
    move-exception v0

    .line 377
    new-instance v1, Ljava/lang/RuntimeException;

    invoke-direct {v1, v0}, Ljava/lang/RuntimeException;-><init>(Ljava/lang/Throwable;)V

    throw v1

    .line 386
    :cond_0
    invoke-virtual {v1}, Ljava/lang/StringBuffer;->toString()Ljava/lang/String;

    move-result-object v0

    return-object v0
.end method

.method public static final sync()V
    .locals 3

    .prologue
    const v2, 0x186a1

    .line 792
    sget-boolean v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isInitialized:Z

    if-nez v0, :cond_0

    .line 793
    const-string v0, "mobclix-controller"

    const-string v1, "sync failed - You must initialize DoodleMobileAnaylise by calling DoodleMobileAnaylise.onCreate(this)."

    invoke-static {v0, v1}, Landroid/util/Log;->v(Ljava/lang/String;Ljava/lang/String;)I

    .line 801
    :goto_0
    return-void

    .line 798
    :cond_0
    const-string v0, "DoodleMobileAnaylise"

    const-string v1, "static sync "

    invoke-static {v0, v1}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    .line 799
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->mSyncHandler:Landroid/os/Handler;

    invoke-virtual {v0, v2}, Landroid/os/Handler;->removeMessages(I)V

    .line 800
    sget-object v0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->mSyncHandler:Landroid/os/Handler;

    invoke-virtual {v0, v2}, Landroid/os/Handler;->sendEmptyMessage(I)Z

    goto :goto_0
.end method

.method static syncCookiesToCookieManager(Lorg/apache/http/client/CookieStore;Ljava/lang/String;)V
    .locals 7

    .prologue
    const-string v0, "DoodleMobileAnaylise"

    .line 911
    :try_start_0
    const-string v0, "DoodleMobileAnaylise"

    const-string v1, "syncCookieManager "

    invoke-static {v0, v1}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    .line 912
    invoke-static {}, Landroid/webkit/CookieManager;->getInstance()Landroid/webkit/CookieManager;

    move-result-object v0

    .line 913
    invoke-interface {p0}, Lorg/apache/http/client/CookieStore;->getCookies()Ljava/util/List;

    move-result-object v1

    .line 914
    new-instance v2, Ljava/lang/StringBuffer;

    invoke-direct {v2}, Ljava/lang/StringBuffer;-><init>()V

    .line 915
    invoke-interface {v1}, Ljava/util/List;->isEmpty()Z

    move-result v3

    if-nez v3, :cond_4

    .line 916
    const/4 v3, 0x0

    :goto_0
    invoke-interface {v1}, Ljava/util/List;->size()I

    move-result v4

    if-ge v3, v4, :cond_3

    .line 917
    invoke-interface {v1, v3}, Ljava/util/List;->get(I)Ljava/lang/Object;

    move-result-object p0

    check-cast p0, Lorg/apache/http/cookie/Cookie;

    .line 918
    invoke-interface {p0}, Lorg/apache/http/cookie/Cookie;->getName()Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v2, v4}, Ljava/lang/StringBuffer;->append(Ljava/lang/String;)Ljava/lang/StringBuffer;

    move-result-object v4

    const-string v5, "="

    invoke-virtual {v4, v5}, Ljava/lang/StringBuffer;->append(Ljava/lang/String;)Ljava/lang/StringBuffer;

    move-result-object v4

    invoke-interface {p0}, Lorg/apache/http/cookie/Cookie;->getValue()Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v4, v5}, Ljava/lang/StringBuffer;->append(Ljava/lang/String;)Ljava/lang/StringBuffer;

    .line 921
    invoke-interface {p0}, Lorg/apache/http/cookie/Cookie;->getExpiryDate()Ljava/util/Date;

    move-result-object v4

    if-eqz v4, :cond_0

    .line 922
    new-instance v4, Ljava/text/SimpleDateFormat;

    const-string v5, "E, dd-MMM-yyyy HH:mm:ss"

    invoke-direct {v4, v5}, Ljava/text/SimpleDateFormat;-><init>(Ljava/lang/String;)V

    .line 924
    const-string v5, "; expires="

    invoke-virtual {v2, v5}, Ljava/lang/StringBuffer;->append(Ljava/lang/String;)Ljava/lang/StringBuffer;

    move-result-object v5

    invoke-interface {p0}, Lorg/apache/http/cookie/Cookie;->getExpiryDate()Ljava/util/Date;

    move-result-object v6

    invoke-virtual {v4, v6}, Ljava/text/SimpleDateFormat;->format(Ljava/util/Date;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v5, v4}, Ljava/lang/StringBuffer;->append(Ljava/lang/String;)Ljava/lang/StringBuffer;

    move-result-object v4

    const-string v5, " GMT"

    invoke-virtual {v4, v5}, Ljava/lang/StringBuffer;->append(Ljava/lang/String;)Ljava/lang/StringBuffer;

    .line 928
    :cond_0
    invoke-interface {p0}, Lorg/apache/http/cookie/Cookie;->getPath()Ljava/lang/String;

    move-result-object v4

    if-eqz v4, :cond_1

    .line 929
    const-string v4, "; path="

    invoke-virtual {v2, v4}, Ljava/lang/StringBuffer;->append(Ljava/lang/String;)Ljava/lang/StringBuffer;

    move-result-object v4

    invoke-interface {p0}, Lorg/apache/http/cookie/Cookie;->getPath()Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v4, v5}, Ljava/lang/StringBuffer;->append(Ljava/lang/String;)Ljava/lang/StringBuffer;

    .line 931
    :cond_1
    invoke-interface {p0}, Lorg/apache/http/cookie/Cookie;->getDomain()Ljava/lang/String;

    move-result-object v4

    if-eqz v4, :cond_2

    .line 932
    const-string v4, "; domain="

    invoke-virtual {v2, v4}, Ljava/lang/StringBuffer;->append(Ljava/lang/String;)Ljava/lang/StringBuffer;

    move-result-object v4

    invoke-interface {p0}, Lorg/apache/http/cookie/Cookie;->getDomain()Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v4, v5}, Ljava/lang/StringBuffer;->append(Ljava/lang/String;)Ljava/lang/StringBuffer;

    .line 935
    :cond_2
    invoke-virtual {v2}, Ljava/lang/StringBuffer;->toString()Ljava/lang/String;

    move-result-object v4

    .line 937
    invoke-virtual {v0, p1, v4}, Landroid/webkit/CookieManager;->setCookie(Ljava/lang/String;Ljava/lang/String;)V

    .line 916
    add-int/lit8 v3, v3, 0x1

    goto :goto_0

    .line 939
    :cond_3
    const-string v0, "DoodleMobileAnaylise"

    const-string v1, "before syn "

    invoke-static {v0, v1}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    .line 940
    invoke-static {}, Landroid/webkit/CookieSyncManager;->getInstance()Landroid/webkit/CookieSyncManager;

    move-result-object v0

    invoke-virtual {v0}, Landroid/webkit/CookieSyncManager;->sync()V

    .line 941
    const-string v0, "DoodleMobileAnaylise"

    const-string v1, "after syn "

    invoke-static {v0, v1}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    .line 943
    invoke-static {}, Landroid/webkit/CookieSyncManager;->getInstance()Landroid/webkit/CookieSyncManager;

    move-result-object v0

    invoke-virtual {v0}, Landroid/webkit/CookieSyncManager;->stopSync()V
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    .line 947
    :cond_4
    :goto_1
    return-void

    .line 945
    :catch_0
    move-exception v0

    goto :goto_1
.end method

.method private updateConnectivity()V
    .locals 4

    .prologue
    const-string v3, "null"

    .line 425
    :try_start_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->context:Landroid/content/Context;

    const-string v1, "connectivity"

    invoke-virtual {v0, v1}, Landroid/content/Context;->getSystemService(Ljava/lang/String;)Ljava/lang/Object;

    move-result-object v0

    check-cast v0, Landroid/net/ConnectivityManager;

    .line 427
    const-string v1, "u"

    .line 429
    iget-boolean v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->haveNetworkStatePermission:Z

    if-eqz v2, :cond_4

    .line 430
    invoke-virtual {v0}, Landroid/net/ConnectivityManager;->getActiveNetworkInfo()Landroid/net/NetworkInfo;

    move-result-object v0

    .line 432
    if-eqz v0, :cond_4

    .line 433
    invoke-virtual {v0}, Landroid/net/NetworkInfo;->getTypeName()Ljava/lang/String;

    move-result-object v0

    .line 436
    :goto_0
    const-string v1, "WI_FI"

    invoke-virtual {v0, v1}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v1

    if-nez v1, :cond_0

    const-string v1, "WIFI"

    invoke-virtual {v0, v1}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v1

    if-eqz v1, :cond_2

    .line 437
    :cond_0
    const-string v0, "wifi"

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->connectionType:Ljava/lang/String;

    .line 446
    :goto_1
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->connectionType:Ljava/lang/String;

    if-nez v0, :cond_1

    .line 447
    const-string v0, "null"

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->connectionType:Ljava/lang/String;

    .line 451
    :cond_1
    :goto_2
    return-void

    .line 438
    :cond_2
    const-string v1, "MOBILE"

    invoke-virtual {v0, v1}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v0

    if-eqz v0, :cond_3

    .line 439
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->context:Landroid/content/Context;

    const-string v1, "phone"

    invoke-virtual {v0, v1}, Landroid/content/Context;->getSystemService(Ljava/lang/String;)Ljava/lang/Object;

    move-result-object v0

    check-cast v0, Landroid/telephony/TelephonyManager;

    .line 441
    invoke-virtual {v0}, Landroid/telephony/TelephonyManager;->getNetworkType()I

    move-result v0

    .line 442
    invoke-static {v0}, Ljava/lang/Integer;->toString(I)Ljava/lang/String;

    move-result-object v0

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->connectionType:Ljava/lang/String;
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    goto :goto_1

    .line 448
    :catch_0
    move-exception v0

    .line 449
    const-string v0, "null"

    iput-object v3, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->connectionType:Ljava/lang/String;

    goto :goto_2

    .line 444
    :cond_3
    :try_start_1
    const-string v0, "null"

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->connectionType:Ljava/lang/String;
    :try_end_1
    .catch Ljava/lang/Exception; {:try_start_1 .. :try_end_1} :catch_0

    goto :goto_1

    :cond_4
    move-object v0, v1

    goto :goto_0
.end method

.method private updateLocation()V
    .locals 3

    .prologue
    .line 409
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->location:Lcom/doodlemobile/gamecenter/DoodleMobileLocation;

    iget-object v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->context:Landroid/content/Context;

    new-instance v2, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$1;

    invoke-direct {v2, p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise$1;-><init>(Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;)V

    invoke-virtual {v0, v1, v2}, Lcom/doodlemobile/gamecenter/DoodleMobileLocation;->getLocation(Landroid/content/Context;Lcom/doodlemobile/gamecenter/DoodleMobileLocation$LocationResult;)Z

    .line 421
    return-void
.end method


# virtual methods
.method protected finalize()V
    .locals 0

    .prologue
    .line 896
    invoke-direct {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->endSession()V

    .line 897
    return-void
.end method

.method getAnalyticsServer()Ljava/lang/String;
    .locals 1

    .prologue
    .line 289
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->analyticsServer:Ljava/lang/String;

    return-object v0
.end method

.method getAndroidId()Ljava/lang/String;
    .locals 1

    .prologue
    .line 218
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->androidId:Ljava/lang/String;

    if-nez v0, :cond_0

    const-string v0, "null"

    :goto_0
    return-object v0

    :cond_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->androidId:Ljava/lang/String;

    goto :goto_0
.end method

.method getAndroidVersion()Ljava/lang/String;
    .locals 1

    .prologue
    .line 205
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->androidVersion:Ljava/lang/String;

    if-nez v0, :cond_0

    const-string v0, "null"

    :goto_0
    return-object v0

    :cond_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->androidVersion:Ljava/lang/String;

    goto :goto_0
.end method

.method getApplicationId()Ljava/lang/String;
    .locals 1

    .prologue
    .line 201
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationId:Ljava/lang/String;

    if-nez v0, :cond_0

    const-string v0, "null"

    :goto_0
    return-object v0

    :cond_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationId:Ljava/lang/String;

    goto :goto_0
.end method

.method getApplicationVersion()Ljava/lang/String;
    .locals 1

    .prologue
    .line 209
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationVersion:Ljava/lang/String;

    if-nez v0, :cond_0

    const-string v0, "null"

    :goto_0
    return-object v0

    :cond_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->applicationVersion:Ljava/lang/String;

    goto :goto_0
.end method

.method getConfigServer()Ljava/lang/String;
    .locals 1

    .prologue
    .line 285
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->configServer:Ljava/lang/String;

    return-object v0
.end method

.method getConnectionType()Ljava/lang/String;
    .locals 1

    .prologue
    .line 231
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->connectionType:Ljava/lang/String;

    if-nez v0, :cond_0

    const-string v0, "null"

    :goto_0
    return-object v0

    :cond_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->connectionType:Ljava/lang/String;

    goto :goto_0
.end method

.method getContext()Landroid/content/Context;
    .locals 1

    .prologue
    .line 197
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->context:Landroid/content/Context;

    return-object v0
.end method

.method getDeviceHardwareModel()Ljava/lang/String;
    .locals 1

    .prologue
    .line 226
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceHardwareModel:Ljava/lang/String;

    if-nez v0, :cond_0

    const-string v0, "null"

    :goto_0
    return-object v0

    :cond_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceHardwareModel:Ljava/lang/String;

    goto :goto_0
.end method

.method getDeviceId()Ljava/lang/String;
    .locals 1

    .prologue
    .line 214
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceId:Ljava/lang/String;

    if-nez v0, :cond_0

    const-string v0, "null"

    :goto_0
    return-object v0

    :cond_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceId:Ljava/lang/String;

    goto :goto_0
.end method

.method getDeviceModel()Ljava/lang/String;
    .locals 1

    .prologue
    .line 222
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceModel:Ljava/lang/String;

    if-nez v0, :cond_0

    const-string v0, "null"

    :goto_0
    return-object v0

    :cond_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->deviceModel:Ljava/lang/String;

    goto :goto_0
.end method

.method getGPS()Ljava/lang/String;
    .locals 3

    .prologue
    const-string v2, "null"

    .line 243
    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getLatitude()Ljava/lang/String;

    move-result-object v0

    const-string v1, "null"

    invoke-virtual {v0, v2}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v0

    if-nez v0, :cond_0

    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getLongitude()Ljava/lang/String;

    move-result-object v0

    const-string v1, "null"

    invoke-virtual {v0, v2}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v0

    if-eqz v0, :cond_1

    .line 244
    :cond_0
    const-string v0, "null"

    move-object v0, v2

    .line 245
    :goto_0
    return-object v0

    :cond_1
    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getLatitude()Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v1, ","

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getLongitude()Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    goto :goto_0
.end method

.method getLanguage()Ljava/lang/String;
    .locals 1

    .prologue
    .line 249
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->language:Ljava/lang/String;

    if-nez v0, :cond_0

    const-string v0, "null"

    :goto_0
    return-object v0

    :cond_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->language:Ljava/lang/String;

    goto :goto_0
.end method

.method getLatitude()Ljava/lang/String;
    .locals 1

    .prologue
    .line 235
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->latitude:Ljava/lang/String;

    if-nez v0, :cond_0

    const-string v0, "null"

    :goto_0
    return-object v0

    :cond_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->latitude:Ljava/lang/String;

    goto :goto_0
.end method

.method getLocale()Ljava/lang/String;
    .locals 1

    .prologue
    .line 253
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->locale:Ljava/lang/String;

    if-nez v0, :cond_0

    const-string v0, "null"

    :goto_0
    return-object v0

    :cond_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->locale:Ljava/lang/String;

    goto :goto_0
.end method

.method getLogLevel()I
    .locals 1

    .prologue
    .line 261
    iget v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->logLevel:I

    return v0
.end method

.method getLongitude()Ljava/lang/String;
    .locals 1

    .prologue
    .line 239
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->longitude:Ljava/lang/String;

    if-nez v0, :cond_0

    const-string v0, "null"

    :goto_0
    return-object v0

    :cond_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->longitude:Ljava/lang/String;

    goto :goto_0
.end method

.method getMobclixVersion()Ljava/lang/String;
    .locals 1

    .prologue
    .line 257
    const-string v0, "2.3"

    return-object v0
.end method

.method getRefreshTime(Ljava/lang/String;)J
    .locals 2

    .prologue
    .line 278
    :try_start_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->refreshTime:Ljava/util/HashMap;

    invoke-virtual {v0, p1}, Ljava/util/HashMap;->get(Ljava/lang/Object;)Ljava/lang/Object;

    move-result-object p0

    check-cast p0, Ljava/lang/Long;

    invoke-virtual {p0}, Ljava/lang/Long;->longValue()J
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    move-result-wide v0

    .line 281
    :goto_0
    return-wide v0

    .line 279
    :catch_0
    move-exception v0

    .line 281
    const-wide/16 v0, -0x1

    goto :goto_0
.end method

.method getUserAgent()Ljava/lang/String;
    .locals 3

    .prologue
    const-string v2, "UserAgent"

    .line 297
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->userAgent:Ljava/lang/String;

    const-string v1, ""

    invoke-virtual {v0, v1}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v0

    if-eqz v0, :cond_0

    const-string v0, "UserAgent"

    invoke-static {v2}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->hasPref(Ljava/lang/String;)Z

    move-result v0

    if-eqz v0, :cond_0

    .line 298
    const-string v0, "UserAgent"

    invoke-static {v2}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getPref(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    iput-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->userAgent:Ljava/lang/String;

    .line 299
    :cond_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->userAgent:Ljava/lang/String;

    return-object v0
.end method

.method isEnabled(Ljava/lang/String;)Z
    .locals 1

    .prologue
    .line 270
    :try_start_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->enabled:Ljava/util/HashMap;

    invoke-virtual {v0, p1}, Ljava/util/HashMap;->get(Ljava/lang/Object;)Ljava/lang/Object;

    move-result-object p0

    check-cast p0, Ljava/lang/Boolean;

    invoke-virtual {p0}, Ljava/lang/Boolean;->booleanValue()Z
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    move-result v0

    .line 273
    :goto_0
    return v0

    .line 271
    :catch_0
    move-exception v0

    .line 273
    const/4 v0, 0x1

    goto :goto_0
.end method

.method isRemoteConfigSet()I
    .locals 1

    .prologue
    .line 293
    iget v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->remoteConfigSet:I

    return v0
.end method

.method isTopTask()Z
    .locals 1

    .prologue
    .line 265
    iget-boolean v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->isTopTask:Z

    return v0
.end method

.method setContext(Landroid/app/Activity;)V
    .locals 0

    .prologue
    .line 193
    iput-object p1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->context:Landroid/content/Context;

    .line 194
    return-void
.end method

.method setUserAgent(Ljava/lang/String;)V
    .locals 1

    .prologue
    .line 303
    iput-object p1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->userAgent:Ljava/lang/String;

    .line 304
    const-string v0, "UserAgent"

    invoke-static {v0, p1}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->addPref(Ljava/lang/String;Ljava/lang/String;)V

    .line 305
    return-void
.end method

.method updateSession()V
    .locals 4

    .prologue
    const-string v0, "ll"

    .line 391
    invoke-direct {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->updateConnectivity()V

    .line 393
    iget-boolean v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->haveLocationPermission:Z

    if-eqz v0, :cond_0

    .line 394
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->locationHandler:Landroid/os/Handler;

    const/4 v1, 0x0

    invoke-virtual {v0, v1}, Landroid/os/Handler;->sendEmptyMessage(I)Z

    .line 397
    :cond_0
    :try_start_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->session:Lorg/json/JSONObject;

    const-string v1, "ts"

    invoke-static {}, Ljava/lang/System;->currentTimeMillis()J

    move-result-wide v2

    invoke-virtual {v0, v1, v2, v3}, Lorg/json/JSONObject;->put(Ljava/lang/String;J)Lorg/json/JSONObject;

    .line 398
    invoke-virtual {p0}, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->getGPS()Ljava/lang/String;

    move-result-object v0

    .line 399
    const-string v1, "null"

    invoke-virtual {v0, v1}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v1

    if-nez v1, :cond_1

    .line 400
    iget-object v1, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->session:Lorg/json/JSONObject;

    const-string v2, "ll"

    invoke-virtual {v1, v2, v0}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 403
    :goto_0
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->session:Lorg/json/JSONObject;

    const-string v1, "g"

    iget-object v2, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->connectionType:Ljava/lang/String;

    invoke-virtual {v0, v1, v2}, Lorg/json/JSONObject;->put(Ljava/lang/String;Ljava/lang/Object;)Lorg/json/JSONObject;

    .line 406
    :goto_1
    return-void

    .line 402
    :cond_1
    iget-object v0, p0, Lcom/doodlemobile/gamecenter/DoodleMobileAnaylise;->session:Lorg/json/JSONObject;

    const-string v1, "ll"

    invoke-virtual {v0, v1}, Lorg/json/JSONObject;->remove(Ljava/lang/String;)Ljava/lang/Object;
    :try_end_0
    .catch Lorg/json/JSONException; {:try_start_0 .. :try_end_0} :catch_0

    goto :goto_0

    .line 404
    :catch_0
    move-exception v0

    goto :goto_1
.end method
