.class public final Lorg/apache/harmony/javax/security/auth/Subject;
.super Ljava/lang/Object;
.source "Subject.java"

# interfaces
.implements Ljava/io/Serializable;


# annotations
.annotation system Ldalvik/annotation/MemberClasses;
    value = {
        Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;
    }
.end annotation


# static fields
.field private static final _AS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

.field private static final _AS_PRIVILEGED:Lorg/apache/harmony/javax/security/auth/AuthPermission;

.field private static final _PRINCIPALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

.field private static final _PRIVATE_CREDENTIALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

.field private static final _PUBLIC_CREDENTIALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

.field private static final _READ_ONLY:Lorg/apache/harmony/javax/security/auth/AuthPermission;

.field private static final _SUBJECT:Lorg/apache/harmony/javax/security/auth/AuthPermission;

.field private static final serialVersionUID:J = -0x734dcd6cffcc0598L


# instance fields
.field private final principals:Ljava/util/Set;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Ljava/util/Set",
            "<",
            "Ljava/security/Principal;",
            ">;"
        }
    .end annotation
.end field

.field private transient privateCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lorg/apache/harmony/javax/security/auth/Subject$SecureSet",
            "<",
            "Ljava/lang/Object;",
            ">;"
        }
    .end annotation
.end field

.field private transient publicCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lorg/apache/harmony/javax/security/auth/Subject$SecureSet",
            "<",
            "Ljava/lang/Object;",
            ">;"
        }
    .end annotation
.end field

.field private readOnly:Z


# direct methods
.method static constructor <clinit>()V
    .locals 2

    .prologue
    .line 59
    new-instance v0, Lorg/apache/harmony/javax/security/auth/AuthPermission;

    const-string v1, "doAs"

    invoke-direct {v0, v1}, Lorg/apache/harmony/javax/security/auth/AuthPermission;-><init>(Ljava/lang/String;)V

    sput-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_AS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    .line 61
    new-instance v0, Lorg/apache/harmony/javax/security/auth/AuthPermission;

    .line 62
    const-string v1, "doAsPrivileged"

    .line 61
    invoke-direct {v0, v1}, Lorg/apache/harmony/javax/security/auth/AuthPermission;-><init>(Ljava/lang/String;)V

    sput-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_AS_PRIVILEGED:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    .line 64
    new-instance v0, Lorg/apache/harmony/javax/security/auth/AuthPermission;

    .line 65
    const-string v1, "getSubject"

    .line 64
    invoke-direct {v0, v1}, Lorg/apache/harmony/javax/security/auth/AuthPermission;-><init>(Ljava/lang/String;)V

    sput-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_SUBJECT:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    .line 67
    new-instance v0, Lorg/apache/harmony/javax/security/auth/AuthPermission;

    .line 68
    const-string v1, "modifyPrincipals"

    .line 67
    invoke-direct {v0, v1}, Lorg/apache/harmony/javax/security/auth/AuthPermission;-><init>(Ljava/lang/String;)V

    sput-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_PRINCIPALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    .line 70
    new-instance v0, Lorg/apache/harmony/javax/security/auth/AuthPermission;

    .line 71
    const-string v1, "modifyPrivateCredentials"

    .line 70
    invoke-direct {v0, v1}, Lorg/apache/harmony/javax/security/auth/AuthPermission;-><init>(Ljava/lang/String;)V

    sput-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_PRIVATE_CREDENTIALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    .line 73
    new-instance v0, Lorg/apache/harmony/javax/security/auth/AuthPermission;

    .line 74
    const-string v1, "modifyPublicCredentials"

    .line 73
    invoke-direct {v0, v1}, Lorg/apache/harmony/javax/security/auth/AuthPermission;-><init>(Ljava/lang/String;)V

    sput-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_PUBLIC_CREDENTIALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    .line 76
    new-instance v0, Lorg/apache/harmony/javax/security/auth/AuthPermission;

    .line 77
    const-string v1, "setReadOnly"

    .line 76
    invoke-direct {v0, v1}, Lorg/apache/harmony/javax/security/auth/AuthPermission;-><init>(Ljava/lang/String;)V

    sput-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_READ_ONLY:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    .line 55
    return-void
.end method

.method public constructor <init>()V
    .locals 2

    .prologue
    .line 94
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    .line 95
    new-instance v0, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    sget-object v1, Lorg/apache/harmony/javax/security/auth/Subject;->_PRINCIPALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-direct {v0, p0, v1}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;-><init>(Lorg/apache/harmony/javax/security/auth/Subject;Lorg/apache/harmony/javax/security/auth/AuthPermission;)V

    iput-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->principals:Ljava/util/Set;

    .line 96
    new-instance v0, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    sget-object v1, Lorg/apache/harmony/javax/security/auth/Subject;->_PUBLIC_CREDENTIALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-direct {v0, p0, v1}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;-><init>(Lorg/apache/harmony/javax/security/auth/Subject;Lorg/apache/harmony/javax/security/auth/AuthPermission;)V

    iput-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->publicCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    .line 97
    new-instance v0, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    sget-object v1, Lorg/apache/harmony/javax/security/auth/Subject;->_PRIVATE_CREDENTIALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-direct {v0, p0, v1}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;-><init>(Lorg/apache/harmony/javax/security/auth/Subject;Lorg/apache/harmony/javax/security/auth/AuthPermission;)V

    iput-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->privateCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    .line 99
    const/4 v0, 0x0

    iput-boolean v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->readOnly:Z

    .line 100
    return-void
.end method

.method public constructor <init>(ZLjava/util/Set;Ljava/util/Set;Ljava/util/Set;)V
    .locals 2
    .param p1, "readOnly"    # Z
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(Z",
            "Ljava/util/Set",
            "<+",
            "Ljava/security/Principal;",
            ">;",
            "Ljava/util/Set",
            "<*>;",
            "Ljava/util/Set",
            "<*>;)V"
        }
    .end annotation

    .prologue
    .line 119
    .local p2, "subjPrincipals":Ljava/util/Set;, "Ljava/util/Set<+Ljava/security/Principal;>;"
    .local p3, "pubCredentials":Ljava/util/Set;, "Ljava/util/Set<*>;"
    .local p4, "privCredentials":Ljava/util/Set;, "Ljava/util/Set<*>;"
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    .line 122
    if-eqz p2, :cond_0

    if-eqz p3, :cond_0

    if-nez p4, :cond_1

    .line 123
    :cond_0
    new-instance v0, Ljava/lang/NullPointerException;

    invoke-direct {v0}, Ljava/lang/NullPointerException;-><init>()V

    throw v0

    .line 126
    :cond_1
    new-instance v0, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    sget-object v1, Lorg/apache/harmony/javax/security/auth/Subject;->_PRINCIPALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-direct {v0, p0, v1, p2}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;-><init>(Lorg/apache/harmony/javax/security/auth/Subject;Lorg/apache/harmony/javax/security/auth/AuthPermission;Ljava/util/Collection;)V

    iput-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->principals:Ljava/util/Set;

    .line 127
    new-instance v0, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    sget-object v1, Lorg/apache/harmony/javax/security/auth/Subject;->_PUBLIC_CREDENTIALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-direct {v0, p0, v1, p3}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;-><init>(Lorg/apache/harmony/javax/security/auth/Subject;Lorg/apache/harmony/javax/security/auth/AuthPermission;Ljava/util/Collection;)V

    iput-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->publicCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    .line 128
    new-instance v0, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    sget-object v1, Lorg/apache/harmony/javax/security/auth/Subject;->_PRIVATE_CREDENTIALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-direct {v0, p0, v1, p4}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;-><init>(Lorg/apache/harmony/javax/security/auth/Subject;Lorg/apache/harmony/javax/security/auth/AuthPermission;Ljava/util/Collection;)V

    iput-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->privateCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    .line 130
    iput-boolean p1, p0, Lorg/apache/harmony/javax/security/auth/Subject;->readOnly:Z

    .line 131
    return-void
.end method

.method static synthetic access$0(Lorg/apache/harmony/javax/security/auth/Subject;)V
    .locals 0

    .prologue
    .line 514
    invoke-direct {p0}, Lorg/apache/harmony/javax/security/auth/Subject;->checkState()V

    return-void
.end method

.method static synthetic access$1(Ljava/security/Permission;)V
    .locals 0

    .prologue
    .line 506
    invoke-static {p0}, Lorg/apache/harmony/javax/security/auth/Subject;->checkPermission(Ljava/security/Permission;)V

    return-void
.end method

.method static synthetic access$2()Lorg/apache/harmony/javax/security/auth/AuthPermission;
    .locals 1

    .prologue
    .line 67
    sget-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_PRINCIPALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    return-object v0
.end method

.method static synthetic access$3()Lorg/apache/harmony/javax/security/auth/AuthPermission;
    .locals 1

    .prologue
    .line 70
    sget-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_PRIVATE_CREDENTIALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    return-object v0
.end method

.method static synthetic access$4(Lorg/apache/harmony/javax/security/auth/Subject;)Ljava/util/Set;
    .locals 1

    .prologue
    .line 79
    iget-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->principals:Ljava/util/Set;

    return-object v0
.end method

.method static synthetic access$5()Lorg/apache/harmony/javax/security/auth/AuthPermission;
    .locals 1

    .prologue
    .line 73
    sget-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_PUBLIC_CREDENTIALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    return-object v0
.end method

.method private static checkPermission(Ljava/security/Permission;)V
    .locals 1
    .param p0, "p"    # Ljava/security/Permission;

    .prologue
    .line 507
    invoke-static {}, Ljava/lang/System;->getSecurityManager()Ljava/lang/SecurityManager;

    move-result-object v0

    .line 508
    .local v0, "sm":Ljava/lang/SecurityManager;
    if-eqz v0, :cond_0

    .line 509
    invoke-virtual {v0, p0}, Ljava/lang/SecurityManager;->checkPermission(Ljava/security/Permission;)V

    .line 511
    :cond_0
    return-void
.end method

.method private checkState()V
    .locals 2

    .prologue
    .line 515
    iget-boolean v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->readOnly:Z

    if-eqz v0, :cond_0

    .line 516
    new-instance v0, Ljava/lang/IllegalStateException;

    const-string v1, "auth.0A"

    invoke-direct {v0, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw v0

    .line 518
    :cond_0
    return-void
.end method

.method public static doAs(Lorg/apache/harmony/javax/security/auth/Subject;Ljava/security/PrivilegedAction;)Ljava/lang/Object;
    .locals 1
    .param p0, "subject"    # Lorg/apache/harmony/javax/security/auth/Subject;
    .param p1, "action"    # Ljava/security/PrivilegedAction;

    .prologue
    .line 146
    sget-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_AS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-static {v0}, Lorg/apache/harmony/javax/security/auth/Subject;->checkPermission(Ljava/security/Permission;)V

    .line 148
    invoke-static {}, Ljava/security/AccessController;->getContext()Ljava/security/AccessControlContext;

    move-result-object v0

    invoke-static {p0, p1, v0}, Lorg/apache/harmony/javax/security/auth/Subject;->doAs_PrivilegedAction(Lorg/apache/harmony/javax/security/auth/Subject;Ljava/security/PrivilegedAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;

    move-result-object v0

    return-object v0
.end method

.method public static doAs(Lorg/apache/harmony/javax/security/auth/Subject;Ljava/security/PrivilegedExceptionAction;)Ljava/lang/Object;
    .locals 1
    .param p0, "subject"    # Lorg/apache/harmony/javax/security/auth/Subject;
    .param p1, "action"    # Ljava/security/PrivilegedExceptionAction;
    .annotation system Ldalvik/annotation/Throws;
        value = {
            Ljava/security/PrivilegedActionException;
        }
    .end annotation

    .prologue
    .line 223
    sget-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_AS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-static {v0}, Lorg/apache/harmony/javax/security/auth/Subject;->checkPermission(Ljava/security/Permission;)V

    .line 225
    invoke-static {}, Ljava/security/AccessController;->getContext()Ljava/security/AccessControlContext;

    move-result-object v0

    invoke-static {p0, p1, v0}, Lorg/apache/harmony/javax/security/auth/Subject;->doAs_PrivilegedExceptionAction(Lorg/apache/harmony/javax/security/auth/Subject;Ljava/security/PrivilegedExceptionAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;

    move-result-object v0

    return-object v0
.end method

.method public static doAsPrivileged(Lorg/apache/harmony/javax/security/auth/Subject;Ljava/security/PrivilegedAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;
    .locals 2
    .param p0, "subject"    # Lorg/apache/harmony/javax/security/auth/Subject;
    .param p1, "action"    # Ljava/security/PrivilegedAction;
    .param p2, "context"    # Ljava/security/AccessControlContext;

    .prologue
    .line 170
    sget-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_AS_PRIVILEGED:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-static {v0}, Lorg/apache/harmony/javax/security/auth/Subject;->checkPermission(Ljava/security/Permission;)V

    .line 172
    if-nez p2, :cond_0

    .line 173
    new-instance v0, Ljava/security/AccessControlContext;

    .line 174
    const/4 v1, 0x0

    new-array v1, v1, [Ljava/security/ProtectionDomain;

    invoke-direct {v0, v1}, Ljava/security/AccessControlContext;-><init>([Ljava/security/ProtectionDomain;)V

    .line 173
    invoke-static {p0, p1, v0}, Lorg/apache/harmony/javax/security/auth/Subject;->doAs_PrivilegedAction(Lorg/apache/harmony/javax/security/auth/Subject;Ljava/security/PrivilegedAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;

    move-result-object v0

    .line 176
    :goto_0
    return-object v0

    :cond_0
    invoke-static {p0, p1, p2}, Lorg/apache/harmony/javax/security/auth/Subject;->doAs_PrivilegedAction(Lorg/apache/harmony/javax/security/auth/Subject;Ljava/security/PrivilegedAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;

    move-result-object v0

    goto :goto_0
.end method

.method public static doAsPrivileged(Lorg/apache/harmony/javax/security/auth/Subject;Ljava/security/PrivilegedExceptionAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;
    .locals 2
    .param p0, "subject"    # Lorg/apache/harmony/javax/security/auth/Subject;
    .param p1, "action"    # Ljava/security/PrivilegedExceptionAction;
    .param p2, "context"    # Ljava/security/AccessControlContext;
    .annotation system Ldalvik/annotation/Throws;
        value = {
            Ljava/security/PrivilegedActionException;
        }
    .end annotation

    .prologue
    .line 250
    sget-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_AS_PRIVILEGED:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-static {v0}, Lorg/apache/harmony/javax/security/auth/Subject;->checkPermission(Ljava/security/Permission;)V

    .line 252
    if-nez p2, :cond_0

    .line 254
    new-instance v0, Ljava/security/AccessControlContext;

    const/4 v1, 0x0

    new-array v1, v1, [Ljava/security/ProtectionDomain;

    invoke-direct {v0, v1}, Ljava/security/AccessControlContext;-><init>([Ljava/security/ProtectionDomain;)V

    .line 253
    invoke-static {p0, p1, v0}, Lorg/apache/harmony/javax/security/auth/Subject;->doAs_PrivilegedExceptionAction(Lorg/apache/harmony/javax/security/auth/Subject;Ljava/security/PrivilegedExceptionAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;

    move-result-object v0

    .line 256
    :goto_0
    return-object v0

    :cond_0
    invoke-static {p0, p1, p2}, Lorg/apache/harmony/javax/security/auth/Subject;->doAs_PrivilegedExceptionAction(Lorg/apache/harmony/javax/security/auth/Subject;Ljava/security/PrivilegedExceptionAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;

    move-result-object v0

    goto :goto_0
.end method

.method private static doAs_PrivilegedAction(Lorg/apache/harmony/javax/security/auth/Subject;Ljava/security/PrivilegedAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;
    .locals 4
    .param p0, "subject"    # Lorg/apache/harmony/javax/security/auth/Subject;
    .param p1, "action"    # Ljava/security/PrivilegedAction;
    .param p2, "context"    # Ljava/security/AccessControlContext;

    .prologue
    .line 187
    if-nez p0, :cond_0

    .line 190
    const/4 v0, 0x0

    .line 195
    .local v0, "combiner":Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;
    :goto_0
    new-instance v1, Lorg/apache/harmony/javax/security/auth/Subject$1;

    invoke-direct {v1, p2, v0}, Lorg/apache/harmony/javax/security/auth/Subject$1;-><init>(Ljava/security/AccessControlContext;Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;)V

    .line 202
    .local v1, "dccAction":Ljava/security/PrivilegedAction;
    invoke-static {v1}, Ljava/security/AccessController;->doPrivileged(Ljava/security/PrivilegedAction;)Ljava/lang/Object;

    move-result-object v2

    check-cast v2, Ljava/security/AccessControlContext;

    .line 204
    .local v2, "newContext":Ljava/security/AccessControlContext;
    invoke-static {p1, v2}, Ljava/security/AccessController;->doPrivileged(Ljava/security/PrivilegedAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;

    move-result-object v3

    return-object v3

    .line 192
    .end local v0    # "combiner":Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;
    .end local v1    # "dccAction":Ljava/security/PrivilegedAction;
    .end local v2    # "newContext":Ljava/security/AccessControlContext;
    :cond_0
    new-instance v0, Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;

    invoke-direct {v0, p0}, Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;-><init>(Lorg/apache/harmony/javax/security/auth/Subject;)V

    .restart local v0    # "combiner":Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;
    goto :goto_0
.end method

.method private static doAs_PrivilegedExceptionAction(Lorg/apache/harmony/javax/security/auth/Subject;Ljava/security/PrivilegedExceptionAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;
    .locals 4
    .param p0, "subject"    # Lorg/apache/harmony/javax/security/auth/Subject;
    .param p1, "action"    # Ljava/security/PrivilegedExceptionAction;
    .param p2, "context"    # Ljava/security/AccessControlContext;
    .annotation system Ldalvik/annotation/Throws;
        value = {
            Ljava/security/PrivilegedActionException;
        }
    .end annotation

    .prologue
    .line 268
    if-nez p0, :cond_0

    .line 271
    const/4 v0, 0x0

    .line 276
    .local v0, "combiner":Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;
    :goto_0
    new-instance v1, Lorg/apache/harmony/javax/security/auth/Subject$2;

    invoke-direct {v1, p2, v0}, Lorg/apache/harmony/javax/security/auth/Subject$2;-><init>(Ljava/security/AccessControlContext;Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;)V

    .line 282
    .local v1, "dccAction":Ljava/security/PrivilegedAction;, "Ljava/security/PrivilegedAction<Ljava/security/AccessControlContext;>;"
    invoke-static {v1}, Ljava/security/AccessController;->doPrivileged(Ljava/security/PrivilegedAction;)Ljava/lang/Object;

    move-result-object v2

    check-cast v2, Ljava/security/AccessControlContext;

    .line 284
    .local v2, "newContext":Ljava/security/AccessControlContext;
    invoke-static {p1, v2}, Ljava/security/AccessController;->doPrivileged(Ljava/security/PrivilegedExceptionAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;

    move-result-object v3

    return-object v3

    .line 273
    .end local v0    # "combiner":Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;
    .end local v1    # "dccAction":Ljava/security/PrivilegedAction;, "Ljava/security/PrivilegedAction<Ljava/security/AccessControlContext;>;"
    .end local v2    # "newContext":Ljava/security/AccessControlContext;
    :cond_0
    new-instance v0, Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;

    invoke-direct {v0, p0}, Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;-><init>(Lorg/apache/harmony/javax/security/auth/Subject;)V

    .restart local v0    # "combiner":Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;
    goto :goto_0
.end method

.method public static getSubject(Ljava/security/AccessControlContext;)Lorg/apache/harmony/javax/security/auth/Subject;
    .locals 4
    .param p0, "context"    # Ljava/security/AccessControlContext;

    .prologue
    .line 488
    sget-object v2, Lorg/apache/harmony/javax/security/auth/Subject;->_SUBJECT:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-static {v2}, Lorg/apache/harmony/javax/security/auth/Subject;->checkPermission(Ljava/security/Permission;)V

    .line 489
    if-nez p0, :cond_0

    .line 490
    new-instance v2, Ljava/lang/NullPointerException;

    const-string v3, "auth.09"

    invoke-direct {v2, v3}, Ljava/lang/NullPointerException;-><init>(Ljava/lang/String;)V

    throw v2

    .line 492
    :cond_0
    new-instance v0, Lorg/apache/harmony/javax/security/auth/Subject$3;

    invoke-direct {v0, p0}, Lorg/apache/harmony/javax/security/auth/Subject$3;-><init>(Ljava/security/AccessControlContext;)V

    .line 497
    .local v0, "action":Ljava/security/PrivilegedAction;, "Ljava/security/PrivilegedAction<Ljava/security/DomainCombiner;>;"
    invoke-static {v0}, Ljava/security/AccessController;->doPrivileged(Ljava/security/PrivilegedAction;)Ljava/lang/Object;

    move-result-object v1

    check-cast v1, Ljava/security/DomainCombiner;

    .line 499
    .local v1, "combiner":Ljava/security/DomainCombiner;
    if-eqz v1, :cond_1

    instance-of v2, v1, Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;

    if-nez v2, :cond_2

    .line 500
    :cond_1
    const/4 v2, 0x0

    .line 502
    .end local v1    # "combiner":Ljava/security/DomainCombiner;
    :goto_0
    return-object v2

    .restart local v1    # "combiner":Ljava/security/DomainCombiner;
    :cond_2
    check-cast v1, Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;

    .end local v1    # "combiner":Ljava/security/DomainCombiner;
    invoke-virtual {v1}, Lorg/apache/harmony/javax/security/auth/SubjectDomainCombiner;->getSubject()Lorg/apache/harmony/javax/security/auth/Subject;

    move-result-object v2

    goto :goto_0
.end method

.method private readObject(Ljava/io/ObjectInputStream;)V
    .locals 2
    .param p1, "in"    # Ljava/io/ObjectInputStream;
    .annotation system Ldalvik/annotation/Throws;
        value = {
            Ljava/io/IOException;,
            Ljava/lang/ClassNotFoundException;
        }
    .end annotation

    .prologue
    .line 467
    invoke-virtual {p1}, Ljava/io/ObjectInputStream;->defaultReadObject()V

    .line 469
    new-instance v0, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    sget-object v1, Lorg/apache/harmony/javax/security/auth/Subject;->_PUBLIC_CREDENTIALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-direct {v0, p0, v1}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;-><init>(Lorg/apache/harmony/javax/security/auth/Subject;Lorg/apache/harmony/javax/security/auth/AuthPermission;)V

    iput-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->publicCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    .line 470
    new-instance v0, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    sget-object v1, Lorg/apache/harmony/javax/security/auth/Subject;->_PRIVATE_CREDENTIALS:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-direct {v0, p0, v1}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;-><init>(Lorg/apache/harmony/javax/security/auth/Subject;Lorg/apache/harmony/javax/security/auth/AuthPermission;)V

    iput-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->privateCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    .line 471
    return-void
.end method

.method private writeObject(Ljava/io/ObjectOutputStream;)V
    .locals 0
    .param p1, "out"    # Ljava/io/ObjectOutputStream;
    .annotation system Ldalvik/annotation/Throws;
        value = {
            Ljava/io/IOException;
        }
    .end annotation

    .prologue
    .line 474
    invoke-virtual {p1}, Ljava/io/ObjectOutputStream;->defaultWriteObject()V

    .line 475
    return-void
.end method


# virtual methods
.method public equals(Ljava/lang/Object;)Z
    .locals 5
    .param p1, "obj"    # Ljava/lang/Object;

    .prologue
    const/4 v1, 0x1

    const/4 v2, 0x0

    .line 301
    if-ne p0, p1, :cond_1

    .line 316
    :cond_0
    :goto_0
    return v1

    .line 305
    :cond_1
    if-eqz p1, :cond_2

    invoke-virtual {p0}, Ljava/lang/Object;->getClass()Ljava/lang/Class;

    move-result-object v3

    invoke-virtual {p1}, Ljava/lang/Object;->getClass()Ljava/lang/Class;

    move-result-object v4

    if-eq v3, v4, :cond_3

    :cond_2
    move v1, v2

    .line 306
    goto :goto_0

    :cond_3
    move-object v0, p1

    .line 309
    check-cast v0, Lorg/apache/harmony/javax/security/auth/Subject;

    .line 311
    .local v0, "that":Lorg/apache/harmony/javax/security/auth/Subject;
    iget-object v3, p0, Lorg/apache/harmony/javax/security/auth/Subject;->principals:Ljava/util/Set;

    iget-object v4, v0, Lorg/apache/harmony/javax/security/auth/Subject;->principals:Ljava/util/Set;

    invoke-interface {v3, v4}, Ljava/util/Set;->equals(Ljava/lang/Object;)Z

    move-result v3

    if-eqz v3, :cond_4

    .line 312
    iget-object v3, p0, Lorg/apache/harmony/javax/security/auth/Subject;->publicCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    iget-object v4, v0, Lorg/apache/harmony/javax/security/auth/Subject;->publicCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    invoke-virtual {v3, v4}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;->equals(Ljava/lang/Object;)Z

    move-result v3

    if-eqz v3, :cond_4

    .line 313
    iget-object v3, p0, Lorg/apache/harmony/javax/security/auth/Subject;->privateCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    iget-object v4, v0, Lorg/apache/harmony/javax/security/auth/Subject;->privateCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    invoke-virtual {v3, v4}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;->equals(Ljava/lang/Object;)Z

    move-result v3

    if-nez v3, :cond_0

    :cond_4
    move v1, v2

    .line 316
    goto :goto_0
.end method

.method public getPrincipals()Ljava/util/Set;
    .locals 1
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "()",
            "Ljava/util/Set",
            "<",
            "Ljava/security/Principal;",
            ">;"
        }
    .end annotation

    .prologue
    .line 325
    iget-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->principals:Ljava/util/Set;

    return-object v0
.end method

.method public getPrincipals(Ljava/lang/Class;)Ljava/util/Set;
    .locals 1
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "<T::",
            "Ljava/security/Principal;",
            ">(",
            "Ljava/lang/Class",
            "<TT;>;)",
            "Ljava/util/Set",
            "<TT;>;"
        }
    .end annotation

    .prologue
    .line 341
    .local p1, "c":Ljava/lang/Class;, "Ljava/lang/Class<TT;>;"
    iget-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->principals:Ljava/util/Set;

    check-cast v0, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    invoke-virtual {v0, p1}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;->get(Ljava/lang/Class;)Ljava/util/Set;

    move-result-object v0

    return-object v0
.end method

.method public getPrivateCredentials()Ljava/util/Set;
    .locals 1
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "()",
            "Ljava/util/Set",
            "<",
            "Ljava/lang/Object;",
            ">;"
        }
    .end annotation

    .prologue
    .line 350
    iget-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->privateCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    return-object v0
.end method

.method public getPrivateCredentials(Ljava/lang/Class;)Ljava/util/Set;
    .locals 1
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "<T:",
            "Ljava/lang/Object;",
            ">(",
            "Ljava/lang/Class",
            "<TT;>;)",
            "Ljava/util/Set",
            "<TT;>;"
        }
    .end annotation

    .prologue
    .line 365
    .local p1, "c":Ljava/lang/Class;, "Ljava/lang/Class<TT;>;"
    iget-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->privateCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    invoke-virtual {v0, p1}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;->get(Ljava/lang/Class;)Ljava/util/Set;

    move-result-object v0

    return-object v0
.end method

.method public getPublicCredentials()Ljava/util/Set;
    .locals 1
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "()",
            "Ljava/util/Set",
            "<",
            "Ljava/lang/Object;",
            ">;"
        }
    .end annotation

    .prologue
    .line 374
    iget-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->publicCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    return-object v0
.end method

.method public getPublicCredentials(Ljava/lang/Class;)Ljava/util/Set;
    .locals 1
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "<T:",
            "Ljava/lang/Object;",
            ">(",
            "Ljava/lang/Class",
            "<TT;>;)",
            "Ljava/util/Set",
            "<TT;>;"
        }
    .end annotation

    .prologue
    .line 390
    .local p1, "c":Ljava/lang/Class;, "Ljava/lang/Class<TT;>;"
    iget-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->publicCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    invoke-virtual {v0, p1}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;->get(Ljava/lang/Class;)Ljava/util/Set;

    move-result-object v0

    return-object v0
.end method

.method public hashCode()I
    .locals 2

    .prologue
    .line 400
    iget-object v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->principals:Ljava/util/Set;

    invoke-interface {v0}, Ljava/util/Set;->hashCode()I

    move-result v0

    iget-object v1, p0, Lorg/apache/harmony/javax/security/auth/Subject;->privateCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    invoke-virtual {v1}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;->hashCode()I

    move-result v1

    add-int/2addr v0, v1

    .line 401
    iget-object v1, p0, Lorg/apache/harmony/javax/security/auth/Subject;->publicCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    invoke-virtual {v1}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;->hashCode()I

    move-result v1

    .line 400
    add-int/2addr v0, v1

    return v0
.end method

.method public isReadOnly()Z
    .locals 1

    .prologue
    .line 422
    iget-boolean v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->readOnly:Z

    return v0
.end method

.method public setReadOnly()V
    .locals 1

    .prologue
    .line 411
    sget-object v0, Lorg/apache/harmony/javax/security/auth/Subject;->_READ_ONLY:Lorg/apache/harmony/javax/security/auth/AuthPermission;

    invoke-static {v0}, Lorg/apache/harmony/javax/security/auth/Subject;->checkPermission(Ljava/security/Permission;)V

    .line 413
    const/4 v0, 0x1

    iput-boolean v0, p0, Lorg/apache/harmony/javax/security/auth/Subject;->readOnly:Z

    .line 414
    return-void
.end method

.method public toString()Ljava/lang/String;
    .locals 6

    .prologue
    const/16 v5, 0xa

    .line 433
    new-instance v0, Ljava/lang/StringBuilder;

    const-string v4, "Subject:\n"

    invoke-direct {v0, v4}, Ljava/lang/StringBuilder;-><init>(Ljava/lang/String;)V

    .line 435
    .local v0, "buf":Ljava/lang/StringBuilder;
    iget-object v4, p0, Lorg/apache/harmony/javax/security/auth/Subject;->principals:Ljava/util/Set;

    invoke-interface {v4}, Ljava/util/Set;->iterator()Ljava/util/Iterator;

    move-result-object v2

    .line 436
    .local v2, "it":Ljava/util/Iterator;, "Ljava/util/Iterator<*>;"
    :goto_0
    invoke-interface {v2}, Ljava/util/Iterator;->hasNext()Z

    move-result v4

    if-nez v4, :cond_0

    .line 442
    iget-object v4, p0, Lorg/apache/harmony/javax/security/auth/Subject;->publicCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    invoke-virtual {v4}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;->iterator()Ljava/util/Iterator;

    move-result-object v2

    .line 443
    :goto_1
    invoke-interface {v2}, Ljava/util/Iterator;->hasNext()Z

    move-result v4

    if-nez v4, :cond_1

    .line 449
    invoke-virtual {v0}, Ljava/lang/StringBuilder;->length()I

    move-result v4

    add-int/lit8 v3, v4, -0x1

    .line 450
    .local v3, "offset":I
    iget-object v4, p0, Lorg/apache/harmony/javax/security/auth/Subject;->privateCredentials:Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;

    invoke-virtual {v4}, Lorg/apache/harmony/javax/security/auth/Subject$SecureSet;->iterator()Ljava/util/Iterator;

    move-result-object v2

    .line 452
    :goto_2
    :try_start_0
    invoke-interface {v2}, Ljava/util/Iterator;->hasNext()Z
    :try_end_0
    .catch Ljava/lang/SecurityException; {:try_start_0 .. :try_end_0} :catch_0

    move-result v4

    if-nez v4, :cond_2

    .line 461
    :goto_3
    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v4

    return-object v4

    .line 437
    .end local v3    # "offset":I
    :cond_0
    const-string v4, "\tPrincipal: "

    invoke-virtual {v0, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    .line 438
    invoke-interface {v2}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v4

    invoke-virtual {v0, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    .line 439
    invoke-virtual {v0, v5}, Ljava/lang/StringBuilder;->append(C)Ljava/lang/StringBuilder;

    goto :goto_0

    .line 444
    :cond_1
    const-string v4, "\tPublic Credential: "

    invoke-virtual {v0, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    .line 445
    invoke-interface {v2}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v4

    invoke-virtual {v0, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    .line 446
    invoke-virtual {v0, v5}, Ljava/lang/StringBuilder;->append(C)Ljava/lang/StringBuilder;

    goto :goto_1

    .line 453
    .restart local v3    # "offset":I
    :cond_2
    :try_start_1
    const-string v4, "\tPrivate Credential: "

    invoke-virtual {v0, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    .line 454
    invoke-interface {v2}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v4

    invoke-virtual {v0, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    .line 455
    const/16 v4, 0xa

    invoke-virtual {v0, v4}, Ljava/lang/StringBuilder;->append(C)Ljava/lang/StringBuilder;
    :try_end_1
    .catch Ljava/lang/SecurityException; {:try_start_1 .. :try_end_1} :catch_0

    goto :goto_2

    .line 457
    :catch_0
    move-exception v1

    .line 458
    .local v1, "e":Ljava/lang/SecurityException;
    invoke-virtual {v0}, Ljava/lang/StringBuilder;->length()I

    move-result v4

    invoke-virtual {v0, v3, v4}, Ljava/lang/StringBuilder;->delete(II)Ljava/lang/StringBuilder;

    .line 459
    const-string v4, "\tPrivate Credentials: no accessible information\n"

    invoke-virtual {v0, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    goto :goto_3
.end method
