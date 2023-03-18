Attribute VB_Name = "mdTlsNative"
'=========================================================================
'
' VbAsyncSocket Project (c) 2018-2022 by wqweto@gmail.com
'
' Simple and thin WinSock API wrappers for VB6
'
' This project is licensed under the terms of the MIT license
' See the LICENSE file in the project root for more information
'
'=========================================================================
Option Explicit
DefObj A-Z
Private Const MODULE_NAME As String = "mdTlsNative"

#Const ImplTlsServer = (ASYNCSOCKET_NO_TLSSERVER = 0)
#Const ImplUseShared = (ASYNCSOCKET_USE_SHARED <> 0)
#Const ImplUseDebugLog = (USE_DEBUG_LOG <> 0)
#Const ImplCaptureTraffic = CLng(ASYNCSOCKET_CAPTURE_TRAFFIC) '--- bitmask: 1 - traffic

'=========================================================================
' API
'=========================================================================

'--- for VirtualProtect
Private Const PAGE_EXECUTE_READWRITE                    As Long = &H40
'--- for AcquireCredentialsHandle
Private Const UNISP_NAME                                As String = "Microsoft Unified Security Protocol Provider"
Private Const SECPKG_CRED_INBOUND                       As Long = 1
Private Const SECPKG_CRED_OUTBOUND                      As Long = 2
Private Const SCHANNEL_CRED_VERSION                     As Long = 4
Private Const SCH_CREDENTIALS_VERSION                   As Long = 5
Private Const SP_PROT_TLS1_0                            As Long = &H40 Or &H80
Private Const SP_PROT_TLS1_1                            As Long = &H100 Or &H200
Private Const SP_PROT_TLS1_2                            As Long = &H400 Or &H800
Private Const SP_PROT_TLS1_3                            As Long = &H1000 Or &H2000
Private Const SCH_CRED_MANUAL_CRED_VALIDATION           As Long = 8
Private Const SCH_CRED_NO_DEFAULT_CREDS                 As Long = &H10
Private Const SCH_CRED_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT As Long = &H400
Private Const SCH_USE_STRONG_CRYPTO                     As Long = &H400000
'-- for InitializeSecurityContext
Private Const ISC_REQ_REPLAY_DETECT                     As Long = &H4
Private Const ISC_REQ_SEQUENCE_DETECT                   As Long = &H8
Private Const ISC_REQ_CONFIDENTIALITY                   As Long = &H10
Private Const ISC_REQ_USE_SUPPLIED_CREDS                As Long = &H80
Private Const ISC_REQ_ALLOCATE_MEMORY                   As Long = &H100
Private Const ISC_REQ_EXTENDED_ERROR                    As Long = &H4000
Private Const ISC_REQ_STREAM                            As Long = &H8000&
Private Const SECURITY_NATIVE_DREP                      As Long = &H10
'--- for ApiSecBuffer.BufferType
Private Const SECBUFFER_EMPTY                           As Long = 0   ' Undefined, replaced by provider
Private Const SECBUFFER_DATA                            As Long = 1   ' Packet data
Private Const SECBUFFER_TOKEN                           As Long = 2   ' Security token
Private Const SECBUFFER_EXTRA                           As Long = 5   ' Extra data
Private Const SECBUFFER_STREAM_TRAILER                  As Long = 6   ' Security Trailer
Private Const SECBUFFER_STREAM_HEADER                   As Long = 7   ' Security Header
Private Const SECBUFFER_ALERT                           As Long = 17
Private Const SECBUFFER_APPLICATION_PROTOCOLS           As Long = 18
Private Const SECBUFFER_VERSION                         As Long = 0
'--- SSPI/Schannel retvals
Private Const SEC_E_OK                                  As Long = 0
Private Const SEC_I_CONTINUE_NEEDED                     As Long = &H90312
Private Const SEC_I_CONTEXT_EXPIRED                     As Long = &H90317
Private Const SEC_I_INCOMPLETE_CREDENTIALS              As Long = &H90320
Private Const SEC_I_RENEGOTIATE                         As Long = &H90321
Private Const SEC_E_INVALID_HANDLE                      As Long = &H80090301
Private Const SEC_E_INCOMPLETE_MESSAGE                  As Long = &H80090318
Private Const SEC_E_CERT_UNKNOWN                        As Long = &H80090327
'--- for QueryContextAttributes
Private Const SECPKG_ATTR_STREAM_SIZES                  As Long = 4
Private Const SECPKG_ATTR_REMOTE_CERT_CONTEXT           As Long = &H53
Private Const SECPKG_ATTR_ISSUER_LIST_EX                As Long = &H59
Private Const SECPKG_ATTR_CONNECTION_INFO               As Long = &H5A
Private Const SECPKG_ATTR_CIPHER_INFO                   As Long = &H64
Private Const SECPKG_ATTR_APPLICATION_PROTOCOL          As Long = 35
'--- for ApplyControlToken
Private Const SCHANNEL_SHUTDOWN                         As Long = 1   ' gracefully close down a connection
'--- for CryptDecodeObjectEx
Private Const X509_ASN_ENCODING                         As Long = 1
Private Const PKCS_7_ASN_ENCODING                       As Long = &H10000
Private Const PKCS_RSA_PRIVATE_KEY                      As Long = 43
Private Const PKCS_PRIVATE_KEY_INFO                     As Long = 44
Private Const X509_ECC_PRIVATE_KEY                      As Long = 82
Private Const CRYPT_DECODE_NOCOPY_FLAG                  As Long = &H1
Private Const CRYPT_DECODE_ALLOC_FLAG                   As Long = &H8000
Private Const ERROR_FILE_NOT_FOUND                      As Long = 2
'--- for CertOpenStore
Private Const CERT_STORE_PROV_MEMORY                    As Long = 2
Private Const CERT_STORE_CREATE_NEW_FLAG                As Long = &H2000
'--- for CertAddEncodedCertificateToStore
Private Const CERT_STORE_ADD_USE_EXISTING               As Long = 2
'--- for CryptAcquireContext
Private Const PROV_RSA_FULL                             As Long = 1
Private Const CRYPT_NEWKEYSET                           As Long = &H8
Private Const CRYPT_DELETEKEYSET                        As Long = &H10
Private Const AT_KEYEXCHANGE                            As Long = 1
'--- for CertGetCertificateContextProperty
Private Const CERT_KEY_PROV_INFO_PROP_ID                As Long = 2
Private Const CERT_OCSP_RESPONSE_PROP_ID                As Long = 70
'--- for ALPN
Private Const SecApplicationProtocolNegotiationExt_ALPN As Long = 2
Private Const SecApplicationProtocolNegotiationStatus_Success As Long = 1
'--- OIDs
Private Const szOID_RSA_RSA                             As String = "1.2.840.113549.1.1.1"
Private Const szOID_ECC_PUBLIC_KEY                      As String = "1.2.840.10045.2.1"
Private Const szOID_ECC_CURVE_P256                      As String = "1.2.840.10045.3.1.7"
Private Const szOID_ECC_CURVE_P384                      As String = "1.3.132.0.34"
Private Const szOID_ECC_CURVE_P521                      As String = "1.3.132.0.35"
'--- NCrypt
Private Const BCRYPT_ECDSA_PRIVATE_P256_MAGIC           As Long = &H32534345
Private Const BCRYPT_ECDSA_PRIVATE_P384_MAGIC           As Long = &H34534345
Private Const BCRYPT_ECDSA_PRIVATE_P521_MAGIC           As Long = &H36534345
Private Const MS_KEY_STORAGE_PROVIDER                   As String = "Microsoft Software Key Storage Provider"
Private Const NCRYPTBUFFER_PKCS_KEY_NAME                As Long = 45
Private Const NCRYPT_OVERWRITE_KEY_FLAG                 As Long = &H80

Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal Length As Long)
Private Declare Function IsBadReadPtr Lib "kernel32" (ByVal lp As Long, ByVal ucb As Long) As Long
Private Declare Function VirtualProtect Lib "kernel32" (ByVal lpAddress As Long, ByVal dwSize As Long, ByVal flNewProtect As Long, ByRef lpflOldProtect As Long) As Long
Private Declare Function vbaObjSetAddref Lib "msvbvm60" Alias "__vbaObjSetAddref" (oDest As Any, ByVal lSrcPtr As Long) As Long
Private Declare Function lstrlenA Lib "kernel32" (ByVal lpStr As Long) As Long
Private Declare Function lstrlenW Lib "kernel32" (ByVal lpStr As Long) As Long
Private Declare Function LocalFree Lib "kernel32" (ByVal hMem As Long) As Long
Private Declare Function FormatMessage Lib "kernel32" Alias "FormatMessageW" (ByVal dwFlags As Long, ByVal lpSource As Long, ByVal dwMessageId As Long, ByVal dwLanguageId As Long, ByVal lpBuffer As Long, ByVal nSize As Long, ByVal Args As Long) As Long
'--- msvbvm60
Private Declare Function ArrPtr Lib "msvbvm60" Alias "VarPtr" (Ptr() As Any) As Long
'--- version
Private Declare Function GetFileVersionInfo Lib "version" Alias "GetFileVersionInfoW" (ByVal lptstrFilename As Long, ByVal dwHandle As Long, ByVal dwLen As Long, lpData As Any) As Long
Private Declare Function VerQueryValue Lib "version" Alias "VerQueryValueW" (pBlock As Any, ByVal lpSubBlock As Long, lpBuffer As Any, puLen As Long) As Long
'--- security
Private Declare Function AcquireCredentialsHandle Lib "security" Alias "AcquireCredentialsHandleW" (ByVal pszPrincipal As Long, ByVal pszPackage As Long, ByVal fCredentialUse As Long, ByVal pvLogonId As Long, pAuthData As Any, ByVal pGetKeyFn As Long, ByVal pvGetKeyArgument As Long, phCredential As Currency, ByVal ptsExpiry As Long) As Long
Private Declare Function FreeCredentialsHandle Lib "security" (phContext As Currency) As Long
Private Declare Function AcceptSecurityContext Lib "security" (phCredential As Currency, ByVal phContext As Long, pInput As Any, ByVal fContextReq As Long, ByVal TargetDataRep As Long, phNewContext As Currency, pOutput As Any, pfContextAttr As Long, ByVal ptsExpiry As Long) As Long
Private Declare Function InitializeSecurityContext Lib "security" Alias "InitializeSecurityContextW" (phCredential As Currency, ByVal phContext As Long, ByVal pszTargetName As Long, ByVal fContextReq As Long, ByVal Reserved1 As Long, ByVal TargetDataRep As Long, pInput As Any, ByVal Reserved2 As Long, phNewContext As Currency, pOutput As Any, pfContextAttr As Long, ByVal ptsExpiry As Long) As Long
Private Declare Function DeleteSecurityContext Lib "security" (phContext As Currency) As Long
Private Declare Function FreeContextBuffer Lib "security" (ByVal pvContextBuffer As Long) As Long
Private Declare Function QueryContextAttributes Lib "security" Alias "QueryContextAttributesW" (phContext As Currency, ByVal ulAttribute As Long, pBuffer As Any) As Long
Private Declare Function DecryptMessage Lib "security" (phContext As Currency, pMessage As Any, ByVal MessageSeqNo As Long, ByVal pfQOP As Long) As Long
Private Declare Function EncryptMessage Lib "security" (phContext As Currency, ByVal fQOP As Long, pMessage As Any, ByVal MessageSeqNo As Long) As Long
Private Declare Function ApplyControlToken Lib "security" (phContext As Currency, pInput As Any) As Long
'--- crypt32
Private Declare Function CryptDecodeObjectEx Lib "crypt32" (ByVal dwCertEncodingType As Long, ByVal lpszStructType As Any, pbEncoded As Any, ByVal cbEncoded As Long, ByVal dwFlags As Long, ByVal pDecodePara As Long, pvStructInfo As Any, pcbStructInfo As Long) As Long
Private Declare Function CertOpenStore Lib "crypt32" (ByVal lpszStoreProvider As Long, ByVal dwEncodingType As Long, ByVal hCryptProv As Long, ByVal dwFlags As Long, ByVal pvPara As Long) As Long
Private Declare Function CertCloseStore Lib "crypt32" (ByVal hCertStore As Long, ByVal dwFlags As Long) As Long
Private Declare Function CertAddEncodedCertificateToStore Lib "crypt32" (ByVal hCertStore As Long, ByVal dwCertEncodingType As Long, pbCertEncoded As Any, ByVal cbCertEncoded As Long, ByVal dwAddDisposition As Long, ByVal ppCertContext As Long) As Long
Private Declare Function CertSetCertificateContextProperty Lib "crypt32" (ByVal pCertContext As Long, ByVal dwPropId As Long, ByVal dwFlags As Long, pvData As Any) As Long
Private Declare Function CertFreeCertificateContext Lib "crypt32" (ByVal pCertContext As Long) As Long
Private Declare Function CertEnumCertificatesInStore Lib "crypt32" (ByVal hCertStore As Long, ByVal pPrevCertContext As Long) As Long
Private Declare Function CertGetCertificateContextProperty Lib "crypt32" (ByVal pCertContext As Long, ByVal dwPropId As Long, pvData As Any, pcbData As Long) As Long
Private Declare Function CryptStringToBinary Lib "crypt32" Alias "CryptStringToBinaryW" (ByVal pszString As Long, ByVal cchString As Long, ByVal dwFlags As Long, ByVal pbBinary As Long, pcbBinary As Long, Optional ByVal pdwSkip As Long, Optional ByVal pdwFlags As Long) As Long
'--- advapi32
Private Declare Function CryptAcquireContext Lib "advapi32" Alias "CryptAcquireContextW" (phProv As Long, ByVal pszContainer As Long, ByVal pszProvider As Long, ByVal dwProvType As Long, ByVal dwFlags As Long) As Long
Private Declare Function CryptReleaseContext Lib "advapi32" (ByVal hProv As Long, ByVal dwFlags As Long) As Long
Private Declare Function CryptImportKey Lib "advapi32" (ByVal hProv As Long, pbData As Any, ByVal dwDataLen As Long, ByVal hPubKey As Long, ByVal dwFlags As Long, phKey As Long) As Long
Private Declare Function CryptDestroyKey Lib "advapi32" (ByVal hKey As Long) As Long
Private Declare Function RtlGenRandom Lib "advapi32" Alias "SystemFunction036" (RandomBuffer As Any, ByVal RandomBufferLength As Long) As Long
'--- ncrypt
Private Declare Function NCryptOpenStorageProvider Lib "ncrypt" (phProvider As Long, ByVal pszProviderName As Long, ByVal dwFlags As Long) As Long
Private Declare Function NCryptImportKey Lib "ncrypt" (ByVal hProvider As Long, ByVal hImportKey As Long, ByVal pszBlobType As Long, pParameterList As Any, phKey As Long, pbData As Any, ByVal cbData As Long, ByVal dwFlags As Long) As Long
Private Declare Function NCryptFreeObject Lib "ncrypt" (ByVal hObject As Long) As Long

Private Type SCHANNEL_CRED
    dwVersion               As Long
    cCreds                  As Long
    paCred                  As Long
    hRootStore              As Long
    cMappers                As Long
    aphMappers              As Long
    cSupportedAlgs          As Long
    palgSupportedAlgs       As Long
    grbitEnabledProtocols   As Long
    dwMinimumCipherStrength As Long
    dwMaximumCipherStrength As Long
    dwSessionLifespan       As Long
    dwFlags                 As Long
    dwCredFormat            As Long
End Type

Private Type SCH_CREDENTIALS
    dwVersion               As Long
    dwCredFormat            As Long
    cCreds                  As Long
    paCred                  As Long
    hRootStore              As Long
    cMappers                As Long
    aphMappers              As Long
    dwSessionLifespan       As Long
    dwFlags                 As Long
    cTlsParameters          As Long
    pTlsParameters          As Long
End Type

Private Type TLS_PARAMETERS
    cAlpnIds                As Long
    rgstrAlpnIds            As Long
    grbitDisabledProtocols  As Long
    cDisabledCrypto         As Long
    pDisabledCrypto         As Long
    dwFlags                 As Long
End Type

Private Type ApiSecBuffer
    cbBuffer                As Long
    BufferType              As Long
    pvBuffer                As Long
End Type

Private Type ApiSecBufferDesc
    ulVersion               As Long
    cBuffers                As Long
    pBuffers                As Long
End Type

Private Type ApiSecPkgContext_StreamSizes
    cbHeader                As Long
    cbTrailer               As Long
    cbMaximumMessage        As Long
    cBuffers                As Long
    cbBlockSize             As Long
End Type

Private Type CRYPT_KEY_PROV_INFO
    pwszContainerName   As Long
    pwszProvName        As Long
    dwProvType          As Long
    dwFlags             As Long
    cProvParam          As Long
    rgProvParam         As Long
    dwKeySpec           As Long
End Type

Private Type BCRYPT_ECCKEY_BLOB
    dwMagic             As Long
    cbKey               As Long
    Buffer(0 To 1000)   As Byte
End Type

Private Type CRYPT_DATA_BLOB
    cbData              As Long
    pbData              As Long
End Type

Private Type CRYPT_BIT_BLOB
    cbData              As Long
    pbData              As Long
    cUnusedBits         As Long
End Type

Private Type CRYPT_ALGORITHM_IDENTIFIER
    pszObjId            As Long
    Parameters          As CRYPT_DATA_BLOB
End Type

Private Type CERT_PUBLIC_KEY_INFO
    Algorithm           As CRYPT_ALGORITHM_IDENTIFIER
    PublicKey           As CRYPT_BIT_BLOB
End Type

Private Type CRYPT_ECC_PRIVATE_KEY_INFO
    dwVersion           As Long
    PrivateKey          As CRYPT_DATA_BLOB
    szCurveOid          As Long
    PublicKey           As CRYPT_DATA_BLOB
End Type

Private Type CRYPT_PRIVATE_KEY_INFO
    dwVersion           As Long
    Algorithm           As CRYPT_ALGORITHM_IDENTIFIER
    PrivateKey          As CRYPT_DATA_BLOB
    pAttributes         As Long
End Type

Private Type CERT_CONTEXT
    dwCertEncodingType  As Long
    pbCertEncoded       As Long
    cbCertEncoded       As Long
    pCertInfo           As Long
    hCertStore          As Long
End Type

Private Type SecPkgContext_IssuerListInfoEx
    aIssuers            As Long
    cIssuers            As Long
End Type

Private Type SecPkgContext_ConnectionInfo
    dwProtocol          As Long
    aiCipher            As Long
    dwCipherStrength    As Long
    aiHash              As Long
    dwHashStrength      As Long
    aiExch              As Long
    dwExchStrength      As Long
End Type

Private Const SZ_ALG_MAX_SIZE  As Long = 64
Private Type SecPkgContext_CipherInfo
    dwVersion           As Long
    dwProtocol          As Long
    dwCipherSuite       As Long
    dwBaseCipherSuite   As Long
    szCipherSuite(0 To SZ_ALG_MAX_SIZE - 1) As Integer
    szCipher(0 To SZ_ALG_MAX_SIZE - 1) As Integer
    dwCipherLen         As Long
    dwCipherBlockLen    As Long
    szHash(0 To SZ_ALG_MAX_SIZE - 1) As Integer
    dwHashLen           As Long
    szExchange(0 To SZ_ALG_MAX_SIZE - 1) As Integer
    dwMinExchangeLen    As Long
    dwMaxExchangeLen    As Long
    szCertificate(0 To SZ_ALG_MAX_SIZE - 1) As Integer
    dwKeyType           As Long
End Type

Private Const MAX_PROTOCOL_ID_SIZE As Long = &HFF&
Private Type SecPkgContext_ApplicationProtocol
    ProtoNegoStatus     As Long
    ProtoNegoExt        As Long
    ProtocolIdSize      As Byte
    ProtocolId(0 To MAX_PROTOCOL_ID_SIZE) As Byte
End Type

'=========================================================================
' Constants and member variables
'=========================================================================

Private Const STR_VL_ALERTS                 As String = "0|Close notify|10|Unexpected message|20|Bad record mac|21|Decryption failed|22|Record overflow|30|Decompression failure|40|Handshake failure|41|No certificate|42|Bad certificate|43|Unsupported certificate|44|Certificate revoked|45|Certificate expired|46|Certificate unknown|47|Illegal parameter|48|Unknown certificate authority|50|Decode error|51|Decrypt error|70|Protocol version|71|Insufficient security|80|Internal error|90|User canceled|100|No renegotiation|109|Missing extension|110|Unsupported expension|112|Unrecognized name|116|Certificate required|120|No application protocol"
Private Const STR_UNKNOWN                   As String = "Unknown (%1)"
Private Const STR_FORMAT_ALERT              As String = "%1."
'--- errors
Private Const ERR_UNEXPECTED_RESULT         As String = "Unexpected result from %1 (%2)"
Private Const ERR_CONNECTION_CLOSED         As String = "Connection closed"
Private Const ERR_UNKNOWN_ECC_PRIVKEY       As String = "Unknown ECC private key (%1)"
Private Const ERR_UNKNOWN_PUBKEY            As String = "Unknown public key (%1)"
Private Const ERR_NO_SERVER_COMPILED        As String = "Server-side TLS not compiled (ASYNCSOCKET_NO_TLSSERVER = 1)"
'--- numeric
Private Const TLS_CONTENT_TYPE_ALERT        As Long = 21
Private Const LNG_FACILITY_WIN32            As Long = &H80070000

Private Enum UcsTlsLocalFeaturesEnum '--- bitmask
    ucsTlsSupportTls10 = 2 ^ 0
    ucsTlsSupportTls11 = 2 ^ 1
    ucsTlsSupportTls12 = 2 ^ 2
    ucsTlsSupportTls13 = 2 ^ 3
    ucsTlsIgnoreServerCertificateErrors = 2 ^ 4
    ucsTlsSupportAll = ucsTlsSupportTls10 Or ucsTlsSupportTls11 Or ucsTlsSupportTls12 Or ucsTlsSupportTls13
End Enum

Private Enum UcsTlsStatesEnum
    ucsTlsStateNew = 0
    ucsTlsStateClosed = 1
    ucsTlsStateHandshakeStart = 2
    ucsTlsStatePostHandshake = 8
    ucsTlsStateShutdown = 9
End Enum

Private Enum UcsTlsAlertDescriptionsEnum
    uscTlsAlertCloseNotify = 0
    uscTlsAlertUnexpectedMessage = 10
    uscTlsAlertBadRecordMac = 20
    uscTlsAlertHandshakeFailure = 40
    uscTlsAlertBadCertificate = 42
    uscTlsAlertCertificateRevoked = 44
    uscTlsAlertCertificateExpired = 45
    uscTlsAlertCertificateUnknown = 46
    uscTlsAlertIllegalParameter = 47
    uscTlsAlertUnknownCa = 48
    uscTlsAlertDecodeError = 50
    uscTlsAlertDecryptError = 51
    uscTlsAlertProtocolVersion = 70
    uscTlsAlertInternalError = 80
    uscTlsAlertUserCanceled = 90
    uscTlsAlertMissingExtension = 109
    uscTlsAlertUnrecognizedName = 112
    uscTlsAlertCertificateRequired = 116
    uscTlsAlertNoApplicationProtocol = 120
End Enum

#If Not ImplUseShared Then
Private Enum UcsOsVersionEnum
    ucsOsvNt4 = 400
    ucsOsvWin98 = 410
    ucsOsvWin2000 = 500
    ucsOsvXp = 501
    ucsOsvVista = 600
    ucsOsvWin7 = 601
    ucsOsvWin8 = 602
    [ucsOsvWin8.1] = 603
    ucsOsvWin10 = 1000
End Enum
#End If

Public Type UcsTlsContext
    '--- config
    IsServer            As Boolean
    RemoteHostName      As String
    LocalFeatures       As UcsTlsLocalFeaturesEnum
    ClientCertCallback  As Long
    AlpnProtocols       As String
    '--- state
    State               As UcsTlsStatesEnum
    LastErrNumber       As Long
    LastError           As String
    LastErrSource       As String
    LastAlertCode       As UcsTlsAlertDescriptionsEnum
    AlpnNegotiated      As String
    SniRequested        As String
    '--- handshake
    LocalCertificates   As Collection
    LocalPrivateKey     As Collection
    RemoteCertificates  As Collection
    RemoteCertStatuses  As Collection
    '--- SSPI
    ContextReq          As Long
    hTlsCredentials     As Currency
    hTlsContext         As Currency
    TlsSizes            As ApiSecPkgContext_StreamSizes
    InDesc              As ApiSecBufferDesc
    InBuffers()         As ApiSecBuffer
    OutDesc             As ApiSecBufferDesc
    OutBuffers()        As ApiSecBuffer
    '--- I/O buffers
    RecvBuffer()        As Byte
    RecvPos             As Long
#If ImplCaptureTraffic <> 0 Then
    TrafficDump         As Collection
#End If
End Type

Private Type UcsKeyInfo
    AlgoObjId           As String
    KeyBlob()           As Byte
    BitLen              As Long
End Type

Public g_oRequestSocket             As Object

'=========================================================================
' Error handling
'=========================================================================

Private Sub ErrRaise(ByVal Number As Long, Optional Source As Variant, Optional Description As Variant)
    Err.Raise Number, Source, Description
End Sub

'=========================================================================
' Properties
'=========================================================================

Public Property Get TlsIsClosed(uCtx As UcsTlsContext) As Boolean
    TlsIsClosed = (uCtx.State = ucsTlsStateClosed)
End Property

Public Property Get TlsIsStarted(uCtx As UcsTlsContext) As Boolean
    TlsIsStarted = (uCtx.State > ucsTlsStateClosed)
End Property

Public Property Get TlsIsReady(uCtx As UcsTlsContext) As Boolean
    TlsIsReady = (uCtx.State >= ucsTlsStatePostHandshake)
End Property

Public Property Get TlsIsShutdown(uCtx As UcsTlsContext) As Boolean
    TlsIsShutdown = (uCtx.State = ucsTlsStateShutdown)
End Property

'=========================================================================
' TLS support
'=========================================================================

Public Function TlsInitClient( _
            uCtx As UcsTlsContext, _
            Optional RemoteHostName As String, _
            Optional ByVal LocalFeatures As Long = ucsTlsSupportAll, _
            Optional ClientCertCallback As Object, _
            Optional AlpnProtocols As String) As Boolean
    Dim uEmpty          As UcsTlsContext
    
    On Error GoTo EH
    With uEmpty
        pvTlsClearLastError uEmpty
        .State = ucsTlsStateHandshakeStart
        .RemoteHostName = RemoteHostName
        .LocalFeatures = LocalFeatures
        .ClientCertCallback = ObjPtr(ClientCertCallback)
        If RealOsVersion >= [ucsOsvWin8.1] Then
            .AlpnProtocols = AlpnProtocols
        End If
        #If ImplCaptureTraffic <> 0 Then
            Set .TrafficDump = New Collection
        #End If
    End With
    uCtx = uEmpty
    '--- success
    TlsInitClient = True
    Exit Function
EH:
    pvTlsSetLastError uCtx, Err.Number, Err.Source, Err.Description
End Function

Public Function TlsInitServer( _
            uCtx As UcsTlsContext, _
            Optional RemoteHostName As String, _
            Optional Certificates As Collection, _
            Optional PrivateKey As Collection, _
            Optional AlpnProtocols As String, _
            Optional ByVal LocalFeatures As Long = ucsTlsSupportAll) As Boolean
#If Not ImplTlsServer Then
    ErrRaise vbObjectError, , ERR_NO_SERVER_COMPILED
#Else
    Dim uEmpty          As UcsTlsContext
    
    On Error GoTo EH
    With uEmpty
        pvTlsClearLastError uEmpty
        .IsServer = True
        .State = ucsTlsStateHandshakeStart
        .RemoteHostName = RemoteHostName
        .LocalFeatures = LocalFeatures
        Set .LocalCertificates = Certificates
        Set .LocalPrivateKey = PrivateKey
        If RealOsVersion >= [ucsOsvWin8.1] Then
            .AlpnProtocols = AlpnProtocols
        End If
        #If ImplCaptureTraffic <> 0 Then
            Set .TrafficDump = New Collection
        #End If
    End With
    uCtx = uEmpty
    '--- success
    TlsInitServer = True
    Exit Function
EH:
    pvTlsSetLastError uCtx, Err.Number, Err.Source, Err.Description
#End If
End Function

Public Function TlsTerminate(uCtx As UcsTlsContext)
    With uCtx
        .State = ucsTlsStateClosed
        If .hTlsContext <> 0 Then
            Call DeleteSecurityContext(.hTlsContext)
            .hTlsContext = 0
        End If
        If .hTlsCredentials <> 0 Then
            Call FreeCredentialsHandle(.hTlsCredentials)
            .hTlsCredentials = 0
        End If
    End With
End Function

Public Function TlsHandshake(uCtx As UcsTlsContext, baInput() As Byte, ByVal lSize As Long, baOutput() As Byte, lOutputPos As Long) As Boolean
    Const FUNC_NAME     As String = "TlsHandshake"
    Dim uCred           As SCHANNEL_CRED
    Dim uNewCred        As SCH_CREDENTIALS
    Dim uNewParams      As TLS_PARAMETERS
    Dim lContextAttr    As Long
    Dim hResult         As Long
    Dim lIdx            As Long
    Dim lPtr            As Long
    Dim oCallback       As Object
    Dim sKeyName        As String
    Dim pCertContext    As Long
    Dim aCred(0 To 0)   As Long
    Dim uIssuerInfo     As SecPkgContext_IssuerListInfoEx
    Dim uIssuerList()   As CRYPT_DATA_BLOB
    Dim cIssuers        As Collection
    Dim baCaDn()        As Byte
    Dim uCertContext    As CERT_CONTEXT
    Dim sApiSource      As String
    Dim uConnInfo       As SecPkgContext_ConnectionInfo
    Dim uCipherInfo     As SecPkgContext_CipherInfo
    Dim baAlpnBuffer()  As Byte
    Dim uAppProtocol    As SecPkgContext_ApplicationProtocol
    
    On Error GoTo EH
    With uCtx
        If .State = ucsTlsStateClosed Then
            pvTlsSetLastError uCtx, vbObjectError, MODULE_NAME & "." & FUNC_NAME, ERR_CONNECTION_CLOSED
            GoTo QH
        End If
        pvTlsClearLastError uCtx
        If .ContextReq = 0 Then
            .ContextReq = .ContextReq Or ISC_REQ_REPLAY_DETECT              ' Detect replayed messages that have been encoded by using the EncryptMessage or MakeSignature functions.
            .ContextReq = .ContextReq Or ISC_REQ_SEQUENCE_DETECT            ' Detect messages received out of sequence.
            .ContextReq = .ContextReq Or ISC_REQ_CONFIDENTIALITY            ' Encrypt messages by using the EncryptMessage function.
            .ContextReq = .ContextReq Or ISC_REQ_ALLOCATE_MEMORY            ' The security package allocates output buffers for you. When you have finished using the output buffers, free them by calling the FreeContextBuffer function.
            .ContextReq = .ContextReq Or ISC_REQ_EXTENDED_ERROR             ' When errors occur, the remote party will be notified.
            .ContextReq = .ContextReq Or ISC_REQ_STREAM                     ' Support a stream-oriented connection.
        End If
        If lSize < 0 Then
            lSize = pvArraySize(baInput)
        End If
        If lSize > 0 Then
            .RecvPos = pvWriteBuffer(.RecvBuffer, .RecvPos, VarPtr(baInput(0)), lSize)
        End If
        '--- note: doesn't work for encrypted alerts
        If lSize = 7 Then
            If baInput(0) = TLS_CONTENT_TYPE_ALERT Then
                .LastAlertCode = baInput(6)
            End If
        End If
RetryCredentials:
        If .hTlsCredentials = 0 Then
            uCred.dwVersion = SCHANNEL_CRED_VERSION
            uCred.grbitEnabledProtocols = IIf((.LocalFeatures And ucsTlsSupportTls10) <> 0, SP_PROT_TLS1_0, 0) Or _
                IIf((.LocalFeatures And ucsTlsSupportTls11) <> 0, SP_PROT_TLS1_1, 0) Or _
                IIf((.LocalFeatures And ucsTlsSupportTls12) <> 0, SP_PROT_TLS1_2, 0)
            uCred.dwFlags = uCred.dwFlags Or SCH_CRED_MANUAL_CRED_VALIDATION    ' Prevent Schannel from validating the received server certificate chain.
            uCred.dwFlags = uCred.dwFlags Or SCH_CRED_NO_DEFAULT_CREDS          ' Prevent Schannel from attempting to automatically supply a certificate chain for client authentication.
            uCred.dwFlags = uCred.dwFlags Or SCH_CRED_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT ' Force TLS certificate status request extension (commonly known as OCSP stapling) to be sent on Vista or later
            If pvCollectionCount(.LocalCertificates) > 0 Then
                If pvTlsImportToCertStore(.LocalCertificates, .LocalPrivateKey, sKeyName, pCertContext) And pCertContext <> 0 Then
                    aCred(uCred.cCreds) = pCertContext
                    uCred.cCreds = uCred.cCreds + 1
                    uCred.paCred = VarPtr(aCred(0))
                    .ContextReq = .ContextReq Or ISC_REQ_USE_SUPPLIED_CREDS     ' Schannel must not attempt to supply credentials for the client automatically.
                End If
            End If
            If RealOsVersion(BuildNo:=lIdx) = ucsOsvWin10 And lIdx >= 20348 Then   '--- 20348 = Windows Server 2022
                '--- use new credentials struct for TLS 1.3 support
                uNewCred.dwVersion = SCH_CREDENTIALS_VERSION
                uNewCred.cCreds = uCred.cCreds
                uNewCred.paCred = uCred.paCred
                uNewCred.dwFlags = uCred.dwFlags Or SCH_USE_STRONG_CRYPTO
                uNewCred.cTlsParameters = 1
                uNewCred.pTlsParameters = VarPtr(uNewParams)
                uNewParams.grbitDisabledProtocols = Not (uCred.grbitEnabledProtocols Or _
                    IIf((.LocalFeatures And ucsTlsSupportTls13) <> 0, SP_PROT_TLS1_3, 0))
                hResult = AcquireCredentialsHandle(0, StrPtr(UNISP_NAME), IIf(.IsServer, SECPKG_CRED_INBOUND, SECPKG_CRED_OUTBOUND), 0, uNewCred, 0, 0, .hTlsCredentials, 0)
            Else
                hResult = -1
            End If
            If hResult < 0 Then
                hResult = AcquireCredentialsHandle(0, StrPtr(UNISP_NAME), IIf(.IsServer, SECPKG_CRED_INBOUND, SECPKG_CRED_OUTBOUND), 0, uCred, 0, 0, .hTlsCredentials, 0)
            End If
            If hResult < 0 Then
                pvTlsSetLastError uCtx, hResult, MODULE_NAME & "." & FUNC_NAME & vbCrLf & "AcquireCredentialsHandle", AlertCode:=.LastAlertCode
                GoTo QH
            End If
            If pCertContext <> 0 Then
                Call CertFreeCertificateContext(pCertContext)
                pCertContext = 0
            End If
        End If
        If .hTlsContext = 0 Then
            pvInitSecDesc .InDesc, 3, .InBuffers
            pvInitSecDesc .OutDesc, 3, .OutBuffers
            #If ImplTlsServer Then
                If .IsServer Then
                    pvTlsParseHandshakeClientHello uCtx, baInput, 0
                End If
            #End If
            If LenB(.AlpnProtocols) <> 0 Then
                pvTlsBuildAlpnBuffer baAlpnBuffer, 0, .AlpnProtocols
            End If
        End If
        Do
            If .RecvPos > 0 Then
                #If (ImplCaptureTraffic And 1) <> 0 Then
                    .TrafficDump.Add FUNC_NAME & ".Input" & vbCrLf & TlsDesignDumpArray(.RecvBuffer, 0, .RecvPos)
                #End If
                pvInitSecBuffer .InBuffers(0), SECBUFFER_TOKEN, VarPtr(.RecvBuffer(0)), .RecvPos
                lPtr = VarPtr(.InDesc)
            Else
                lPtr = 0
            End If
            If pvArraySize(baAlpnBuffer) > 0 Then
                pvInitSecBuffer .InBuffers(IIf(lPtr <> 0, 1, 0)), SECBUFFER_APPLICATION_PROTOCOLS, VarPtr(baAlpnBuffer(0)), UBound(baAlpnBuffer) + 1
                lPtr = VarPtr(.InDesc)
            End If
            #If ImplTlsServer Then
                If .IsServer Then
                    hResult = AcceptSecurityContext(.hTlsCredentials, IIf(.hTlsContext <> 0, VarPtr(.hTlsContext), 0), ByVal lPtr, .ContextReq, _
                        SECURITY_NATIVE_DREP, .hTlsContext, .OutDesc, lContextAttr, 0)
                    sApiSource = "AcceptSecurityContext"
                Else
            #End If
                    hResult = InitializeSecurityContext(.hTlsCredentials, IIf(.hTlsContext <> 0, VarPtr(.hTlsContext), 0), StrPtr(.RemoteHostName), .ContextReq, 0, _
                        SECURITY_NATIVE_DREP, ByVal lPtr, 0, .hTlsContext, .OutDesc, lContextAttr, 0)
                    sApiSource = "InitializeSecurityContext"
            #If ImplTlsServer Then
                End If
            #End If
            If hResult = SEC_E_INCOMPLETE_MESSAGE Then
                pvInitSecBuffer .InBuffers(1), SECBUFFER_EMPTY
                Exit Do
            ElseIf hResult < 0 Then
                pvTlsSetLastError uCtx, hResult, MODULE_NAME & "." & FUNC_NAME & vbCrLf & sApiSource, AlertCode:=.LastAlertCode
                '--- treat as warnings TLS1_ALERT_BAD_CERTIFICATE, TLS1_ALERT_UNSUPPORTED_CERT and TLS1_ALERT_CERTIFICATE_UNKNOWN
                If hResult = SEC_E_CERT_UNKNOWN Then
                    TlsHandshake = True
                End If
                GoTo QH
            Else
                .RecvPos = 0
                For lIdx = 1 To UBound(.InBuffers)
                    With .InBuffers(lIdx)
                        If .cbBuffer > 0 Then
                            Select Case .BufferType
                            Case SECBUFFER_EXTRA
                                lPtr = .pvBuffer
                                If lPtr = 0 Then
                                    lPtr = VarPtr(uCtx.RecvBuffer(uCtx.InBuffers(0).cbBuffer - .cbBuffer))
                                End If
                                uCtx.RecvPos = pvWriteBuffer(uCtx.RecvBuffer, uCtx.RecvPos, lPtr, .cbBuffer)
                            Case SECBUFFER_ALERT
                                #If ImplUseDebugLog Then
                                    DebugLog MODULE_NAME, FUNC_NAME, "InBuffers, SECBUFFER_ALERT:" & vbCrLf & TlsDesignDumpMemory(.pvBuffer, .cbBuffer), vbLogEventTypeWarning
                                #End If
                            Case Else
                                #If ImplUseDebugLog Then
                                    DebugLog MODULE_NAME, FUNC_NAME, ".BufferType(" & lIdx & ")=" & .BufferType
                                #End If
                            End Select
                        End If
                    End With
                    pvInitSecBuffer .InBuffers(lIdx), SECBUFFER_EMPTY
                Next
                Erase baAlpnBuffer
                For lIdx = 0 To UBound(.OutBuffers)
                    With .OutBuffers(lIdx)
                        If .cbBuffer > 0 Then
                            Select Case .BufferType
                            Case SECBUFFER_TOKEN
                                lOutputPos = pvWriteBuffer(baOutput, lOutputPos, .pvBuffer, .cbBuffer)
                                #If (ImplCaptureTraffic And 1) <> 0 Then
                                    uCtx.TrafficDump.Add FUNC_NAME & ".Output" & vbCrLf & TlsDesignDumpMemory(.pvBuffer, .cbBuffer)
                                #End If
                            Case SECBUFFER_ALERT
                                #If ImplUseDebugLog Then
                                    DebugLog MODULE_NAME, FUNC_NAME, "OutBuffers, SECBUFFER_ALERT:" & vbCrLf & TlsDesignDumpMemory(.pvBuffer, .cbBuffer), vbLogEventTypeWarning
                                #End If
                            End Select
                            If .pvBuffer <> 0 Then
                                Call FreeContextBuffer(.pvBuffer)
                                Debug.Assert Err.LastDllError = 0
                            End If
                        End If
                    End With
                    pvInitSecBuffer .OutBuffers(lIdx), SECBUFFER_EMPTY
                Next
                Select Case hResult
                Case SEC_I_CONTINUE_NEEDED
                    '--- do nothing
                Case SEC_E_OK
                    hResult = QueryContextAttributes(.hTlsContext, SECPKG_ATTR_STREAM_SIZES, .TlsSizes)
                    If hResult < 0 Then
                        pvTlsSetLastError uCtx, hResult, MODULE_NAME & "." & FUNC_NAME & vbCrLf & "QueryContextAttributes(SECPKG_ATTR_STREAM_SIZES)", AlertCode:=.LastAlertCode
                        GoTo QH
                    End If
                    pvInitSecDesc .InDesc, .TlsSizes.cBuffers, .InBuffers
                    pvInitSecDesc .OutDesc, .TlsSizes.cBuffers, .OutBuffers
                    If QueryContextAttributes(.hTlsContext, SECPKG_ATTR_REMOTE_CERT_CONTEXT, pCertContext) = 0 And pCertContext <> 0 Then
                        Call CopyMemory(uCertContext, ByVal pCertContext, Len(uCertContext))
                        If Not pvTlsExportFromCertStore(uCertContext.hCertStore, .RemoteCertificates, .RemoteCertStatuses) Then
                            GoTo QH
                        End If
                        Call CertFreeCertificateContext(pCertContext)
                        pCertContext = 0
                    End If
                    If LenB(.AlpnProtocols) <> 0 Then
                        .AlpnNegotiated = vbNullString
                        If QueryContextAttributes(.hTlsContext, SECPKG_ATTR_APPLICATION_PROTOCOL, uAppProtocol) = 0 Then
                            If uAppProtocol.ProtoNegoStatus = SecApplicationProtocolNegotiationStatus_Success Then
                                uAppProtocol.ProtocolId(uAppProtocol.ProtocolIdSize) = 0
                                .AlpnNegotiated = pvToStringA(VarPtr(uAppProtocol.ProtocolId(0)))
                            End If
                        End If
                    End If
                    #If ImplUseDebugLog Then
                        If QueryContextAttributes(.hTlsContext, SECPKG_ATTR_CIPHER_INFO, uCipherInfo) = 0 Then
                            DebugLog MODULE_NAME, FUNC_NAME, "Using " & pvToStringW(VarPtr(uCipherInfo.szCipherSuite(0))) & " (&H" & Hex$(uCipherInfo.dwCipherSuite) & ") from " & .RemoteHostName
                        End If
                        If QueryContextAttributes(.hTlsContext, SECPKG_ATTR_CONNECTION_INFO, uConnInfo) = 0 Then
                            DebugLog MODULE_NAME, FUNC_NAME, pvTlsGetAlgName(uConnInfo.dwProtocol) & " using " & _
                                pvTlsGetAlgName(uConnInfo.aiCipher) & " cipher with " & _
                                pvTlsGetAlgName(uConnInfo.aiHash) & " hash and " & _
                                pvTlsGetAlgName(uConnInfo.aiExch) & " key-exchange" & _
                                IIf(LenB(.AlpnNegotiated) <> 0, " over " & .AlpnNegotiated & " (ALPN)", vbNullString) & _
                                IIf(LenB(.SniRequested) <> 0, " for " & .SniRequested & " (SNI)", vbNullString)
                        End If
                    #End If
                    .State = ucsTlsStatePostHandshake
                    Exit Do
                Case SEC_I_INCOMPLETE_CREDENTIALS
                    If .ClientCertCallback <> 0 Then
                        hResult = QueryContextAttributes(.hTlsContext, SECPKG_ATTR_ISSUER_LIST_EX, uIssuerInfo)
                        If hResult < 0 Then
                            pvTlsSetLastError uCtx, hResult, MODULE_NAME & "." & FUNC_NAME & vbCrLf & "QueryContextAttributes(SECPKG_ATTR_ISSUER_LIST_EX)", AlertCode:=.LastAlertCode
                            GoTo QH
                        End If
                        Set cIssuers = New Collection
                        If uIssuerInfo.cIssuers > 0 Then
                            ReDim uIssuerList(0 To uIssuerInfo.cIssuers - 1) As CRYPT_DATA_BLOB
                            Debug.Assert uIssuerInfo.aIssuers <> 0
                            Call CopyMemory(uIssuerList(0), ByVal uIssuerInfo.aIssuers, uIssuerInfo.cIssuers * Len(uIssuerList(0)))
                            For lIdx = 0 To UBound(uIssuerList)
                                pvWriteBuffer baCaDn, 0, uIssuerList(lIdx).pbData, uIssuerList(lIdx).cbData
                                pvArrayReallocate baCaDn, uIssuerList(lIdx).cbData, FUNC_NAME & ".baCaDn"
                                cIssuers.Add baCaDn
                            Next
                        End If
                        Call vbaObjSetAddref(oCallback, .ClientCertCallback)
                        If oCallback.FireOnCertificate(cIssuers) Then
                            Call FreeCredentialsHandle(.hTlsCredentials)
                            .hTlsCredentials = 0
                        End If
                    ElseIf (.ContextReq And ISC_REQ_USE_SUPPLIED_CREDS) = 0 Then
                        .ContextReq = .ContextReq Or ISC_REQ_USE_SUPPLIED_CREDS
                    End If
                    GoTo RetryCredentials
                Case SEC_I_CONTEXT_EXPIRED
                    .State = ucsTlsStateShutdown
                    Exit Do
                Case Else
                    pvTlsSetLastError uCtx, vbObjectError, MODULE_NAME & "." & FUNC_NAME & vbCrLf & sApiSource, _
                        Replace(Replace(ERR_UNEXPECTED_RESULT, "%1", sApiSource), "%2", "&H" & Hex$(hResult)), AlertCode:=.LastAlertCode
                    GoTo QH
                End Select
                If .RecvPos = 0 Then
                    Exit Do
                End If
            End If
        Loop
    End With
    '--- success
    TlsHandshake = True
QH:
    If pCertContext <> 0 Then
        Call CertFreeCertificateContext(pCertContext)
    End If
    If LenB(sKeyName) Then
        Call CryptAcquireContext(0, StrPtr(sKeyName), 0, PROV_RSA_FULL, CRYPT_DELETEKEYSET)
    End If
    Exit Function
EH:
    pvTlsSetLastError uCtx, Err.Number, Err.Source, Err.Description
    Resume QH
End Function

Public Function TlsReceive(uCtx As UcsTlsContext, baInput() As Byte, ByVal lSize As Long, baPlainText() As Byte, lPos As Long, baOutput() As Byte, lOutputPos As Long) As Boolean
    Const FUNC_NAME     As String = "TlsReceive"
    Dim hResult         As Long
    Dim lIdx            As Long
    Dim lPtr            As Long
    Dim baEmpty()       As Byte
    
    On Error GoTo EH
    With uCtx
        If .State = ucsTlsStateClosed Then
            pvTlsSetLastError uCtx, vbObjectError, MODULE_NAME & "." & FUNC_NAME, ERR_CONNECTION_CLOSED
            GoTo QH
        End If
        pvTlsClearLastError uCtx
        If lSize < 0 Then
            lSize = pvArraySize(baInput)
        End If
        If lSize > 0 Then
            .RecvPos = pvWriteBuffer(.RecvBuffer, .RecvPos, VarPtr(baInput(0)), lSize)
        End If
        Do
            If .RecvPos > 0 Then
                lPtr = VarPtr(.RecvBuffer(0))
                #If (ImplCaptureTraffic And 1) <> 0 Then
                    .TrafficDump.Add FUNC_NAME & ".Input" & vbCrLf & TlsDesignDumpArray(.RecvBuffer, 0, .RecvPos)
                #End If
            Else
                lPtr = VarPtr(.RecvPos)
            End If
            pvInitSecBuffer .InBuffers(0), SECBUFFER_DATA, lPtr, .RecvPos
            hResult = DecryptMessage(.hTlsContext, .InDesc, 0, 0)
            If hResult = SEC_E_INCOMPLETE_MESSAGE Then
                pvInitSecBuffer .InBuffers(1), SECBUFFER_EMPTY
                Exit Do
            ElseIf hResult = SEC_E_INVALID_HANDLE And .RecvPos = 0 Then
                '--- session on hTlsContext already closed so don't call pvTlsSetLastError
                Exit Do
            ElseIf hResult < 0 Then
                pvTlsSetLastError uCtx, hResult, MODULE_NAME & "." & FUNC_NAME & vbCrLf & "DecryptMessage"
                GoTo QH
            End If
            .RecvPos = 0
            For lIdx = 1 To UBound(.InBuffers)
                With .InBuffers(lIdx)
                    If .cbBuffer > 0 Then
                        Select Case .BufferType
                        Case SECBUFFER_DATA
                            lPos = pvWriteBuffer(baPlainText, lPos, .pvBuffer, .cbBuffer)
                        Case SECBUFFER_EXTRA
                            lPtr = .pvBuffer
                            If lPtr = 0 Then
                                lPtr = VarPtr(uCtx.RecvBuffer(uCtx.InBuffers(0).cbBuffer - .cbBuffer))
                            End If
                            uCtx.RecvPos = pvWriteBuffer(uCtx.RecvBuffer, uCtx.RecvPos, lPtr, .cbBuffer)
                        Case SECBUFFER_ALERT
                            #If ImplUseDebugLog Then
                                DebugLog MODULE_NAME, FUNC_NAME, "InBuffers, SECBUFFER_ALERT:" & vbCrLf & TlsDesignDumpMemory(.pvBuffer, .cbBuffer), vbLogEventTypeWarning
                            #End If
                        Case SECBUFFER_STREAM_HEADER, SECBUFFER_STREAM_TRAILER
                            '--- do nothing
                        Case Else
                            #If ImplUseDebugLog Then
                                DebugLog MODULE_NAME, FUNC_NAME, ".BufferType(" & lIdx & ")=" & .BufferType
                            #End If
                        End Select
                    End If
                End With
                pvInitSecBuffer .InBuffers(lIdx), SECBUFFER_EMPTY
            Next
            Select Case hResult
            Case SEC_E_OK
                '--- do nothing
            Case SEC_I_RENEGOTIATE
                .State = ucsTlsStateHandshakeStart
                '--- .RecvBuffer is populated already
                If Not TlsHandshake(uCtx, baEmpty, 0, baOutput, lOutputPos) Then
                    GoTo QH
                End If
            Case SEC_I_CONTEXT_EXPIRED
                .State = ucsTlsStateShutdown
                Exit Do
            Case Else
                pvTlsSetLastError uCtx, vbObjectError, MODULE_NAME & "." & FUNC_NAME & vbCrLf & "DecryptMessage", _
                    Replace(Replace(ERR_UNEXPECTED_RESULT, "%1", "DecryptMessage"), "%2", "&H" & Hex$(hResult))
                GoTo QH
            End Select
            If .RecvPos = 0 Then
                Exit Do
            End If
        Loop
    End With
    '--- success
    TlsReceive = True
QH:
    Exit Function
EH:
    pvTlsSetLastError uCtx, Err.Number, Err.Source, Err.Description
    Resume QH
End Function

Public Function TlsSend(uCtx As UcsTlsContext, baPlainText() As Byte, ByVal lSize As Long, baOutput() As Byte, lOutputPos As Long) As Boolean
    Const FUNC_NAME     As String = "TlsSend"
    Dim hResult         As Long
    Dim lBufPos         As Long
    Dim lBufSize        As Long
    Dim lPos            As Long
    Dim lIdx            As Long
    
    On Error GoTo EH
    With uCtx
        If .State = ucsTlsStateClosed Then
            pvTlsSetLastError uCtx, vbObjectError, MODULE_NAME & "." & FUNC_NAME, ERR_CONNECTION_CLOSED
            GoTo QH
        End If
        pvTlsClearLastError uCtx
        '--- figure out upper bound of total output and reserve space in baOutput
        lIdx = (lSize + .TlsSizes.cbMaximumMessage - 1) \ .TlsSizes.cbMaximumMessage
        pvWriteReserved baOutput, lOutputPos, .TlsSizes.cbHeader * lIdx + lSize + .TlsSizes.cbTrailer * lIdx
        For lPos = 0 To lSize - 1 Step .TlsSizes.cbMaximumMessage
            lBufPos = lOutputPos
            lBufSize = lSize - lPos
            If lBufSize > .TlsSizes.cbMaximumMessage Then
                lBufSize = .TlsSizes.cbMaximumMessage
            End If
            pvWriteReserved baOutput, lOutputPos, .TlsSizes.cbHeader + lBufSize + .TlsSizes.cbTrailer
            pvInitSecBuffer .InBuffers(0), SECBUFFER_STREAM_HEADER, VarPtr(baOutput(lBufPos)), .TlsSizes.cbHeader
            lBufPos = lBufPos + .TlsSizes.cbHeader
            Debug.Assert UBound(baPlainText) + 1 >= lPos + lBufSize
            Call CopyMemory(baOutput(lBufPos), baPlainText(lPos), lBufSize)
            pvInitSecBuffer .InBuffers(1), SECBUFFER_DATA, VarPtr(baOutput(lBufPos)), lBufSize
            lBufPos = lBufPos + lBufSize
            pvInitSecBuffer .InBuffers(2), SECBUFFER_STREAM_TRAILER, VarPtr(baOutput(lBufPos)), .TlsSizes.cbTrailer
            For lIdx = 3 To UBound(.InBuffers)
                pvInitSecBuffer .InBuffers(lIdx), SECBUFFER_EMPTY
            Next
            hResult = EncryptMessage(.hTlsContext, 0, .InDesc, 0)
            If hResult < 0 Then
                pvTlsSetLastError uCtx, hResult, MODULE_NAME & "." & FUNC_NAME & vbCrLf & "EncryptMessage"
                GoTo QH
            End If
            #If (ImplCaptureTraffic And 1) <> 0 Then
                .TrafficDump.Add FUNC_NAME & ".Output" & vbCrLf & TlsDesignDumpArray(baOutput, lOutputPos, .InBuffers(0).cbBuffer + .InBuffers(1).cbBuffer + .InBuffers(2).cbBuffer)
            #End If
            '--- note: use cbBuffer's as returned by EncryptMessage because trailing MAC might be trimmed (shorter than initial .TlsSizes.cbTrailer)
            lOutputPos = lOutputPos + .InBuffers(0).cbBuffer + .InBuffers(1).cbBuffer + .InBuffers(2).cbBuffer
            For lIdx = 1 To UBound(.InBuffers)
                With .InBuffers(lIdx)
                    If .cbBuffer > 0 Then
                        Select Case .BufferType
                        Case SECBUFFER_ALERT
                            #If ImplUseDebugLog Then
                                DebugLog MODULE_NAME, FUNC_NAME, "InBuffers, SECBUFFER_ALERT:" & vbCrLf & TlsDesignDumpMemory(.pvBuffer, .cbBuffer), vbLogEventTypeWarning
                            #End If
                        Case SECBUFFER_DATA, SECBUFFER_STREAM_HEADER, SECBUFFER_STREAM_TRAILER
                            '--- do nothing
                        Case Else
                            #If ImplUseDebugLog Then
                                DebugLog MODULE_NAME, FUNC_NAME, ".BufferType(" & lIdx & ")=" & .BufferType
                            #End If
                        End Select
                    End If
                End With
                pvInitSecBuffer .InBuffers(lIdx), SECBUFFER_EMPTY
            Next
            Select Case hResult
            Case SEC_E_OK
                '--- do nothing
            Case Else
                pvTlsSetLastError uCtx, vbObjectError, MODULE_NAME & "." & FUNC_NAME & vbCrLf & "EncryptMessage", _
                    Replace(Replace(ERR_UNEXPECTED_RESULT, "%1", "EncryptMessage"), "%2", "&H" & Hex$(hResult))
                GoTo QH
            End Select
        Next
    End With
    '--- success
    TlsSend = True
QH:
    Exit Function
EH:
    pvTlsSetLastError uCtx, Err.Number, Err.Source, Err.Description
    Resume QH
End Function

Public Function TlsShutdown(uCtx As UcsTlsContext, baOutput() As Byte, lPos As Long) As Boolean
    Const FUNC_NAME     As String = "pvTlsShutdown"
    Dim lType           As Long
    Dim hResult         As Long
    Dim lIdx            As Long
    Dim sApiSource      As String
    Dim lContextAttr    As Long
    
    On Error GoTo QH
    With uCtx
        If .State = ucsTlsStateClosed Or .State = ucsTlsStateShutdown Then
            '--- success
            TlsShutdown = True
            GoTo QH
        End If
        lType = SCHANNEL_SHUTDOWN
        pvInitSecBuffer .InBuffers(0), SECBUFFER_TOKEN, VarPtr(lType), 4
        '--- note: passing more than one input buffer fails w/ SEC_E_INVALID_TOKEN (&H80090308)
        .InDesc.cBuffers = 1
        hResult = ApplyControlToken(.hTlsContext, .InDesc)
        .InDesc.cBuffers = .TlsSizes.cBuffers
        If hResult < 0 Then
            pvTlsSetLastError uCtx, hResult, MODULE_NAME & "." & FUNC_NAME & vbCrLf & "ApplyControlToken"
            GoTo QH
        End If
        pvInitSecBuffer .OutBuffers(0), SECBUFFER_TOKEN
        For lIdx = 1 To UBound(.OutBuffers)
            pvInitSecBuffer .OutBuffers(lIdx), SECBUFFER_EMPTY
        Next
        #If ImplTlsServer Then
            If .IsServer Then
                hResult = AcceptSecurityContext(.hTlsCredentials, VarPtr(.hTlsContext), ByVal 0, .ContextReq, _
                    SECURITY_NATIVE_DREP, .hTlsContext, .OutDesc, lContextAttr, 0)
                sApiSource = "AcceptSecurityContext"
            Else
        #End If
                hResult = InitializeSecurityContext(.hTlsCredentials, VarPtr(.hTlsContext), StrPtr(.RemoteHostName), .ContextReq, 0, _
                    SECURITY_NATIVE_DREP, ByVal 0, 0, .hTlsContext, .OutDesc, lContextAttr, 0)
                sApiSource = "InitializeSecurityContext"
        #If ImplTlsServer Then
            End If
        #End If
        If hResult < 0 Then
            pvTlsSetLastError uCtx, hResult, MODULE_NAME & "." & FUNC_NAME & vbCrLf & sApiSource
            GoTo QH
        End If
        For lIdx = 0 To UBound(.OutBuffers)
            With .OutBuffers(lIdx)
                If .BufferType = SECBUFFER_TOKEN And .cbBuffer > 0 Then
                    #If (ImplCaptureTraffic And 1) <> 0 Then
                        uCtx.TrafficDump.Add FUNC_NAME & ".Output" & vbCrLf & TlsDesignDumpMemory(.pvBuffer, .cbBuffer)
                    #End If
                    lPos = pvWriteBuffer(baOutput, lPos, .pvBuffer, .cbBuffer)
                End If
                If .pvBuffer <> 0 Then
                    Call FreeContextBuffer(.pvBuffer)
                    Debug.Assert Err.LastDllError = 0
                    .pvBuffer = 0
                End If
            End With
        Next
        .State = ucsTlsStateShutdown
    End With
    '--- success
    TlsShutdown = True
QH:
    Exit Function
EH:
    pvTlsSetLastError uCtx, Err.Number, Err.Source, Err.Description
    Resume QH
End Function

Public Function TlsGetLastError(uCtx As UcsTlsContext, Optional LastErrNumber As Long, Optional LastErrSource As String) As String
    LastErrNumber = uCtx.LastErrNumber
    LastErrSource = uCtx.LastErrSource
    TlsGetLastError = uCtx.LastError
    If uCtx.LastAlertCode <> -1 Then
        TlsGetLastError = IIf(LenB(TlsGetLastError) <> 0, TlsGetLastError & ". ", vbNullString) & Replace(STR_FORMAT_ALERT, "%1", pvTlsGetLastAlert(uCtx))
    End If
End Function

Private Sub pvTlsClearLastError(uCtx As UcsTlsContext)
    With uCtx
        .LastErrNumber = 0
        .LastErrSource = vbNullString
        .LastError = vbNullString
        .LastAlertCode = 0
    End With
End Sub

Private Sub pvTlsSetLastError( _
            uCtx As UcsTlsContext, _
            Optional ByVal ErrNumber As Long, _
            Optional ErrSource As String, _
            Optional ErrDescription As String, _
            Optional ByVal AlertCode As Long = -1)
    Const FUNC_NAME     As String = "pvTlsSetLastError"
    
    With uCtx
        .LastErrNumber = ErrNumber
        .LastErrSource = ErrSource
        .LastAlertCode = AlertCode
        If ErrNumber <> 0 And LenB(ErrDescription) = 0 Then
            uCtx.LastError = GetSystemMessage(ErrNumber)
            If LenB(.LastError) = 0 Then
                .LastError = "Error &H" & Hex$(ErrNumber)
            End If
        Else
            .LastError = ErrDescription
        End If
        If Right$(.LastError, 2) = vbCrLf Then
            .LastError = Left$(.LastError, Len(.LastError) - 2)
        End If
        If Right$(.LastError, 1) = "." Then
            .LastError = Left$(.LastError, Len(.LastError) - 1)
        End If
        If Left$(.LastError, 16) = "Automation error" Then
            .LastError = Mid$(.LastError, 17)
        End If
        If .LastErrNumber <> 0 Then
            .State = ucsTlsStateClosed
        End If
        #If ImplCaptureTraffic <> 0 Then
            Clipboard.Clear
            Clipboard.SetText TlsConcatCollection(.TrafficDump, vbCrLf)
            #If ImplUseDebugLog Then
                DebugLog MODULE_NAME, FUNC_NAME, "Traffic dump copied to clipboard"
            #End If
        #End If
    End With
End Sub

Private Function pvTlsGetLastAlert(uCtx As UcsTlsContext, Optional AlertCode As Long) As String
    Static vTexts       As Variant
    
    AlertCode = uCtx.LastAlertCode
    If AlertCode >= 0 Then
        If IsEmpty(vTexts) Then
            vTexts = SplitOrReindex(STR_VL_ALERTS, "|")
        End If
        If AlertCode <= UBound(vTexts) Then
            pvTlsGetLastAlert = vTexts(AlertCode)
        End If
        If LenB(pvTlsGetLastAlert) = 0 Then
            pvTlsGetLastAlert = Replace(STR_UNKNOWN, "%1", AlertCode)
        End If
    End If
End Function

#If ImplUseDebugLog Then
Private Function pvTlsGetAlgName(ByVal lAlgId As Long) As String
    Select Case lAlgId
    Case &H20&
        pvTlsGetAlgName = "SSL3_CLIENT"
    Case &H80&
        pvTlsGetAlgName = "TLS1_0_CLIENT"
    Case &H200&
        pvTlsGetAlgName = "TLS1_1_CLIENT"
    Case &H800&
        pvTlsGetAlgName = "TLS1_2_CLIENT"
    Case &H2000&
        pvTlsGetAlgName = "TLS1_3_CLIENT"
    Case &H10&
        pvTlsGetAlgName = "SSL3_SERVER"
    Case &H40&
        pvTlsGetAlgName = "TLS1_0_SERVER"
    Case &H100&
        pvTlsGetAlgName = "TLS1_1_SERVER"
    Case &H400&
        pvTlsGetAlgName = "TLS1_2_SERVER"
    Case &H1000
        pvTlsGetAlgName = "TLS1_3_SERVER"
    Case &H6602&
        pvTlsGetAlgName = "RC2"
    Case &H6801&
        pvTlsGetAlgName = "RC4"
    Case &H6601&
        pvTlsGetAlgName = "DES"
    Case &H6603&
        pvTlsGetAlgName = "3DES"
    Case &H660E&
        pvTlsGetAlgName = "AES_128"
    Case &H660F&
        pvTlsGetAlgName = "AES_192"
    Case &H6610&
        pvTlsGetAlgName = "AES_256"
    Case &H8001&
        pvTlsGetAlgName = "MD2"
    Case &H8003&
        pvTlsGetAlgName = "MD5"
    Case &H8004&
        pvTlsGetAlgName = "SHA1"
    Case &H800C&
        pvTlsGetAlgName = "SHA_256"
    Case &H800D&
        pvTlsGetAlgName = "SHA_384"
    Case &H800E&
        pvTlsGetAlgName = "SHA_512"
    Case &HA400&
        pvTlsGetAlgName = "RSA_KEYX"
    Case &H2400&
        pvTlsGetAlgName = "RSA_SIGN"
    Case &HAA02&
        pvTlsGetAlgName = "DH_EPHEM"
    Case &HAA05&
        pvTlsGetAlgName = "ECDH"
    Case &HAE06&
        pvTlsGetAlgName = "ECDH_EPHEM"
    Case Else
        pvTlsGetAlgName = "&H" & Hex$(lAlgId)
    End Select
End Function
#End If

Private Function pvTlsBuildAlpnBuffer(baOutput() As Byte, ByVal lPos As Long, sAlpnProtocols As String) As Long
    Dim vElem           As Variant
    Dim sProtocol       As String
    Dim lSize           As Long
    
    lPos = pvWriteReserved(baOutput, 0, 4)
    lPos = pvWriteBuffer(baOutput, lPos, VarPtr(SecApplicationProtocolNegotiationExt_ALPN), 4)
    lPos = pvWriteReserved(baOutput, lPos, 2)
    For Each vElem In Split(sAlpnProtocols, "|")
        vElem = Left$(vElem, 255)
        lSize = Len(vElem)
        lPos = pvWriteBuffer(baOutput, lPos, VarPtr(lSize), 1)
        sProtocol = StrConv(vElem, vbFromUnicode)
        lPos = pvWriteBuffer(baOutput, lPos, StrPtr(sProtocol), Len(vElem))
    Next
    pvWriteBuffer baOutput, 8, VarPtr(lPos - 10), 2
    pvWriteBuffer baOutput, 0, VarPtr(lPos - 4), 4
    pvTlsBuildAlpnBuffer = lPos
End Function

Private Function pvTlsParseHandshakeClientHello(uCtx As UcsTlsContext, baInput() As Byte, ByVal lPos As Long) As Long
    Const TLS_CONTENT_TYPE_HANDSHAKE                As Long = 22
    Const TLS_HANDSHAKE_TYPE_CLIENT_HELLO           As Long = 1
    Dim lValue          As Long
    Dim lSize           As Long
    Dim lEnd            As Long
    Dim baTemp()        As Byte
    Dim lExtType        As Long
    Dim lExtSize        As Long
    Dim lNamePos        As Long
    Dim lNameType       As Long
    Dim lNameSize       As Long
    
    lPos = pvReadLong(baInput, lPos, lValue)            '--- content type
    If lValue <> TLS_CONTENT_TYPE_HANDSHAKE Then
        GoTo QH
    End If
    lPos = pvReadLong(baInput, lPos, lValue, Size:=2)   '--- protocol version
    lPos = lPos + 2                                     '--- skip handshake message size
    lPos = pvReadLong(baInput, lPos, lValue)            '--- handshake type
    If lValue <> TLS_HANDSHAKE_TYPE_CLIENT_HELLO Then
        GoTo QH
    End If
    lPos = lPos + 3                                     '--- skip size of client hello
    lPos = lPos + 2                                     '--- skip Client Version
    lPos = lPos + 32                                    '--- skip Client Random
    lPos = pvReadLong(baInput, lPos, lSize, Size:=1)    '--- skip Session ID
    lPos = lPos + lSize
    lPos = pvReadLong(baInput, lPos, lSize, Size:=2)    '--- skip Cipher Suites
    lPos = lPos + lSize
    lPos = pvReadLong(baInput, lPos, lSize, Size:=1)    '--- skip Compression Methods
    lPos = lPos + lSize
    lPos = pvReadLong(baInput, lPos, lSize, Size:=2)    '--- size of Extensions
    lEnd = lPos + lSize
    Do While lPos < lEnd And lPos <= UBound(baInput)
        lPos = pvReadLong(baInput, lPos, lExtType, Size:=2)
        lPos = pvReadLong(baInput, lPos, lExtSize, Size:=2)
        Select Case lExtType
        Case 0 '--- Extension -- Server Name
            lNamePos = pvReadLong(baInput, lPos, lValue, Size:=2)
            Do While lNamePos < lPos + lValue
                lNamePos = pvReadLong(baInput, lNamePos, lNameType, Size:=1)
                lNamePos = pvReadLong(baInput, lNamePos, lNameSize, Size:=2)
                If lNameType = 0 Then '--- FQDN
                    lNamePos = pvReadArray(baInput, lNamePos, baTemp, lNameSize)
                    uCtx.SniRequested = StrConv(baTemp, vbUnicode)
                Else
                    lNamePos = lNamePos + lNameSize
                End If
            Loop
        End Select
        lPos = lPos + lExtSize
    Loop
QH:
    pvTlsParseHandshakeClientHello = lPos
End Function

Private Function pvTlsImportToCertStore(cCerts As Collection, cPrivKey As Collection, sOutKeyName As String, pOutCertContext As Long) As Boolean
    Const FUNC_NAME     As String = "pvTlsImportToCertStore"
    Const IDX_KEYNAME   As Long = 1
    Const IDX_PROVNAME  As Long = 2
    Const IDX_PROVTYPE  As Long = 3
    Const IDX_KEYSPEC   As Long = 4
    Dim hCertStore      As Long
    Dim lIdx            As Long
    Dim baCert()        As Byte
    Dim pCertContext    As Long
    Dim baPrivKey()     As Byte
    Dim sProvName       As String
    Dim sKeyName        As String
    Dim hProv           As Long
    Dim hKey            As Long
    Dim lPtr            As Long
    Dim uPrivKeyInfo    As UcsKeyInfo
    Dim uPublicKeyInfo  As CERT_PUBLIC_KEY_INFO
    Dim uProvInfo       As CRYPT_KEY_PROV_INFO
    Dim uEccBlob        As BCRYPT_ECCKEY_BLOB
    Dim lBlobSize       As Long
    Dim hNProv          As Long
    Dim hNKey           As Long
    Dim uDesc           As ApiSecBufferDesc
    Dim uBuffers()      As ApiSecBuffer
    Dim hResult         As Long
    Dim sApiSource      As String
    
    '--- load server X.509 certificates to an in-memory certificate store
    hCertStore = CertOpenStore(CERT_STORE_PROV_MEMORY, 0, 0, CERT_STORE_CREATE_NEW_FLAG, 0)
    If hCertStore = 0 Then
        hResult = Err.LastDllError
        sApiSource = "CertOpenStore"
        GoTo QH
    End If
    For lIdx = 1 To pvCollectionCount(cCerts)
        baCert = cCerts.Item(lIdx)
        If CertAddEncodedCertificateToStore(hCertStore, X509_ASN_ENCODING, baCert(0), UBound(baCert) + 1, CERT_STORE_ADD_USE_EXISTING, IIf(lIdx = 1, VarPtr(pCertContext), 0)) = 0 Then
            hResult = Err.LastDllError
            sApiSource = "CertAddEncodedCertificateToStore"
            GoTo QH
        End If
    Next
    If pCertContext <> 0 Then
        If cPrivKey.Count > 1 Then
            With cPrivKey
                sKeyName = .Item(IDX_KEYNAME)
                sProvName = .Item(IDX_PROVNAME)
                uProvInfo.pwszContainerName = StrPtr(sKeyName)
                uProvInfo.pwszProvName = StrPtr(sProvName)
                If .Count > IDX_PROVNAME Then
                    uProvInfo.dwProvType = .Item(IDX_PROVTYPE)
                    uProvInfo.dwKeySpec = .Item(IDX_KEYSPEC)
                End If
            End With
        ElseIf SearchCollection(cPrivKey, 1, RetVal:=baPrivKey) Then
            sKeyName = "VbAsyncSocket" & pvGetRandomString()
            If Not pvAsn1DecodePrivateKey(baPrivKey, uPrivKeyInfo) Then
                GoTo QH
            End If
            Call CopyMemory(lPtr, ByVal UnsignedAdd(pCertContext, 12), 4)       '--- dereference pCertContext->pCertInfo
            lPtr = UnsignedAdd(lPtr, 56)                                        '--- &pCertContext->pCertInfo->SubjectPublicKeyInfo
            Call CopyMemory(uPublicKeyInfo, ByVal lPtr, Len(uPublicKeyInfo))
            Select Case pvToStringA(uPublicKeyInfo.Algorithm.pszObjId)
            Case szOID_RSA_RSA
                uProvInfo.pwszContainerName = StrPtr(sKeyName)
                uProvInfo.dwProvType = PROV_RSA_FULL
                uProvInfo.dwKeySpec = AT_KEYEXCHANGE
                If CryptAcquireContext(hProv, uProvInfo.pwszContainerName, uProvInfo.pwszProvName, uProvInfo.dwProvType, uProvInfo.dwFlags) = 0 Then
                    If CryptAcquireContext(hProv, uProvInfo.pwszContainerName, uProvInfo.pwszProvName, uProvInfo.dwProvType, uProvInfo.dwFlags Or CRYPT_NEWKEYSET) = 0 Then
                        hResult = Err.LastDllError
                        sApiSource = "CryptAcquireContext"
                        GoTo QH
                    End If
                    sOutKeyName = sKeyName
                End If
                If CryptImportKey(hProv, uPrivKeyInfo.KeyBlob(0), UBound(uPrivKeyInfo.KeyBlob) + 1, 0, 0, hKey) = 0 Then
                    hResult = Err.LastDllError
                    sApiSource = "CryptImportKey"
                    GoTo QH
                End If
            Case szOID_ECC_PUBLIC_KEY
                Select Case uPrivKeyInfo.AlgoObjId
                Case szOID_ECC_CURVE_P256
                    uEccBlob.dwMagic = BCRYPT_ECDSA_PRIVATE_P256_MAGIC
                Case szOID_ECC_CURVE_P384
                    uEccBlob.dwMagic = BCRYPT_ECDSA_PRIVATE_P384_MAGIC
                Case szOID_ECC_CURVE_P521
                    uEccBlob.dwMagic = BCRYPT_ECDSA_PRIVATE_P521_MAGIC
                Case Else
                    ErrRaise vbObjectError, , Replace(ERR_UNKNOWN_ECC_PRIVKEY, "%1", uPrivKeyInfo.AlgoObjId)
                End Select
                lBlobSize = uPublicKeyInfo.PublicKey.cbData - 1
                uEccBlob.cbKey = UBound(uPrivKeyInfo.KeyBlob) + 1
                Call CopyMemory(uEccBlob.Buffer(0), ByVal UnsignedAdd(uPublicKeyInfo.PublicKey.pbData, 1), lBlobSize)
                Call CopyMemory(uEccBlob.Buffer(lBlobSize), uPrivKeyInfo.KeyBlob(0), uEccBlob.cbKey)
                lBlobSize = 8 + lBlobSize + uEccBlob.cbKey
                '--- import key
                uProvInfo.pwszContainerName = StrPtr(sKeyName)
                uProvInfo.pwszProvName = StrPtr(MS_KEY_STORAGE_PROVIDER)
                hResult = NCryptOpenStorageProvider(hNProv, uProvInfo.pwszProvName, 0)
                If hResult < 0 Then
                    sApiSource = "NCryptOpenStorageProvider"
                    GoTo QH
                End If
                pvInitSecDesc uDesc, 1, uBuffers
                pvInitSecBuffer uBuffers(0), NCRYPTBUFFER_PKCS_KEY_NAME, StrPtr(sKeyName), LenB(sKeyName) + 2
                hResult = NCryptImportKey(hNProv, 0, StrPtr("ECCPRIVATEBLOB"), uDesc, hNKey, uEccBlob, lBlobSize, NCRYPT_OVERWRITE_KEY_FLAG)
                If hResult < 0 Then
                    sApiSource = "NCryptImportKey"
                    GoTo QH
                End If
            Case Else
                ErrRaise vbObjectError, , Replace(ERR_UNKNOWN_PUBKEY, "%1", pvToStringA(uPublicKeyInfo.Algorithm.pszObjId))
            End Select
        End If
        If CertSetCertificateContextProperty(pCertContext, CERT_KEY_PROV_INFO_PROP_ID, 0, uProvInfo) = 0 Then
            hResult = Err.LastDllError
            sApiSource = "CertSetCertificateContextProperty"
            GoTo QH
        End If
        pOutCertContext = pCertContext
        pCertContext = 0
    End If
    '--- success
    pvTlsImportToCertStore = True
QH:
    If hNKey <> 0 Then
        Call NCryptFreeObject(hNKey)
    End If
    If hNProv <> 0 Then
        Call NCryptFreeObject(hNProv)
    End If
    If hKey <> 0 Then
        Call CryptDestroyKey(hKey)
    End If
    If hProv <> 0 Then
        Call CryptReleaseContext(hProv, 0)
    End If
    If pCertContext <> 0 Then
        Call CertFreeCertificateContext(pCertContext)
    End If
    If hCertStore <> 0 Then
        Call CertCloseStore(hCertStore, 0)
    End If
    If LenB(sApiSource) <> 0 Then
        ErrRaise IIf(hResult < 0, hResult, hResult Or LNG_FACILITY_WIN32), FUNC_NAME & "." & sApiSource
    End If
End Function

Private Function pvTlsExportFromCertStore(ByVal hCertStore As Long, cCerts As Collection, cStatuses As Collection) As Boolean
    Const FUNC_NAME     As String = "pvTlsExportFromCertStore"
    Dim uCertContext    As CERT_CONTEXT
    Dim baCert()        As Byte
    Dim pCertContext    As Long
    Dim lSize           As Long
    Dim hResult         As Long
    Dim sApiSource      As String

    '--- export server X.509 certificates from certificate store
    Set cCerts = New Collection
    Set cStatuses = New Collection
    Do
        pCertContext = CertEnumCertificatesInStore(hCertStore, pCertContext)
        If pCertContext = 0 Then
            Exit Do
        End If
        Call CopyMemory(uCertContext, ByVal pCertContext, Len(uCertContext))
        pvWriteBuffer baCert, 0, uCertContext.pbCertEncoded, uCertContext.cbCertEncoded
        pvArrayReallocate baCert, uCertContext.cbCertEncoded, FUNC_NAME & ".baCert"
        cCerts.Add baCert
        '--- collect OCSP response
        If CertGetCertificateContextProperty(pCertContext, CERT_OCSP_RESPONSE_PROP_ID, ByVal 0, lSize) <> 0 And lSize > 0 Then
            pvArrayReallocate baCert, lSize, FUNC_NAME & ".baCert"
            If CertGetCertificateContextProperty(pCertContext, CERT_OCSP_RESPONSE_PROP_ID, baCert(0), lSize) = 0 Then
                hResult = Err.LastDllError
                sApiSource = "CertGetCertificateContextProperty"
                GoTo QH
            End If
            cStatuses.Add baCert
        End If
    Loop
    '--- success
    pvTlsExportFromCertStore = True
QH:
    If LenB(sApiSource) <> 0 Then
        ErrRaise IIf(hResult < 0, hResult, hResult Or LNG_FACILITY_WIN32), FUNC_NAME & "." & sApiSource
    End If
End Function

Private Function pvAsn1DecodePrivateKey(baPrivKey() As Byte, uRetVal As UcsKeyInfo) As Boolean
    Const FUNC_NAME     As String = "pvAsn1DecodePrivateKey"
    Dim lPkiPtr         As Long
    Dim uPrivKey        As CRYPT_PRIVATE_KEY_INFO
    Dim lKeyPtr         As Long
    Dim lKeySize        As Long
    Dim lSize           As Long
    Dim uEccKeyInfo     As CRYPT_ECC_PRIVATE_KEY_INFO
    Dim hResult         As Long
    Dim sApiSource      As String
    
    If CryptDecodeObjectEx(X509_ASN_ENCODING Or PKCS_7_ASN_ENCODING, PKCS_PRIVATE_KEY_INFO, baPrivKey(0), UBound(baPrivKey) + 1, CRYPT_DECODE_ALLOC_FLAG Or CRYPT_DECODE_NOCOPY_FLAG, 0, lPkiPtr, 0) <> 0 Then
        Debug.Assert lPkiPtr <> 0
        Call CopyMemory(uPrivKey, ByVal lPkiPtr, Len(uPrivKey))
        If CryptDecodeObjectEx(X509_ASN_ENCODING Or PKCS_7_ASN_ENCODING, PKCS_RSA_PRIVATE_KEY, ByVal uPrivKey.PrivateKey.pbData, uPrivKey.PrivateKey.cbData, CRYPT_DECODE_ALLOC_FLAG Or CRYPT_DECODE_NOCOPY_FLAG, 0, lKeyPtr, lKeySize) = 0 Then
            hResult = Err.LastDllError
            sApiSource = "CryptDecodeObjectEx(PKCS_RSA_PRIVATE_KEY)"
            GoTo QH
        End If
        uRetVal.AlgoObjId = pvToStringA(uPrivKey.Algorithm.pszObjId)
        GoTo DecodeRsa
    ElseIf CryptDecodeObjectEx(X509_ASN_ENCODING Or PKCS_7_ASN_ENCODING, PKCS_RSA_PRIVATE_KEY, baPrivKey(0), UBound(baPrivKey) + 1, CRYPT_DECODE_ALLOC_FLAG Or CRYPT_DECODE_NOCOPY_FLAG, 0, lKeyPtr, lKeySize) <> 0 Then
        uRetVal.AlgoObjId = szOID_RSA_RSA
DecodeRsa:
        pvArrayAllocate uRetVal.KeyBlob, lKeySize, FUNC_NAME & ".uRetVal.KeyBlob"
        Debug.Assert lKeyPtr <> 0
        Call CopyMemory(uRetVal.KeyBlob(0), ByVal lKeyPtr, lKeySize)
        Debug.Assert UBound(uRetVal.KeyBlob) + 1 >= 16
        Call CopyMemory(uRetVal.BitLen, uRetVal.KeyBlob(12), 4)
    ElseIf CryptDecodeObjectEx(X509_ASN_ENCODING Or PKCS_7_ASN_ENCODING, X509_ECC_PRIVATE_KEY, baPrivKey(0), UBound(baPrivKey) + 1, CRYPT_DECODE_ALLOC_FLAG Or CRYPT_DECODE_NOCOPY_FLAG, 0, lKeyPtr, 0) <> 0 Then
        Debug.Assert lKeyPtr <> 0
        Call CopyMemory(uEccKeyInfo, ByVal lKeyPtr, Len(uEccKeyInfo))
        uRetVal.AlgoObjId = pvToStringA(uEccKeyInfo.szCurveOid)
        pvArrayAllocate uRetVal.KeyBlob, uEccKeyInfo.PrivateKey.cbData, FUNC_NAME & ".uRetVal.KeyBlob"
        Debug.Assert uEccKeyInfo.PrivateKey.pbData <> 0
        Call CopyMemory(uRetVal.KeyBlob(0), ByVal uEccKeyInfo.PrivateKey.pbData, uEccKeyInfo.PrivateKey.cbData)
    ElseIf Err.LastDllError = ERROR_FILE_NOT_FOUND Then
        '--- no X509_ECC_PRIVATE_KEY struct type on NT4 -> decode in a wildly speculative way
        Call CopyMemory(lSize, baPrivKey(6), 1)
        If 7 + lSize <= UBound(baPrivKey) Then
            uRetVal.AlgoObjId = szOID_ECC_CURVE_P256
            pvArrayAllocate uRetVal.KeyBlob, lSize, FUNC_NAME & ".uRetVal.KeyBlob"
            Call CopyMemory(uRetVal.KeyBlob(0), baPrivKey(7), lSize)
        Else
            hResult = ERROR_FILE_NOT_FOUND
            sApiSource = "CryptDecodeObjectEx(X509_ECC_PRIVATE_KEY)"
            GoTo QH
        End If
    Else
        hResult = Err.LastDllError
        sApiSource = "CryptDecodeObjectEx(X509_ECC_PRIVATE_KEY)"
        GoTo QH
    End If
    '--- success
    pvAsn1DecodePrivateKey = True
QH:
    If lKeyPtr <> 0 Then
        Call LocalFree(lKeyPtr)
    End If
    If lPkiPtr <> 0 Then
        Call LocalFree(lPkiPtr)
    End If
    If LenB(sApiSource) <> 0 Then
        ErrRaise IIf(hResult < 0, hResult, hResult Or LNG_FACILITY_WIN32), FUNC_NAME & "." & sApiSource
    End If
End Function

Private Sub pvArrayAllocate(baRetVal() As Byte, ByVal lSize As Long, sFuncName As String)
    If lSize > 0 Then
        ReDim baRetVal(0 To lSize - 1) As Byte
    Else
        baRetVal = vbNullString
    End If
    Debug.Assert RedimStats(MODULE_NAME & "." & sFuncName, lSize)
End Sub

Private Sub pvArrayReallocate(baArray() As Byte, ByVal lSize As Long, sFuncName As String)
    If lSize > 0 Then
        ReDim Preserve baArray(0 To lSize - 1) As Byte
    Else
        baArray = vbNullString
    End If
    Debug.Assert RedimStats(MODULE_NAME & "." & sFuncName, lSize)
End Sub

Private Property Get pvArraySize(baArray() As Byte) As Long
    Dim lPtr            As Long

    '--- peek long at ArrPtr(baArray)
    Call CopyMemory(lPtr, ByVal ArrPtr(baArray), 4)
    If lPtr <> 0 Then
        pvArraySize = UBound(baArray) + 1
    End If
End Property

Private Function pvWriteReserved(baBuffer() As Byte, ByVal lPos As Long, ByVal lSize As Long) As Long
    pvWriteReserved = pvWriteBuffer(baBuffer, lPos, 0, lSize)
End Function

Private Function pvWriteBuffer(baBuffer() As Byte, ByVal lPos As Long, ByVal lPtr As Long, ByVal lSize As Long) As Long
    Const FUNC_NAME     As String = "pvWriteBuffer"
    Dim lBufPtr         As Long
    
    '--- peek long at ArrPtr(baBuffer)
    Call CopyMemory(lBufPtr, ByVal ArrPtr(baBuffer), 4)
    If lBufPtr = 0 Then
        pvArrayAllocate baBuffer, lPos + lSize, FUNC_NAME & ".baBuffer"
    ElseIf UBound(baBuffer) < lPos + lSize - 1 Then
        pvArrayReallocate baBuffer, lPos + lSize, FUNC_NAME & ".baRetVal"
    End If
    If lSize > 0 And lPtr <> 0 Then
        Debug.Assert IsBadReadPtr(lPtr, lSize) = 0
        Call CopyMemory(baBuffer(lPos), ByVal lPtr, lSize)
    End If
    pvWriteBuffer = lPos + lSize
End Function

Private Function pvReadLong(baBuffer() As Byte, ByVal lPos As Long, lValue As Long, Optional ByVal Size As Long = 1) As Long
    Static baTemp(0 To 3) As Byte
    
    If lPos + Size <= pvArraySize(baBuffer) Then
        If Size <= 1 Then
            lValue = baBuffer(lPos)
        Else
            baTemp(Size - 1) = baBuffer(lPos + 0)
            baTemp(Size - 2) = baBuffer(lPos + 1)
            If Size >= 3 Then baTemp(Size - 3) = baBuffer(lPos + 2)
            If Size >= 4 Then baTemp(Size - 4) = baBuffer(lPos + 3)
            Call CopyMemory(lValue, baTemp(0), Size)
        End If
    Else
        lValue = 0
    End If
    pvReadLong = lPos + Size
End Function

Private Function pvReadArray(baBuffer() As Byte, ByVal lPos As Long, baDest() As Byte, ByVal lSize As Long) As Long
    Const FUNC_NAME     As String = "pvReadArray"
    
    If lSize < 0 Then
        lSize = pvArraySize(baBuffer) - lPos
    End If
    If lSize > 0 Then
        pvArrayAllocate baDest, lSize, FUNC_NAME & ".baDest"
        If lPos + lSize <= pvArraySize(baBuffer) Then
            Call CopyMemory(baDest(0), baBuffer(lPos), lSize)
        ElseIf lPos < pvArraySize(baBuffer) Then
            Call CopyMemory(baDest(0), baBuffer(lPos), pvArraySize(baBuffer) - lPos)
        End If
    Else
        Erase baDest
    End If
    pvReadArray = lPos + lSize
End Function

'= Schannel buffers helpers ==============================================

Private Sub pvInitSecDesc(uDesc As ApiSecBufferDesc, ByVal lCount As Long, uBuffers() As ApiSecBuffer)
    ReDim uBuffers(0 To lCount - 1)
    With uDesc
        .ulVersion = SECBUFFER_VERSION
        .cBuffers = lCount
        .pBuffers = VarPtr(uBuffers(0))
    End With
End Sub

Private Sub pvInitSecBuffer(uBuffer As ApiSecBuffer, ByVal lType As Long, Optional ByVal lPtr As Long, Optional ByVal lSize As Long)
    With uBuffer
        .BufferType = lType
        .pvBuffer = lPtr
        .cbBuffer = lSize
    End With
End Sub

Private Function pvToStringA(ByVal lPtr As Long) As String
    If lPtr <> 0 Then
        pvToStringA = String$(lstrlenA(lPtr), 0)
        Call CopyMemory(ByVal pvToStringA, ByVal lPtr, Len(pvToStringA))
    End If
End Function

#If ImplUseDebugLog Then
Private Function pvToStringW(ByVal lPtr As Long) As String
    If lPtr <> 0 Then
        pvToStringW = String$(lstrlenW(lPtr), 0)
        Call CopyMemory(ByVal StrPtr(pvToStringW), ByVal lPtr, LenB(pvToStringW))
    End If
End Function
#End If

Private Function pvCollectionCount(oCol As Collection) As Long
    If Not oCol Is Nothing Then
        pvCollectionCount = oCol.Count
    End If
End Function

Private Function pvGetRandomString(Optional Size As Long = 16, Optional Delimiter As String) As String
    Dim baBuffer()          As Byte
    Dim aText()             As String
    Dim lIdx                As Long
    
    ReDim baBuffer(0 To Size - 1) As Byte
    Call RtlGenRandom(baBuffer(0), Size)
    ReDim aText(0 To UBound(baBuffer)) As String
    For lIdx = 0 To UBound(baBuffer)
        aText(lIdx) = Right$("0" & Hex$(baBuffer(lIdx)), 2)
    Next
    pvGetRandomString = LCase$(Join(aText, Delimiter))
End Function

#If Not ImplUseShared Then
Public Function RedimStats(sFuncName As String, ByVal lSize As Long) As Boolean
    #If sFuncName And lSize Then
    #End If
    RedimStats = True
End Function

Public Sub RemoveCollection(ByVal oCol As Collection, Index As Variant)
    If Not oCol Is Nothing Then
        pvCallCollectionRemove oCol, Index
    End If
End Sub

Public Function SearchCollection(ByVal oCol As Collection, Index As Variant, Optional RetVal As Variant) As Boolean
    Dim vItem           As Variant
    
    If oCol Is Nothing Then
        GoTo QH
    ElseIf pvCallCollectionItem(oCol, Index, vItem) < 0 Then
        GoTo QH
    End If
    If IsObject(vItem) Then
        Set RetVal = vItem
    Else
        RetVal = vItem
    End If
    '--- success
    SearchCollection = True
QH:
End Function

Private Function pvCallCollectionItem(ByVal oCol As Collection, Index As Variant, Optional RetVal As Variant) As Long
    Const IDX_COLLECTION_ITEM As Long = 7
    
    pvPatchMethodTrampoline AddressOf mdTlsNative.pvCallCollectionItem, IDX_COLLECTION_ITEM
    pvCallCollectionItem = pvCallCollectionItem(oCol, Index, RetVal)
End Function

Private Function pvCallCollectionRemove(ByVal oCol As Collection, Index As Variant) As Long
    Const IDX_COLLECTION_REMOVE As Long = 10
    
    pvPatchMethodTrampoline AddressOf mdTlsNative.pvCallCollectionRemove, IDX_COLLECTION_REMOVE
    pvCallCollectionRemove = pvCallCollectionRemove(oCol, Index)
End Function

Private Function pvPatchMethodTrampoline(ByVal Pfn As Long, ByVal lMethodIdx As Long) As Boolean
    Dim bInIDE          As Boolean

    Debug.Assert pvSetTrue(bInIDE)
    If bInIDE Then
        '--- note: IDE is not large-address aware
        Call CopyMemory(Pfn, ByVal Pfn + &H16, 4)
    Else
        Call VirtualProtect(Pfn, 12, PAGE_EXECUTE_READWRITE, 0)
    End If
    ' 0: 8B 44 24 04          mov         eax,dword ptr [esp+4]
    ' 4: 8B 00                mov         eax,dword ptr [eax]
    ' 6: FF A0 00 00 00 00    jmp         dword ptr [eax+lMethodIdx*4]
    Call CopyMemory(ByVal Pfn, -684575231150992.4725@, 8)
    Call CopyMemory(ByVal (Pfn Xor &H80000000) + 8 Xor &H80000000, lMethodIdx * 4, 4)
    '--- success
    pvPatchMethodTrampoline = True
End Function

Private Function pvSetTrue(bValue As Boolean) As Boolean
    #If TWINBASIC = 0 Then
        bValue = True
    #End If
    pvSetTrue = True
End Function

Public Function FromBase64Array(sText As String) As Byte()
    Const CRYPT_STRING_BASE64 As Long = 1
    Dim lSize           As Long
    Dim baOutput()      As Byte
    
    On Error GoTo EH
    lSize = Len(sText) + 1
    ReDim baOutput(0 To lSize - 1) As Byte
    If CryptStringToBinary(StrPtr(sText), Len(sText), CRYPT_STRING_BASE64, VarPtr(baOutput(0)), lSize) <> 0 Then
        If lSize > 0 Then
            ReDim Preserve baOutput(0 To lSize - 1) As Byte
            FromBase64Array = baOutput
        Else
            FromBase64Array = vbNullString
        End If
        Exit Function
    End If
EH:
    With CreateObject("MSXML2.DOMDocument").createElement("dummy")
        .DataType = "bin.base64"
        .Text = sText
        If IsArray(.NodeTypedValue) Then
            FromBase64Array = .NodeTypedValue
        Else
            FromBase64Array = vbNullString
        End If
    End With
End Function

Private Function UnsignedAdd(ByVal lUnsignedPtr As Long, ByVal lSignedOffset As Long) As Long
    '--- note: safely add *signed* offset to *unsigned* ptr for *unsigned* retval w/o overflow in LARGEADDRESSAWARE processes
    UnsignedAdd = ((lUnsignedPtr Xor &H80000000) + lSignedOffset) Xor &H80000000
End Function

Private Function SplitOrReindex(Expression As String, Delimiter As String) As Variant
    Dim vResult         As Variant
    Dim vTemp           As Variant
    Dim lIdx            As Long
    Dim lSize           As Long
    
    vResult = Split(Expression, Delimiter)
    '--- check if reindex needed
    If IsNumeric(vResult(0)) Then
        vTemp = vResult
        For lIdx = 0 To UBound(vTemp) Step 2
            If lSize < vTemp(lIdx) Then
                lSize = vTemp(lIdx)
            End If
        Next
        ReDim vResult(0 To lSize) As Variant
        Debug.Assert RedimStats(MODULE_NAME & ".SplitOrReindex.vResult", 0)
        For lIdx = 0 To UBound(vTemp) Step 2
            vResult(vTemp(lIdx)) = vTemp(lIdx + 1)
        Next
        SplitOrReindex = vResult
    End If
End Function

Private Property Get RealOsVersion(Optional BuildNo As Long) As UcsOsVersionEnum
    Static lVersion     As Long
    Static lBuildNo     As Long
    Dim baBuffer()      As Byte
    Dim lPtr            As Long
    Dim lSize           As Long
    Dim aVer(0 To 9)    As Integer
    
    If lVersion = 0 Then
        ReDim baBuffer(0 To 8192) As Byte
        Call GetFileVersionInfo(StrPtr("kernel32.dll"), 0, UBound(baBuffer), baBuffer(0))
        Call VerQueryValue(baBuffer(0), StrPtr("\"), lPtr, lSize)
        Call CopyMemory(aVer(0), ByVal lPtr, 20)
        lVersion = aVer(9) * 100 + aVer(8)
        lBuildNo = aVer(7)
    End If
    RealOsVersion = lVersion
    BuildNo = lBuildNo
End Property

Private Function GetSystemMessage(ByVal lLastDllError As Long) As String
    Const FORMAT_MESSAGE_FROM_SYSTEM    As Long = &H1000
    Const FORMAT_MESSAGE_IGNORE_INSERTS As Long = &H200
    Dim lSize               As Long
   
    GetSystemMessage = String$(2000, 0)
    lSize = FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM Or FORMAT_MESSAGE_IGNORE_INSERTS, 0, lLastDllError, 0, StrPtr(GetSystemMessage), Len(GetSystemMessage), 0)
    GetSystemMessage = Left$(GetSystemMessage, lSize)
End Function
#End If ' Not ImplUseShared

Public Function TlsDesignDumpArray(baData() As Byte, Optional ByVal Pos As Long, Optional ByVal Size As Long = -1) As String
    If Size < 0 Then
        Size = UBound(baData) + 1 - Pos
    End If
    If Size > 0 Then
        TlsDesignDumpArray = TlsDesignDumpMemory(VarPtr(baData(Pos)), Size)
    End If
End Function

Public Function TlsDesignDumpMemory(ByVal lPtr As Long, ByVal lSize As Long) As String
    Dim lIdx            As Long
    Dim sHex            As String
    Dim sChar           As String
    Dim lValue          As Long
    Dim aResult()       As String
    
    ReDim aResult(0 To (lSize + 15) \ 16) As String
    Debug.Assert RedimStats("TlsDesignDumpMemory.aResult", UBound(aResult) + 1)
    For lIdx = 0 To ((lSize + 15) \ 16) * 16
        If lIdx < lSize Then
            If IsBadReadPtr(lPtr, 1) = 0 Then
                Call CopyMemory(lValue, ByVal lPtr, 1)
                sHex = sHex & Right$("0" & Hex$(lValue), 2) & " "
                If lValue >= 32 Then
                    sChar = sChar & Chr$(lValue)
                Else
                    sChar = sChar & "."
                End If
            Else
                sHex = sHex & "?? "
                sChar = sChar & "."
            End If
        Else
            sHex = sHex & "   "
        End If
        If ((lIdx + 1) Mod 4) = 0 Then
            sHex = sHex & " "
        End If
        If ((lIdx + 1) Mod 16) = 0 Then
            aResult(lIdx \ 16) = Right$("000" & Hex$(lIdx - 15), 4) & " - " & sHex & sChar
            sHex = vbNullString
            sChar = vbNullString
        End If
        lPtr = (lPtr Xor &H80000000) + 1 Xor &H80000000
    Next
    TlsDesignDumpMemory = Join(aResult, vbCrLf)
End Function

#If ImplCaptureTraffic <> 0 Then
Public Function TlsConcatCollection(oCol As Collection, Optional Separator As String = vbCrLf) As String
    Dim lSize           As Long
    Dim vElem           As Variant
    
    For Each vElem In oCol
        lSize = lSize + Len(vElem) + Len(Separator)
    Next
    If lSize > 0 Then
        TlsConcatCollection = String$(lSize - Len(Separator), 0)
        lSize = 1
        For Each vElem In oCol
            If lSize <= Len(TlsConcatCollection) Then
                Mid$(TlsConcatCollection, lSize, Len(vElem) + Len(Separator)) = vElem & Separator
            End If
            lSize = lSize + Len(vElem) + Len(Separator)
        Next
    End If
End Function
#End If ' ImplCaptureTraffic

