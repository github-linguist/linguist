/**
 * Copyright (c) 2020-2023 Weitian Leung
 *
 * This file is part of pywpsrpc.
 *
 * This file is distributed under the MIT License.
 * See the LICENSE file for details.
 *
*/

%Module(name=pywpsrpc.common, keyword_arguments="Optional")

%ModuleHeaderCode
    #include "stdafx.h"
    #include <QCoreApplication>
    #include <atomic>

BSTR _SysAllocString(const OLECHAR* psz);
UINT _SysStringLen(BSTR bstr);
void _SysFreeString(BSTR bstr);

UINT32 _SafeArrayGetElemsize(SAFEARRAY *psa);
HRESULT _SafeArrayGetElement(SAFEARRAY *psa, INT32 *rgIndices, void *pv);
_SAFEARRAY_PTR _SafeArrayCreate(VARTYPE vt, UINT32 cDims, SAFEARRAYBOUND* rgsabound);
HRESULT _SafeArrayPutElement(SAFEARRAY *psa, INT32 *rgIndices, void *pv);
UINT32 _SafeArrayGetDim(SAFEARRAY *psa);
HRESULT _SafeArrayGetUBound(SAFEARRAY *psa, UINT32 nDim, INT32 *plUbound);
HRESULT _SafeArrayGetLBound(SAFEARRAY *psa, UINT32 nDim, INT32 *plLbound);

bool _getVarDate(double date,
                 int &year, int &month, int &day,
                 int &hour, int &minute, int &second,
                 int &us);

double _toVarDate(int year, int month, int day,
                  int hour, int minute, int second,
                  int us);

class QtApp
{
public:
    QtApp(PyObject *args);
    ~QtApp();

private:
    static int argc;
    static char **argv;
    static QCoreApplication *qtApp;
    static std::atomic<int> ref;
};
%End

%Include typedef.sip
%Include objbase.sip
%Include objidl.sip
%Include oaidl.sip
%Include guid.sip
%Include ksoapi.sip
%Include wpsapiex.sip

HRESULT S_OK;
HRESULT S_FALSE;

HRESULT E_INVALIDARG;
HRESULT E_NOINTERFACE;
HRESULT E_ABORT;
HRESULT E_FAIL;
HRESULT E_ACCESSDENIED;
HRESULT E_NOTIMPL;

bool SUCCEEDED(HRESULT hr);
bool FAILED(HRESULT hr);

class QtApp /NoDefaultCtors/
{
public:
    QtApp(PyObject *args /TypeHint="List[str]"/);
    ~QtApp();
};

%ModuleCode

BSTR _SysAllocString(const OLECHAR* psz)
{
    typedef BSTR (*pfnSysAllocString)(const OLECHAR*);
    static pfnSysAllocString fnAllocStr = (pfnSysAllocString)sipImportSymbol("SysAllocString");

    return fnAllocStr(psz);
}

UINT _SysStringLen(BSTR bstr)
{
    typedef UINT (*pfnSysStringLen)(BSTR);
    static pfnSysStringLen fnStrLen = (pfnSysStringLen)sipImportSymbol("SysStringLen");

    return fnStrLen(bstr);
}

void _SysFreeString(BSTR bstr)
{
    typedef void (*pfnSysFreeString)(BSTR);
    static pfnSysFreeString fnFreeStr = (pfnSysFreeString)sipImportSymbol("SysFreeString");

    fnFreeStr(bstr);
}

UINT32 _SafeArrayGetElemsize(SAFEARRAY *psa)
{
    typedef UINT32 (*pfnSafeArrayGetElemsize)(SAFEARRAY*);
    static pfnSafeArrayGetElemsize fnGetSize = (pfnSafeArrayGetElemsize)sipImportSymbol("SafeArrayGetElemsize");

    return fnGetSize(psa);
}

HRESULT _SafeArrayGetElement(SAFEARRAY *psa, INT32 *rgIndices, void *pv)
{
    typedef HRESULT (*pfnSafeArrayGetElement)(SAFEARRAY *, INT32 *, void *);
    static pfnSafeArrayGetElement fnGetEle = (pfnSafeArrayGetElement)sipImportSymbol("SafeArrayGetElement");

    return fnGetEle(psa, rgIndices, pv);
}

_SAFEARRAY_PTR _SafeArrayCreate(VARTYPE vt, UINT32 cDims, SAFEARRAYBOUND* rgsabound)
{
    typedef _SAFEARRAY_PTR (*pfnSafeArrayCreate)(VARTYPE vt, UINT32 cDims, SAFEARRAYBOUND* rgsabound);
    static pfnSafeArrayCreate fnCreate = (pfnSafeArrayCreate)sipImportSymbol("SafeArrayCreate");

    return fnCreate(vt, cDims, rgsabound);
}

HRESULT _SafeArrayPutElement(SAFEARRAY *psa, INT32 *rgIndices, void *pv)
{
    typedef HRESULT (*pfnSafeArrayPutElement)(SAFEARRAY *, INT32 *, void *);
    static pfnSafeArrayPutElement fnPutEle = (pfnSafeArrayPutElement)sipImportSymbol("SafeArrayPutElement");

    return fnPutEle(psa, rgIndices, pv);
}

UINT32 _SafeArrayGetDim(SAFEARRAY *psa)
{
    typedef UINT32 (*pfnSafeArrayGetDim)(SAFEARRAY *);
    static pfnSafeArrayGetDim fnGetDim = (pfnSafeArrayGetDim)sipImportSymbol("SafeArrayGetDim");

    return fnGetDim(psa);
}

HRESULT _SafeArrayGetUBound(SAFEARRAY *psa, UINT32 nDim, INT32 *plUbound)
{
    typedef UINT32 (*pfnSafeArrayGetUBound)(SAFEARRAY *, UINT32, INT32*);
    static pfnSafeArrayGetUBound fnGetUBound = (pfnSafeArrayGetUBound)sipImportSymbol("SafeArrayGetUBound");

    return fnGetUBound(psa, nDim, plUbound);
}

HRESULT _SafeArrayGetLBound(SAFEARRAY *psa, UINT32 nDim, INT32 *plLbound)
{
    typedef UINT32 (*pfnSafeArrayGetLBound)(SAFEARRAY *, UINT32, INT32*);
    static pfnSafeArrayGetLBound fnGetLBound = (pfnSafeArrayGetLBound)sipImportSymbol("SafeArrayGetLBound");

    return fnGetLBound(psa, nDim, plLbound);
}

bool _getVarDate(double date,
                 int &year, int &month, int &day,
                 int &hour, int &minute, int &second,
                 int &us)
{
    const int DATE_MIN = -657434;
    const int DATE_MAX = 2958465;

    if (date <= (DATE_MIN - 1.0) || date >= (DATE_MAX + 1.0))
        return false;

    double datePart = date < 0.0 ? ceil(date) : floor(date);
    double timePart = fabs(date - datePart) + 0.00000000001;
    if (timePart >= 1.0)
        timePart -= 0.00000000001;

    auto julianDays = [](int date) -> int
    {
        date -= DATE_MIN;
        date += 1757585;
        return date;
    };

    auto parseJulian = [](int jd, int &year, int &month, int &day)
    {
        int l = jd + 68569;
        int n = l * 4 / 146097;
        l -= (n * 146097 + 3) / 4;
        int i = (4000 * (l + 1)) / 1461001;
        l += 31 - (i * 1461) / 4;
        int j = (l * 80) / 2447;
        day = l - (j * 2447) / 80;
        l = j / 11;
        month = (j + 2) - (12 * l);
        year = 100 * (n - 49) + i + l;
    };

    parseJulian(julianDays(date), year, month, day);

    timePart *= 24.0;
    hour = timePart;
    timePart -= hour;
    timePart *= 60.0;
    minute = timePart;
    timePart -= minute;
    timePart *= 60.0;
    second = timePart;
    timePart -= second;
    us = timePart * 1000 * 1000;

    return true;
}

double _toVarDate(int year, int month, int day,
                  int hour, int minute, int second,
                  int us)
{
    auto toJulian = [](int year, int month, int day) -> double
    {
        int m12 = (month - 14) / 12;

        return ((1461 * (year + 4800 + m12)) / 4 +
                (367 * (month - 2 - 12 * m12)) / 12 -
                (3 * ((year + 4900 + m12) / 100)) / 4 + day - 32075);
    };

    auto dateFromJulian = [](int date) -> int
    {
        date -= 1757585;
        date += -657434;
        return date;
    };

    double date = dateFromJulian(toJulian(year, month, day));
    double sign = (date < 0.0) ? -1.0 : 1.0;

    date += hour / 24.0 * sign;
    date += minute / 1440.0 * sign;
    date += second / 86400.0 * sign;
    date += us / 1000000 * sign;

    return date;
}

#define _HRESULT_CONST(name, value) \
    static const HRESULT name = value;

#undef S_OK
#undef S_FALSE
#undef E_INVALIDARG
#undef E_NOINTERFACE
#undef E_ABORT
#undef E_FAIL
#undef E_ACCESSDENIED
#undef E_NOTIMPL

_HRESULT_CONST(S_OK, 0x00000000)
_HRESULT_CONST(S_FALSE, 0x00000001)
_HRESULT_CONST(E_INVALIDARG, 0x80070057)
_HRESULT_CONST(E_NOINTERFACE, 0x80004002)
_HRESULT_CONST(E_ABORT, 0x80004004)
_HRESULT_CONST(E_FAIL, 0x80004005)
_HRESULT_CONST(E_ACCESSDENIED, 0x80070005)
_HRESULT_CONST(E_NOTIMPL, 0x80004001)

#undef SUCCEEDED
static bool SUCCEEDED(HRESULT hr)
{
    return hr >= 0;
}

#undef FAILED
static bool FAILED(HRESULT hr)
{
    return hr < 0;
}

// to avoid depends on python's Qt (PySide2, PyQt5 etc.)
#include <QString>

int QtApp::argc = 0;
char **QtApp::argv = nullptr;
QCoreApplication *QtApp::qtApp = nullptr;
std::atomic<int> QtApp::ref;


QtApp::QtApp(PyObject *args)
{
    ++ref;
    if (qtApp)
        return;

    argc = PyList_Size(args);
    argv = new char *[argc + 1];

    for (int i = 0; i < argc; ++i)
    {
        PyObject *item = PyList_GetItem(args, i);
        char *arg = nullptr;

        if (PyUnicode_Check(item))
        {
            int char_size = 0;
            Py_ssize_t len = 0;
            void *data = sipUnicodeData(item, &char_size, &len);

            switch (char_size)
            {
            case 1:
                arg = strdup(reinterpret_cast<char*>(data));
                break;

            case 2:
                {
                    QString qStr(reinterpret_cast<QChar*>(data), len);
                    const QByteArray ba = qStr.toLocal8Bit();
                    arg = strdup(ba.constData());
                }
                break;

            case 4:
                {
                    QString qStr = QString::fromUcs4(reinterpret_cast<uint*>(data), len);
                    const QByteArray ba = qStr.toLocal8Bit();
                    arg = strdup(ba.constData());
                }
                break;
            }
        }
        else if (PyBytes_Check(item))
        {
            arg = strdup(PyBytes_AsString(item));
        }

        argv[i] = arg;
    }

    argv[argc] = nullptr;

    qtApp = new QCoreApplication(argc, argv);
}

QtApp::~QtApp()
{
    --ref;
    if (ref == 0 && qtApp)
    {
        delete qtApp;

        for (int i = 0; i < argc; ++i)
        {
            if (argv[i])
                free(argv[i]);
        }

        delete [] argv;

        qtApp = nullptr;
        argc = 0;
        argv = nullptr;
    }
}

static void IUnknown_CollectingWrapperEventHandler(sipSimpleWrapper *sipSelf)
{
    if (!sipIsOwnedByPython(sipSelf))
    {
        if (IUnknown *cppPtr = reinterpret_cast<IUnknown*>(sipGetAddress(sipSelf)))
            cppPtr->Release();
    }
}
%End

%PostInitialisationCode
sipRegisterEventHandler(sipEventCollectingWrapper,
    sipType_IUnknown,
    (void *)IUnknown_CollectingWrapperEventHandler);
%End
