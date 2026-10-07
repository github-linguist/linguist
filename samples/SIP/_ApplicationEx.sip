/**
 * Copyright (c) 2020 Weitian Leung
 *
 * This file is part of pywpsrpc.
 *
 * This file is distributed under the MIT License.
 * See the LICENSE file for details.
 *
*/

namespace wpsapiex
{
    struct _ApplicationEx : public IDispatch /Abstract/
    {
    public:
        virtual HRESULT get_WpsCloudService(
            WpsCloudService **prop /Out/) = 0;

        virtual HRESULT get_SaveRememberStatus(
            VARIANT_BOOL *prop /Out/) = 0;

        virtual HRESULT put_SaveRememberStatus(
            VARIANT_BOOL prop) = 0;

        virtual HRESULT CreateCustomTaskPane(
            long wid,
            BSTR title,
            IDispatch **prop /Out/) = 0;
        virtual HRESULT put_ForceBackupEnable(
            VARIANT_BOOL prop) = 0;

        virtual HRESULT get_ForceBackupEnable(
            VARIANT_BOOL *prop /Out/) = 0;

        virtual HRESULT get_EmbedWid(
            unsigned long *wid /Out/) = 0;

        WpsCloudService* getWpsCloudService();
        %MethodCode
            wpsapiex::WpsCloudService *prop = nullptr;
            HRESULT hr = sipCpp->get_WpsCloudService(&prop);
            if (hr != S_OK)
            {
                PyErr_Format(PyExc_AttributeError,
                    "Call 'get_WpsCloudService()' failed with 0x%x", hr);
                return nullptr;
            }
            else
            {
                sipRes = prop;
            }
        %End

        %Property(name=WpsCloudService, get=getWpsCloudService)

        VARIANT_BOOL getSaveRememberStatus();
        %MethodCode
            VARIANT_BOOL *prop = new VARIANT_BOOL;
            HRESULT hr = sipCpp->get_SaveRememberStatus(prop);
            if (hr != S_OK)
            {
                delete prop;
                PyErr_Format(PyExc_AttributeError,
                    "Call 'get_SaveRememberStatus()' failed with 0x%x", hr);
                return nullptr;
            }
            else
            {
                sipRes = prop;
            }
        %End

        PyObject* setSaveRememberStatus(VARIANT_BOOL prop);
        %MethodCode
            HRESULT hr = sipCpp->put_SaveRememberStatus(*a0);
            if (hr != S_OK)
            {
                PyErr_Format(PyExc_AttributeError,
                    "Call 'put_SaveRememberStatus()' failed with 0x%x", hr);
                sipRes = nullptr;
            }
            else
            {
                Py_INCREF(Py_None);
                sipRes = Py_None;
            }
        %End

        %Property(name=SaveRememberStatus, get=getSaveRememberStatus, set=setSaveRememberStatus)

        VARIANT_BOOL getForceBackupEnable();
        %MethodCode
            VARIANT_BOOL *prop = new VARIANT_BOOL;
            HRESULT hr = sipCpp->get_ForceBackupEnable(prop);
            if (hr != S_OK)
            {
                delete prop;
                PyErr_Format(PyExc_AttributeError,
                    "Call 'get_ForceBackupEnable()' failed with 0x%x", hr);
                return nullptr;
            }
            else
            {
                sipRes = prop;
            }
        %End

        PyObject* setForceBackupEnable(VARIANT_BOOL prop);
        %MethodCode
            HRESULT hr = sipCpp->put_ForceBackupEnable(*a0);
            if (hr != S_OK)
            {
                PyErr_Format(PyExc_AttributeError,
                    "Call 'put_ForceBackupEnable()' failed with 0x%x", hr);
                sipRes = nullptr;
            }
            else
            {
                Py_INCREF(Py_None);
                sipRes = Py_None;
            }
        %End

        %Property(name=ForceBackupEnable, get=getForceBackupEnable, set=setForceBackupEnable)

        unsigned long getEmbedWid();
        %MethodCode
            unsigned long prop = 0;
            HRESULT hr = sipCpp->get_EmbedWid(&prop);
            if (hr != S_OK)
            {
                PyErr_Format(PyExc_AttributeError,
                    "Call 'get_EmbedWid()' failed with 0x%x", hr);
                return nullptr;
            }
            else
            {
                sipRes = prop;
            }
        %End

        %Property(name=EmbedWid, get=getEmbedWid)
    };
};
