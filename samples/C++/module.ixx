//**********************************************************************
// Copyright Patrick Sweeney 2015-2021
// Licensed under the MIT license.
// See file LICENSE for details.
//**********************************************************************
module;
#include <string>
#include <atlbase.h>
#include <d3d11.h>
#include <vector>
#include <any>
#include <map>

export module Rendering.Caustic.Shader;
import Base.Core.Core;
import Base.Core.Error;
import Base.Core.RefCount;
import Base.Core.IRefCount;
import Base.Math.BBox;
import Base.Math.Matrix;
import Rendering.Caustic.IShader;

// A shader is a object that manages the vertex and pixel shader
//
export namespace Caustic
{
    const int c_MaxFrames = 2; // Maximum number of frames being buffered

    //**********************************************************************
    // EShaderParamType defines the various types of parameters that can
    // be passed to a CShader
    //
    // Module:
    // {Link:import Rendering.Caustic.Shader;{Rendering/Caustic/Shader.ixx}}
    //**********************************************************************
    enum EShaderParamType
    {
        ShaderType_Undefined,
        ShaderType_Texture,
        ShaderType_Sampler,
        ShaderType_Float,
        ShaderType_Float2,
        ShaderType_Float3,
        ShaderType_Float4,
        ShaderType_Int,
        ShaderType_Matrix,
        ShaderType_Matrix3x3,
        ShaderType_Float_Array,
        ShaderType_Float2_Array,
        ShaderType_Float3_Array,
        ShaderType_Float4_Array,
        ShaderType_Int_Array,
        ShaderType_Matrix_Array,
        ShaderType_Matrix3x3_Array,
        ShaderType_StructuredBuffer,
        ShaderType_RWStructuredBuffer,
        ShaderType_AppendStructuredBuffer,
        ShaderType_RWByteAddressBuffer
    };

    //**********************************************************************
    // Struct: ShaderParamDef
    // ShaderParamDef defines the shader definitions that were parsed from HLSL
    // by ParseShader. These definitions define the variables that each
    // shader defines (i.e. may be set by the client)
    //
    // Parameters:
    // m_type - Defines type of this parameter 
    // m_name - Name of shader parameter
    // m_offset - Register offset
    // m_members - Number of elements (i.e. some parameters can be arrays) 
    // m_elemSize - sizeof a single element in bytes (used by buffers)
    //
    // Module:
    // {Link:import Rendering.Caustic.Shader;{Rendering/Caustic/Shader.ixx}}
    //**********************************************************************
    struct ShaderParamDef
    {
        EShaderParamType m_type; // Defines type of this parameter
        std::wstring m_name;     // Name of shader parameter
        uint32 m_offset;         // register offset
        uint32 m_members;        // Number of elements (i.e. some parameters can be arrays)
        uint32 m_elemSize;       // size of a single element (used by buffers)
    };

    //**********************************************************************
    // Struct: ShaderParamInstance
    // ShaderParamInstance defines each parameter that a shader exposes. These parameters
    // are derived from the ShaderParamDefs above. This is servers copy of each
    // parameter along with its value and position in the constant buffer.
    //
    // Parameters:
    // m_value - Value assigned to this parameter
    // m_values - Array of values assigned to this parameter
    // m_dirty - Is parameter dirty and needs to be pushed to constant buffer
    //
    // Module:
    // {Link:import Rendering.Caustic.Shader;{Rendering/Caustic/Shader.ixx}}
    //**********************************************************************
    struct ShaderParamInstance : public ShaderParamDef
    {
        std::any m_value;      // Value assigned to this parameter
        std::vector<std::any> m_values;
        bool m_dirty;            // Is parameter dirty and needs to be pushed to constant buffer
        uint32 m_cbOffset;      // Byte offset of this variable in the constant buffer
    };

    struct Float { float x; Float(float _x) { x = _x; } };
    struct Int { int x; Int(int _x) { x = _x; } };
    struct Float2 { float x; float y; Float2(float _x, float _y) { x = _x; y = _y; } };
    struct Float3 { float x; float y; float z; Float3(float _x, float _y, float _z) { x = _x; y = _y; z = _z; } };
    struct Float4 { float x; float y; float z; float w; Float4(float _x, float _y, float _z, float _w) { x = _x; y = _y; z = _z; w = _w; } };
    struct Matrix
    { 
        float x[16]; 
        Matrix() 
        { 
            ZeroMemory(x, sizeof(x));
        }
    
        Matrix(Matrix4x4& m)
        {
            int index = 0;
            for (int col = 0; col < 4; col++)
                for (int row = 0; row < 4; row++)
                    x[index++] = m[row][col];
        }

        Matrix(Matrix3x3& m)
        {
            int index = 0;
            x[index++] = m[0][0];
            x[index++] = m[1][0];
            x[index++] = 0.0f;
            x[index++] = m[2][0];
            x[index++] = m[0][1];
            x[index++] = m[1][1];
            x[index++] = 0.0f;
            x[index++] = m[2][1];
            x[index++] = m[0][2];
            x[index++] = m[1][2];
            x[index++] = 0.0f;
            x[index++] = m[2][2];
            x[index++] = 0.0f;
            x[index++] = 0.0f;
            x[index++] = 0.0f;
            x[index++] = 1.0f;
        }

        Matrix(float _x[16])
        {
            memcpy(x, _x, sizeof(float) * 16);
        }
    };
    
    struct Matrix_3x3
    {
        float x[16];

        Matrix_3x3()
        {
            ZeroMemory(x, sizeof(x));
        }
        
        Matrix_3x3(Matrix3x3& m)
        {
            int index = 0;
            for (int col = 0; col < 4; col++)
                for (int row = 0; row < 4; row++)
                    if (row == 3 || col == 3)
                        x[index++] = 0.0f;
                    else
                        x[index++] = m[row][col];
        }

        Matrix_3x3(float _x[16])
        {
            memcpy(x, _x, sizeof(float) * 16);
        }
    };

    //**********************************************************************
    // Class: SBuffer
    // Defines a buffer (which may be either a constant buffer or
    // and unordered access buffer used by a compute shader).
    //
    // Members:
    // m_spBuffer - the D3D buffer (that will be passed to the compute shader)
    // m_spStagingBuffer - staging buffer for going between CPU and GPU
    // m_spView - A view onto the buffer m_spBuffer
    // m_bufferSize - size of buffer in bytes
    // m_heapSize - size of the total heap the buffer is contained in
    //
    // Module:
    // {Link:import Rendering.Caustic.Shader;{Rendering/Caustic/Shader.ixx}}
    //**********************************************************************
    struct SBuffer
    {
        CComPtr<ID3D11Buffer> m_spBuffer;
        CComPtr<ID3D11Buffer> m_spStagingBuffer;
        CComPtr<ID3D11UnorderedAccessView> m_spUAView;
        CComPtr<ID3D11ShaderResourceView> m_spSRView;
        uint32 m_bufferSize;
        uint32 m_heapSize;
        int m_bufferSlot;
        std::wstring m_name;

        SBuffer() :
            m_bufferSize(0),
            m_heapSize(0),
            m_bufferSlot(0)
        {
        }
    };

    //**********************************************************************
    // Class: CGPUBuffer
    // Defines a buffer that is created by the client for passing/receiving
    // data from a compute shader.
    //
    // Module:
    // {Link:import Rendering.Caustic.Shader;{Rendering/Caustic/Shader.ixx}}
    //**********************************************************************
    class CGPUBuffer : public IGPUBuffer, public CRefCount
    {
        CComPtr<ID3D11Buffer> m_spBuffer;
        CComPtr<ID3D11Buffer> m_spStagingBuffer;
        CComPtr<ID3D11UnorderedAccessView> m_spUAView;
        CComPtr<ID3D11ShaderResourceView> m_spSRView;
        uint32 m_bufferSize;
        uint32 m_heapSize;
        int m_bufferSlot;
        std::wstring m_name;
        EBufferType m_bufferType;
        uint32 m_numElems; // Number of elemens in buffer
        uint32 m_elemSize; // Element size (unaligned on CPU)
        
        void CreateBuffer(ID3D11Device* pDevice, uint32 bufSize,
            uint32 bindFlags, uint32 cpuAccessFlags, D3D11_USAGE usage,
            uint32 miscFlags, uint32 stride, uint32 alignment, ID3D11Buffer** ppBuffer);
    public:
        CGPUBuffer() :
            m_bufferSize(0),
            m_heapSize(0),
            m_bufferSlot(0),
            m_bufferType(EBufferType::StructuredBuffer)
        {
        }

        ~CGPUBuffer()
        {
            m_numElems = 0;
        }

        void Create(IRenderer* pRenderer, EBufferType bufferType, uint32 numElems, uint32 elemSize, uint32 bindFlags);

        //**********************************************************************
        // IRefCount
        //**********************************************************************
        virtual uint32 AddRef() override { return CRefCount::AddRef(); }
        virtual uint32 Release() override { return CRefCount::Release(); }

        //**********************************************************************
        // IGPUBuffer
        //**********************************************************************
        virtual EBufferType GetBufferType() override { return m_bufferType; }
        virtual CComPtr<ID3D11Buffer> GetBuffer() override { return m_spBuffer; }
        virtual CComPtr<ID3D11Buffer> GetStagingBuffer() override { return m_spStagingBuffer; }
        virtual CComPtr<ID3D11UnorderedAccessView> GetUAView() override { return m_spUAView; }
        virtual CComPtr<ID3D11ShaderResourceView> GetSRView() override { return m_spSRView; }
        virtual void CopyFromCPU(IRenderer* pRenderer, uint8* pData) override;
        virtual void CopyToCPU(IRenderer* pRenderer, uint8* pData) override;
    };

    //**********************************************************************
    // Class: CShader
    // Defines a shader used to render materials on an object
    //
    // A shader defines a material on a renderable. It is comprised of a pixel
    // shader, a vertex shader, and constants passed to those shaders.
    //
    // Module:
    // {Link:import Rendering.Caustic.Shader;{Rendering/Caustic/Shader.ixx}}
    //**********************************************************************
    class CShader : public IShader, public CRefCount
    {
        enum MatrixTypesAvail
        {
            PSMatrixAvail_world = 1,
            PSMatrixAvail_worldInv = 1 << 1,
            PSMatrixAvail_worldInvTranspose = 1 << 2,
            PSMatrixAvail_view = 1 << 3,
            PSMatrixAvail_viewInv = 1 << 4,
            PSMatrixAvail_proj = 1 << 5,
            PSMatrixAvail_projInv = 1 << 6,
            PSMatrixAvail_worldView = 1 << 7,
            PSMatrixAvail_worldViewInv = 1 << 8,
            PSMatrixAvail_worldViewProj = 1 << 9,
            PSMatrixAvail_worldViewProjInv = 1 << 10,

            VSMatrixAvail_world = 1 << 11,
            VSMatrixAvail_worldInv = 1 << 12,
            VSMatrixAvail_worldInvTranspose = 1 << 13,
            VSMatrixAvail_view = 1 << 14,
            VSMatrixAvail_viewInv = 1 << 15,
            VSMatrixAvail_proj = 1 << 16,
            VSMatrixAvail_projInv = 1 << 17,
            VSMatrixAvail_worldView = 1 << 18,
            VSMatrixAvail_worldViewInv = 1 << 19,
            VSMatrixAvail_worldViewProj = 1 << 20,
            VSMatrixAvail_worldViewProjInv = 1 << 21,
        };
        uint32 m_matricesAvail; // Combination of MatrixTypesAvail flags indicating which matrices are referenced by the shader
        std::wstring m_name;
        std::vector<D3D11_INPUT_ELEMENT_DESC> m_layout;
        CComPtr<ID3D11SamplerState> m_spSamplerState;
        CComPtr<ID3D11InputLayout> m_spLayout;
        CComPtr<ID3D11PixelShader> m_spPixelShader;
        CComPtr<ID3D11VertexShader> m_spVertexShader;
        CComPtr<ID3D11ComputeShader> m_spComputeShader;
        SBuffer m_vertexConstants;
        SBuffer m_pixelConstants;
        SBuffer m_computeConstants;
        std::map<std::wstring, ShaderParamInstance> m_psParams;
        std::map<std::wstring, ShaderParamInstance> m_vsParams;
        std::map<std::wstring, ShaderParamInstance> m_csParams;
        CRefObj<IShaderInfo> m_spShaderInfo;
        int m_xThreads;
        int m_yThreads;
        int m_zThreads;
        int m_maxTextureSlot;
    protected:
        void DetermineMatricesUsed();
        void PushMatrix(const wchar_t* pParamName, std::any mat, uint32 vsmask, uint32 psmask);
        void PushLights(std::vector<CRefObj<ILight>>& lights);
        void PushMatrices(IRenderer* pRenderer, DirectX::XMMATRIX *pWorld);
        uint32 ComputeParamSize(ShaderParamDef *pParams, uint32 numParams, std::map<std::wstring, ShaderParamInstance> &params);
        uint32 ShaderTypeSize(ShaderParamDef& paramDef);
        void PushConstants(IRenderer* pRenderer, SBuffer *pBuffer, std::map<std::wstring, ShaderParamInstance> &params);
        void ClearSamplers(IRenderer* pRenderer);
        void PushSamplers(IRenderer* pRenderer, std::map<std::wstring, ShaderParamInstance>& params, bool isPixelShader);
        void PushBuffers(IRenderer* pRenderer, std::map<std::wstring, ShaderParamInstance>& params);
        void PopBuffers(IRenderer* pRenderer, std::map<std::wstring, ShaderParamInstance>& params);
        void SetParam(const std::wstring& paramName, const std::any& value, std::map<std::wstring, ShaderParamInstance>& params);
        void SetParam(const wchar_t* paramName, const std::any& value, std::map<std::wstring, ShaderParamInstance>& params);
        void SetParam(const std::wstring& paramName, int index, const std::any& value, std::map<std::wstring, ShaderParamInstance>& params);
        void SetParam(const wchar_t* paramName, int index, const std::any& value, std::map<std::wstring, ShaderParamInstance>& params);
    public:
        void Create(IRenderer *pRenderer, const wchar_t *pShaderName, IShaderInfo *pShaderInfo, ID3DBlob *pPSBlob, ID3DBlob* pVSBlob, ID3DBlob* pCSBlob);
        void CreateBuffer(ID3D11Device* pDevice, uint32 bufSize, uint32 bindFlags, uint32 cpuAccessFlags, D3D11_USAGE usage, uint32 miscFlags, uint32 stride, uint32 alignment, SBuffer* pBuffer, ID3D11Buffer **ppBuffer);
        void CreateConstantBuffer(ID3D11Device *pDevice, ShaderParamDef *pDefs, uint32 paramsSize, std::map<std::wstring, ShaderParamInstance> &params, SBuffer *pConstantBuffer);

        CShader() :
            m_xThreads(32),
            m_yThreads(32),
            m_zThreads(1),
            m_maxTextureSlot(0),
            m_matricesAvail(0)
        {
        }

        //**********************************************************************
        // IRefCount
        //**********************************************************************
        virtual uint32 AddRef() override { return CRefCount::AddRef(); }
        virtual uint32 Release() override { return CRefCount::Release(); }

        //**********************************************************************
        // IShader
        //**********************************************************************
        virtual CRefObj<IShader> Clone(ID3D11Device *pDevice) override;
        virtual std::wstring &Name() override { return m_name; }
        virtual void BeginRender(IRenderer* pRenderer, IRenderMaterial* pMaterial, std::vector<CRefObj<ILight>>& lights, DirectX::XMMATRIX* pWorld) override;
        virtual void SetPSParam(const std::wstring& paramName, const std::any& value) override;
        virtual void SetPSParam(const wchar_t* paramName, const std::any& value) override;
        virtual void SetPSParamFloat(const std::wstring& paramName, float value) override;
        virtual void SetPSParamFloat(const wchar_t* paramName, float value) override;
        virtual void SetPSParamInt(const std::wstring& paramName, int value) override;
        virtual void SetPSParamInt(const wchar_t* paramName, int value) override;
        virtual void SetPSParam(const std::wstring& paramName, int index, const std::any& value) override;
        virtual void SetPSParam(const wchar_t* paramName, int index, const std::any& value) override;
        virtual void SetVSParam(const std::wstring& paramName, const std::any& value) override;
        virtual void SetVSParam(const wchar_t* paramName, const std::any& value) override;
        virtual void SetVSParamFloat(const std::wstring& paramName, float value) override;
        virtual void SetVSParamFloat(const wchar_t* paramName, float value) override;
        virtual void SetVSParamInt(const std::wstring& paramName, int value) override;
        virtual void SetVSParamInt(const wchar_t* paramName, int value) override;
        virtual void SetVSParam(const std::wstring& paramName, int index, const std::any& value) override;
        virtual void SetVSParam(const wchar_t* paramName, int index, const std::any& value) override;
        virtual void SetCSParam(const std::wstring& paramName, const std::any& value) override;
        virtual void SetCSParam(const wchar_t* paramName, const std::any& value) override;
        virtual void SetCSParamFloat(const std::wstring& paramName, float value) override;
        virtual void SetCSParamFloat(const wchar_t* paramName, float value) override;
        virtual void SetCSParamInt(const std::wstring& paramName, int value) override;
        virtual void SetCSParamInt(const wchar_t* paramName, int value) override;
        virtual void SetCSParam(const std::wstring& paramName, int index, const std::any& value) override;
        virtual void SetCSParam(const wchar_t* paramName, int index, const std::any& value) override;
        virtual void EndRender(IRenderer* pRenderer) override;
        virtual CRefObj<IShaderInfo> GetShaderInfo() override;
        virtual void Dispatch(IRenderer* pRenderer, int xThreads, int yThreads, int zThreads) override;
    };
}
