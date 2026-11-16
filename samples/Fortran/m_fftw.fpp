!>
!! @file
!! @brief Contains module m_fftw

#:include 'macros.fpp'

!> @brief Forward and inverse FFT wrappers (FFTW/cuFFT/hipFFT) for azimuthal Fourier filtering in cylindrical geometries
module m_fftw

    use, intrinsic :: iso_c_binding

    use m_derived_types
    use m_global_parameters
    use m_mpi_proxy
#if defined(MFC_GPU) && defined(__PGI)
    use cufft
#elif defined(MFC_GPU)
    use hipfort
    use hipfort_check
    use hipfort_hipfft
#endif

    implicit none

    private; public :: s_initialize_fftw_module, s_apply_fourier_filter, s_finalize_fftw_module

#if !defined(MFC_GPU)
    include 'fftw3.f03'
#endif

    type(c_ptr)                        :: fwd_plan, bwd_plan
    type(c_ptr)                        :: fftw_real_data, fftw_cmplx_data, fftw_fltr_cmplx_data
    integer                            :: real_size, cmplx_size, x_size, batch_size, Nfq, i2
    real(c_double), pointer            :: data_real(:)        !< Real data
    complex(c_double_complex), pointer :: data_cmplx(:)       !< Complex data in Fourier space
    complex(c_double_complex), pointer :: data_fltr_cmplx(:)  !< Filtered complex data in Fourier space
#if defined(MFC_GPU)
    $:GPU_DECLARE(create='[real_size, cmplx_size, x_size, batch_size, Nfq, i2]')

    real(dp), allocatable, target    :: data_real_gpu(:)
    complex(dp), allocatable, target :: data_cmplx_gpu(:)
    complex(dp), allocatable, target :: data_fltr_cmplx_gpu(:)
    $:GPU_DECLARE(create='[data_real_gpu, data_cmplx_gpu, data_fltr_cmplx_gpu]')

    !> @cond
#if defined(__PGI)
    integer :: fwd_plan_gpu, bwd_plan_gpu
#else
    !> @endcond
    type(c_ptr) :: fwd_plan_gpu, bwd_plan_gpu
    !> @cond
#endif
    !> @endcond

    integer, allocatable :: gpu_fft_size(:), iembed(:), oembed(:)
    integer              :: istride, ostride, rank
#endif

contains

    !> Initialize the FFTW module
    impure subroutine s_initialize_fftw_module

        integer :: ierr  !< Generic flag used to identify and report GPU errors
        ! Size of input array going into DFT

        real_size = p + 1
        ! Size of output array coming out of DFT
        cmplx_size = (p + 1)/2 + 1

        x_size = m + 1
        batch_size = x_size*sys_size

#if defined(MFC_GPU)
        rank = 1; istride = 1; ostride = 1
        allocate (gpu_fft_size(1:rank), iembed(1:rank), oembed(1:rank))

        gpu_fft_size(1) = real_size
        iembed(1) = real_size
        oembed(1) = cmplx_size
        $:GPU_ENTER_DATA(copyin='[real_size, cmplx_size, x_size, sys_size, batch_size, Nfq]')
        $:GPU_UPDATE(device='[real_size, cmplx_size, x_size, sys_size, batch_size]')
#else
        ! Allocate input and output DFT data sizes
        fftw_real_data = fftw_alloc_real(int(real_size, c_size_t))
        fftw_cmplx_data = fftw_alloc_complex(int(cmplx_size, c_size_t))
        fftw_fltr_cmplx_data = fftw_alloc_complex(int(cmplx_size, c_size_t))
        ! Associate input and output data pointers with allocated memory
        call c_f_pointer(fftw_real_data, data_real, [real_size])
        call c_f_pointer(fftw_cmplx_data, data_cmplx, [cmplx_size])
        call c_f_pointer(fftw_fltr_cmplx_data, data_fltr_cmplx, [cmplx_size])

        ! Generate plans for forward and backward DFTs
        fwd_plan = fftw_plan_dft_r2c_1d(real_size, data_real, data_cmplx, FFTW_ESTIMATE)
        bwd_plan = fftw_plan_dft_c2r_1d(real_size, data_fltr_cmplx, data_real, FFTW_ESTIMATE)
#endif

#if defined(MFC_GPU)
        @:ALLOCATE(data_real_gpu(1:real_size*x_size*sys_size))
        @:ALLOCATE(data_cmplx_gpu(1:cmplx_size*x_size*sys_size))
        @:ALLOCATE(data_fltr_cmplx_gpu(1:cmplx_size*x_size*sys_size))

#if defined(__PGI)
        ierr = cufftPlanMany(fwd_plan_gpu, rank, gpu_fft_size, iembed, istride, real_size, oembed, ostride, cmplx_size, &
                             & CUFFT_D2Z, batch_size)
        ierr = cufftPlanMany(bwd_plan_gpu, rank, gpu_fft_size, iembed, istride, cmplx_size, oembed, ostride, real_size, &
                             & CUFFT_Z2D, batch_size)
#else
        ierr = hipfftPlanMany(fwd_plan_gpu, rank, gpu_fft_size, iembed, istride, real_size, oembed, ostride, cmplx_size, &
                              & HIPFFT_D2Z, batch_size)
        ierr = hipfftPlanMany(bwd_plan_gpu, rank, gpu_fft_size, iembed, istride, cmplx_size, oembed, ostride, real_size, &
                              & HIPFFT_Z2D, batch_size)
#endif
#endif

    end subroutine s_initialize_fftw_module

    !> Apply a Fourier low-pass filter in the azimuthal direction to remove high-frequency content
    impure subroutine s_apply_fourier_filter(q_cons_vf)

        type(scalar_field), dimension(sys_size), intent(inout) :: q_cons_vf
        integer                                                :: i, j, k, l  !< Generic loop iterators
        integer                                                :: ierr        !< Generic flag used to identify and report GPU errors
        ! Restrict filter to processors that have cells adjacent to axis

        if (bc_y%beg >= 0) return
#if defined(MFC_GPU)
        $:GPU_PARALLEL_LOOP(collapse=3)
        do k = 1, sys_size
            do j = 0, m
                do l = 1, cmplx_size
                    data_fltr_cmplx_gpu(l + j*cmplx_size + (k - 1)*cmplx_size*x_size) = (0_dp, 0_dp)
                end do
            end do
        end do
        $:END_GPU_PARALLEL_LOOP()

        $:GPU_PARALLEL_LOOP(collapse=3)
        do k = 1, sys_size
            do j = 0, m
                do l = 0, p
                    data_real_gpu(l + j*real_size + 1 + (k - 1)*real_size*x_size) = q_cons_vf(k)%sf(j, 0, l)
                end do
            end do
        end do
        $:END_GPU_PARALLEL_LOOP()

        #:call GPU_HOST_DATA(use_device_addr='[data_real_gpu, data_cmplx_gpu, data_fltr_cmplx_gpu]')
#if defined(__PGI)
            ierr = cufftExecD2Z(fwd_plan_gpu, data_real_gpu, data_cmplx_gpu)
#else
            ierr = hipfftExecD2Z(fwd_plan_gpu, data_real_gpu, data_cmplx_gpu)
            call hipCheck(hipDeviceSynchronize())
#endif
        #:endcall GPU_HOST_DATA
        Nfq = 3
        $:GPU_UPDATE(device='[Nfq]')

        $:GPU_PARALLEL_LOOP(collapse=3)
        do k = 1, sys_size
            do j = 0, m
                do l = 1, Nfq
                    data_fltr_cmplx_gpu(l + j*cmplx_size + (k - 1)*cmplx_size*x_size) = data_cmplx_gpu(l + j*cmplx_size + (k - 1) &
                                        & *cmplx_size*x_size)
                end do
            end do
        end do
        $:END_GPU_PARALLEL_LOOP()

        #:call GPU_HOST_DATA(use_device_addr='[data_real_gpu, data_cmplx_gpu, data_fltr_cmplx_gpu]')
#if defined(__PGI)
            ierr = cufftExecZ2D(bwd_plan_gpu, data_fltr_cmplx_gpu, data_real_gpu)
#else
            ierr = hipfftExecZ2D(bwd_plan_gpu, data_fltr_cmplx_gpu, data_real_gpu)
            call hipCheck(hipDeviceSynchronize())
#endif
        #:endcall GPU_HOST_DATA

        $:GPU_PARALLEL_LOOP(collapse=3)
        do k = 1, sys_size
            do j = 0, m
                do l = 0, p
                    data_real_gpu(l + j*real_size + 1 + (k - 1)*real_size*x_size) = data_real_gpu(l + j*real_size + 1 + (k - 1) &
                                  & *real_size*x_size)/real(real_size, dp)
                    q_cons_vf(k)%sf(j, 0, l) = data_real_gpu(l + j*real_size + 1 + (k - 1)*real_size*x_size)
                end do
            end do
        end do
        $:END_GPU_PARALLEL_LOOP()

        do i = 1, fourier_rings
            i2 = i
            $:GPU_UPDATE(device='[i2]')

            $:GPU_PARALLEL_LOOP(collapse=3)
            do k = 1, sys_size
                do j = 0, m
                    do l = 1, cmplx_size
                        data_fltr_cmplx_gpu(l + j*cmplx_size + (k - 1)*cmplx_size*x_size) = (0_dp, 0_dp)
                    end do
                end do
            end do
            $:END_GPU_PARALLEL_LOOP()

            $:GPU_PARALLEL_LOOP(collapse=3)
            do k = 1, sys_size
                do j = 0, m
                    do l = 0, p
                        data_real_gpu(l + j*real_size + 1 + (k - 1)*real_size*x_size) = q_cons_vf(k)%sf(j, i2, l)
                    end do
                end do
            end do
            $:END_GPU_PARALLEL_LOOP()

            #:call GPU_HOST_DATA(use_device_addr='[data_real_gpu, data_cmplx_gpu, data_fltr_cmplx_gpu]')
#if defined(__PGI)
                ierr = cufftExecD2Z(fwd_plan_gpu, data_real_gpu, data_cmplx_gpu)
#else
                ierr = hipfftExecD2Z(fwd_plan_gpu, data_real_gpu, data_cmplx_gpu)
                call hipCheck(hipDeviceSynchronize())
#endif
            #:endcall GPU_HOST_DATA

            Nfq = min(floor(2_dp*real(i, dp)*pi), cmplx_size)
            $:GPU_UPDATE(device='[Nfq]')

            $:GPU_PARALLEL_LOOP(collapse=3)
            do k = 1, sys_size
                do j = 0, m
                    do l = 1, Nfq
                        data_fltr_cmplx_gpu(l + j*cmplx_size + (k - 1)*cmplx_size*x_size) = data_cmplx_gpu(l + j*cmplx_size + (k &
                                            & - 1)*cmplx_size*x_size)
                    end do
                end do
            end do
            $:END_GPU_PARALLEL_LOOP()

            #:call GPU_HOST_DATA(use_device_addr='[data_real_gpu, data_cmplx_gpu, data_fltr_cmplx_gpu]')
#if defined(__PGI)
                ierr = cufftExecZ2D(bwd_plan_gpu, data_fltr_cmplx_gpu, data_real_gpu)
#else
                ierr = hipfftExecZ2D(bwd_plan_gpu, data_fltr_cmplx_gpu, data_real_gpu)
                call hipCheck(hipDeviceSynchronize())
#endif
            #:endcall GPU_HOST_DATA

            $:GPU_PARALLEL_LOOP(collapse=3)
            do k = 1, sys_size
                do j = 0, m
                    do l = 0, p
                        data_real_gpu(l + j*real_size + 1 + (k - 1)*real_size*x_size) = data_real_gpu(l + j*real_size + 1 + (k &
                                      & - 1)*real_size*x_size)/real(real_size, dp)
                        q_cons_vf(k)%sf(j, i2, l) = data_real_gpu(l + j*real_size + 1 + (k - 1)*real_size*x_size)
                    end do
                end do
            end do
            $:END_GPU_PARALLEL_LOOP()
        end do
#else
        Nfq = 3
        do j = 0, m
            do k = 1, sys_size
                data_fltr_cmplx(:) = (0_dp, 0_dp)
                data_real(1:p + 1) = q_cons_vf(k)%sf(j, 0,0:p)
                call fftw_execute_dft_r2c(fwd_plan, data_real, data_cmplx)
                data_fltr_cmplx(1:Nfq) = data_cmplx(1:Nfq)
                call fftw_execute_dft_c2r(bwd_plan, data_fltr_cmplx, data_real)
                data_real(:) = data_real(:)/real(real_size, dp)
                q_cons_vf(k)%sf(j, 0,0:p) = data_real(1:p + 1)
            end do
        end do

        ! Apply Fourier filter to additional rings
        do i = 1, fourier_rings
            Nfq = min(floor(2_dp*real(i, dp)*pi), cmplx_size)
            do j = 0, m
                do k = 1, sys_size
                    data_fltr_cmplx(:) = (0_dp, 0_dp)
                    data_real(1:p + 1) = q_cons_vf(k)%sf(j, i,0:p)
                    call fftw_execute_dft_r2c(fwd_plan, data_real, data_cmplx)
                    data_fltr_cmplx(1:Nfq) = data_cmplx(1:Nfq)
                    call fftw_execute_dft_c2r(bwd_plan, data_fltr_cmplx, data_real)
                    data_real(:) = data_real(:)/real(real_size, dp)
                    q_cons_vf(k)%sf(j, i,0:p) = data_real(1:p + 1)
                end do
            end do
        end do
#endif

    end subroutine s_apply_fourier_filter

    !> Finalize the FFTW module
    impure subroutine s_finalize_fftw_module

#if defined(MFC_GPU)
        integer :: ierr  !< Generic flag used to identify and report GPU errors

        @:DEALLOCATE(data_real_gpu, data_fltr_cmplx_gpu, data_cmplx_gpu)
#if defined(__PGI)
        ierr = cufftDestroy(fwd_plan_gpu)
        ierr = cufftDestroy(bwd_plan_gpu)
#else
        ierr = hipfftDestroy(fwd_plan_gpu)
        ierr = hipfftDestroy(bwd_plan_gpu)
#endif
#else
        call fftw_free(fftw_real_data)
        call fftw_free(fftw_cmplx_data)
        call fftw_free(fftw_fltr_cmplx_data)

        call fftw_destroy_plan(fwd_plan)
        call fftw_destroy_plan(bwd_plan)
#endif

    end subroutine s_finalize_fftw_module

end module m_fftw
