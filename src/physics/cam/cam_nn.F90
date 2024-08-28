module cam_nn
    use ftorch
    use camsrfexch,     only: cam_in_t
    use physics_types,  only: physics_state
    implicit none

    public :: torch_inference

    ! Declare the torch model without initializing it here
    type(torch_model), save :: model
    logical, save :: model_initialized = .false.

contains

    subroutine init_torch_model(model)
        ! Initialize the model
        type(torch_model), intent(inout) :: model
        call torch_model_load(model, "/weights/cam_nn.pt", torch_kCPU)
    end subroutine init_torch_model

    subroutine torch_inference(phys_state, cam_in)
        ! CAM Types
        type(physics_state), intent(inout) :: phys_state(:)
        type(cam_in_t),      intent(in) :: cam_in(:)

        ! Torch Types
        type(torch_tensor), dimension(1) :: in_tensors
        type(torch_tensor), dimension(1) :: out_tensors

        ! Make input tensors
        real(8), allocatable :: phys_state_t_array(:,:,:)
        real(8), allocatable :: new_phys_state_t_array(:,:,:)

        integer :: tensor_layout_4d(4) = [4,3,2,1]
        integer :: tensor_layout_3d(3) = [3,2,1]

        ! ints 
        integer :: i, m, n

        ! Initialize the model if it has not been initialized yet
        if (.not. model_initialized) then
            call init_torch_model(model)
            model_initialized = .true.
        end if

        ! Check for empty arrays
        if (size(phys_state) == 0) then
            print *, "ERROR: Empty physics state array"
            stop
        end if

        if(size(cam_in) == 0) then
            print *, "ERROR: Empty cam_in array"
            stop
        end if

        ! Make Temp Tensor
        m = size(phys_state(1)%t, 1)  ! Number of rows
        n = size(phys_state(1)%t, 2)  ! Number of columns

        allocate(phys_state_t_array(size(phys_state), m, n))
        allocate(new_phys_state_t_array(size(phys_state), m, n))

        do i = 1, size(phys_state)
            phys_state_t_array(i, :, :) = phys_state(i)%t
        end do

        ! Make Torch Tensors for input and output
        call torch_tensor_from_array(in_tensors(1), phys_state_t_array, tensor_layout_3d, torch_kCPU)
        !call torch_tensor_from_array(in_tensors(1), in_x, tensor_layout_4d, torch_kCPU)
        !call torch_tensor_from_array(in_tensors(2), landmass, tensor_layout_3d, torch_kCPU)

        call torch_tensor_from_array(out_tensors(1), new_phys_state_t_array, tensor_layout_3d, torch_kCPU)

        ! Perform inference
        call torch_model_forward(model, in_tensors, out_tensors)

        ! Free Memory
        call torch_tensor_delete(in_tensors(1))
        !call torch_tensor_delete(in_tensors(2))
        call torch_tensor_delete(out_tensors(1))
    
    end subroutine torch_inference

end module cam_nn
