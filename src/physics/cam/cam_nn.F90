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
        type(torch_tensor), dimension(2) :: in_tensors
        type(torch_tensor), dimension(1) :: out_tensors

        ! Make input tensors ???

        integer :: tensor_layout_4d(4) = [4,3,2,1]
        integer :: tensor_layout_3d(3) = [3,2,1]

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

        ! Make Torch Tensors for input and output
        !call torch_tensor_from_array(in_tensors(1), in_x, tensor_layout_4d, torch_kCPU)
        !call torch_tensor_from_array(in_tensors(2), landmass, tensor_layout_3d, torch_kCPU)

        !call torch_tensor_from_array(out_tensors(1), out_y, tensor_layout_4d, torch_kCPU)

        ! Perform inference
        !call torch_model_forward(model, in_tensors, out_tensors)

        ! Free Memory
        !call torch_tensor_delete(in_tensors(1))
        !call torch_tensor_delete(in_tensors(2))
        !call torch_tensor_delete(out_tensors(1))
    
    end subroutine torch_inference

end module cam_nn
