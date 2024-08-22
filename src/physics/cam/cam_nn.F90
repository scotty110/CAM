module cam_nn 
    ! Import precision info from iso
    use, intrinsic :: iso_fortran_env, only : dp => r8

    use ftorch

    implicit none

    ! Global variables
    type(torch_model) :: model

    public :: init_model, inference 

contains
     subroutine init_model(model)
        ! Initialize the model
        call torch_model_load(model, "/weights/cam_nn.pt", torch_kCPU)
    end subroutine init_model

    subroutine inference(phys_state, cam_in)
        ! Vars
        type(physics_state), intent(in) :: phys_state(:) 
        type(cam_in_t), intent(in) :: cam_in(:)

        type(torch_tensor), dimension(2) :: in_tensors
        type(torch_tensor), dimension(1) :: out_tensors

        ! Make input tensors ???

        integer :: tensor_layout_4d(4) = [4,3,2,1]
        integer :: tensor_layout_3d(3) = [3,2,1]

        ! Check for empty arrays
        if (size(physics_state) == 0) then
            print *, "ERROR: Empty physics state array"
            stop
        end if

        if(size(cam_in) == 0) then
            print *, "ERROR: Empty cam_in array"
            stop
        end if

        ! Make Torch Tensors for input and output
        call torch_tensor_from_array(in_tensors(1), in_x, tensor_layout_4d, torch_kCPU)
        call torch_tensor_from_array(in_tensors(2), landmass, tensor_layout_3d, torch_kCPU)

        call torch_tensor_from_array(out_tensors(1), out_y, tensor_layout_4d, torch_kCPU)

        ! Perform inference
        call torch_model_forward(model, in_tensors, out_tensors)

        ! Free Memory
        call torch_tensor_delete(in_tensors(1))
        call torch_tensor_delete(in_tensors(2))
        call torch_tensor_delete(out_tensors(1))
    
    end subroutine inference




end module cam_nn