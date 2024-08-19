program inference

    ! Import precision info from iso
    use, intrinsic :: iso_fortran_env, only : dp => r8 

    ! Import our library for interfacing with PyTorch
    use ftorch

    implicit none

    ! Set up Fortran data structures
    real(wp), dimension(1,70,3,4) :: in_x
    real(wp), dimension(1,3,4) :: landmass
    real(wp), dimension(1,70,2,4) :: out_y

    ! Set up tensor layouts
    integer :: tensor_layout_4d(4) = [4,3,2,1]
    integer :: tensor_layout_3d(3) = [3,2,1]
    
    ! Set up Torch data structures
    ! The net, a vector of input tensors (in this case we only have one), and the output tensor
    type(torch_model) :: model
    type(torch_tensor), dimension(2) :: in_tensors
    type(torch_tensor), dimension(1) :: out_tensors

    ! Load the Model (Due to compiler issues use the CPU)
    call torch_model_load(model, TODO, torch_kCPU)

    ! Push data
    ! Push data
    call torch_tensor_from_array(in_tensors(1), in_x, tensor_layout_4d, torch_kCPU)
    call torch_tensor_from_array(in_tensors(2), landmass, tensor_layout_3d, torch_kCPU)
    call torch_tensor_from_array(out_tensors(1), out_y, tensor_layout_4d, torch_kCPU)

    ! Run the model
    !call torch_model_forward(model, in_tensors, out_tensors)

    call torch_model_delete(model)
    !call torch_tensor_delete(in_tensors(1))
    !call torch_tensor_delete(out_tensors(1))

end program inference