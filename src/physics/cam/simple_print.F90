module simple_print
    !use camsrfexch,     only: cam_in_t
    use physics_types,  only: physics_state
    implicit none

    ! Public interface
    public :: print_cam

contains

    subroutine print_cam(phys_state)
        type(physics_state), intent(in) :: phys_state(:) 
        
        print *, "Entering print_cam subroutine"
        print *, "Number of elements in phys_state array:", size(phys_state)
        
        if (size(phys_state) > 0) then
            if (allocated(phys_state(1)%t)) then
                print *, "Shape of t for the first element:", shape(phys_state(1)%t)
                print *, "Contents of t for the first element:", phys_state(1)%t
            else
                print *, "Array t is not allocated in the first element."
            end if
        else
            print *, "phys_state array is empty."
        end if

    end subroutine print_cam

end module simple_print