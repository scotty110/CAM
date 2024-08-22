module simple_print
    use mpi
    use camsrfexch,     only: cam_in_t
    use physics_types,  only: physics_state
    implicit none

    ! Public interface
    public :: print_cam

contains

    subroutine print_cam(phys_state, cam_in)
        type(physics_state), intent(in) :: phys_state(:) 
        type(cam_in_t), intent(in) :: cam_in(:)
        
        integer :: mpi_rank, mpi_comm, ierr
        mpi_comm = MPI_COMM_WORLD
        call MPI_Comm_rank(mpi_comm, mpi_rank, ierr)
        
        print *, "MPI Rank:", mpi_rank
        print *, "Number of elements in phys_state array:", size(phys_state)
        
        print *, "State:"
        if (size(phys_state) > 0) then
            print *, "  Temp state shape: ", shape(phys_state(1)%t)
            print *, "  Pressure state shape: ", shape(phys_state(1)%pmid)
        end if

        if (size(cam_in) > 0) then
            print *, "  Land Area Fraction: ", shape(cam_in(1)%landfrac)
            print *, "  Sea Ice Area Fraction: ", shape(cam_in(1)%icefrac)
            print *, "  Ocean Area Fraction: ", shape(cam_in(1)%ocnfrac)
        end if

    end subroutine print_cam

end module simple_print
