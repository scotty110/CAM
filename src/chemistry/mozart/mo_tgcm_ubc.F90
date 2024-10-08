!---------------------------------------------------------------
!	... tgcm upper bndy values
!---------------------------------------------------------------

      module mo_tgcm_ubc

        use ppgrid,           only : pver
        use shr_kind_mod,     only : r8 => shr_kind_r8
        use constituents,     only : pcnst, cnst_fixed_ubc

        use cam_abortutils,   only: endrun
        use cam_logfile,      only: iulog

        use tracer_data,      only : trfld,trfile,MAXTRCRS
        use cam_history,      only : addfld, horiz_only

        implicit none

        private
        public  :: tgcm_ubc_inti, set_tgcm_ubc, tgcm_timestep_init

        save

        type(trfld), pointer :: fields(:)
        type(trfile)         :: file

        integer :: ub_nspecies
        character(len=16) :: ubc_name(MAXTRCRS)
        integer :: map(MAXTRCRS)

        logical :: ubc_from_tgcm(pcnst)  = .false.

      contains

      subroutine tgcm_ubc_inti( tgcm_ubc_file, tgcm_ubc_data_type, tgcm_ubc_cycle_yr, tgcm_ubc_fixed_ymd, tgcm_ubc_fixed_tod)
        !------------------------------------------------------------------
        !	... initialize upper boundary values
        !------------------------------------------------------------------
        use tracer_data, only : trcdata_init

        use constituents,    only : cnst_get_ind

        !------------------------------------------------------------------
        !	... dummy args
        !------------------------------------------------------------------
        character(len=*),   intent(in)     :: tgcm_ubc_file
        integer,            intent(in)     :: tgcm_ubc_cycle_yr
        integer,            intent(in)     :: tgcm_ubc_fixed_ymd
        integer,            intent(in)     :: tgcm_ubc_fixed_tod
        character(len=32),  intent(in)     :: tgcm_ubc_data_type


        ! local vars
        integer :: vid, i,ii, ierr

        character(len=256), parameter :: filelist = ' '
        character(len=256), parameter :: datapath = ' '
        logical,            parameter :: rmv_file = .false.
        integer,            parameter :: nubc = 1
        character(len=4),   parameter :: species(nubc) = (/'H2  '/)
        character(len=4)              :: specifier(nubc) = ' '

        character(len=*), parameter :: prefix = 'tgcm_ubc_inti: '

        ii = 0

        do i = 1,nubc
           call cnst_get_ind( species(i), vid, abort=.false. )
           if( vid > 0 ) then
              if( cnst_fixed_ubc(vid) ) then
                 ii = ii+1
                 specifier(ii) = species(i) ! set specifier to the species that actually
                                            ! are registered to have a specified upper bounary
                                            ! so that the species mapping is correct
                 ubc_from_tgcm(vid) = .true.
                 map(ii) = vid              ! elements in map array correspond to elements in specifier
                 ubc_name(ii) = trim(species(i))//'_tgcm'
                 call addfld( ubc_name(ii), horiz_only, 'I', 'kg/kg', 'upper boundary mmr' )
              end if
           end if
        enddo

        ub_nspecies = count( ubc_from_tgcm )

        if (ub_nspecies > 0) then
           file%top_bndry = .true.
           allocate(file%in_pbuf(size(specifier)), stat=ierr)
           if (ierr /= 0) call endrun(prefix//'allocate error : file%in_pbuf')
           file%in_pbuf(:) = .false.
           call trcdata_init( specifier, tgcm_ubc_file, filelist, datapath, fields, file, &
                              rmv_file, tgcm_ubc_cycle_yr, tgcm_ubc_fixed_ymd, tgcm_ubc_fixed_tod, tgcm_ubc_data_type)
        endif

      end subroutine tgcm_ubc_inti

      subroutine tgcm_timestep_init(pbuf2d, state )

        use tracer_data,  only : advance_trcdata
        use physics_types,only : physics_state
        use ppgrid,       only : begchunk, endchunk
        use physics_buffer, only : physics_buffer_desc

        !--------------------------------------------------------------------
        !	... Advance ub values
        !--------------------------------------------------------------------

        ! args
        type(physics_state), intent(in):: state(begchunk:endchunk)
        type(physics_buffer_desc), pointer :: pbuf2d(:,:)

        if (ub_nspecies > 0) then
           call advance_trcdata( fields, file, state, pbuf2d )
        endif

      end subroutine tgcm_timestep_init

      subroutine set_tgcm_ubc( lchunk, ncol, mmr )
        !--------------------------------------------------------------------
        !	... Set the upper boundary values h2o, h2, and h
        !--------------------------------------------------------------------

        use ppgrid,      only : pcols
        use cam_history, only : outfld

        !--------------------------------------------------------------------
        !	... dummy args
        !--------------------------------------------------------------------
        integer,  intent(in)    :: lchunk            ! chunk id
        integer,  intent(in)    :: ncol              ! columns in chunk
        real(r8), intent(inout) :: mmr(pcols,pcnst)

        !--------------------------------------------------------------------
        !	... local variables
        !--------------------------------------------------------------------
        integer  :: m,n

        if (ub_nspecies > 0) then
           do m = 1,ub_nspecies
              n = map(m)
              mmr(:ncol,n) = fields(m)%data(:ncol,1,lchunk)
              call outfld( ubc_name(m), mmr(:ncol,n), ncol, lchunk )
           enddo
        endif

      end subroutine set_tgcm_ubc

      end module mo_tgcm_ubc
