module MOD_GLOBAL_VAR
implicit none
public
save
!!!!!!!!variables for iol which is a list in R code
! variables for trans_pr
type iols
    character*15,allocatable,dimension(:) :: transpr_age_gr
    character*15,allocatable,dimension(:) :: transpr_sex
    real,allocatable,dimension(:)         :: transpr_surv_ill
    real,allocatable,dimension(:)         :: transpr_icu_risk
    real,allocatable,dimension(:)         :: transpr_surv_icu
    character*15,dimension(5)             :: titel
    ! variables for pop
    integer,allocatable,dimension(:)      :: pop_distid
    character(10),allocatable,dimension(:):: pop_date
    character(1),allocatable,dimension(:) :: pop_sex
    character(2),allocatable,dimension(:) :: pop_age
    integer,allocatable,dimension(:)      :: pop_total
    ! variable for seed
    integer,allocatable,dimension(:)      :: seed_distid
    character*10,allocatable,dimension(:) :: seed_date
    integer,allocatable,dimension(:)      :: seed_cases
    character*15,dimension(3)             :: seed_titel
    !variables for seed_dea
    integer,allocatable,dimension(:)      :: death_distid
    character*10,allocatable,dimension(:) :: death_date
    integer,allocatable,dimension(:)      :: death_cases
    !variable for connect_total
    integer,dimension(401)                :: connect_total_distid
    character*15,dimension(401)           :: connect_total_name
    real,dimension(401,401)               :: connect_total
    character*15                          :: connect_titel
    !variable for connect_work
    integer,dimension(401)                :: connect_work_distid
    character*15,dimension(401)           :: connect_work_name
    real,dimension(401,401)               :: connect_work
    !variable for states
    integer,allocatable,dimension(:)      :: states_code
    integer,allocatable,dimension(:)      :: states_inhabitant
    character*15,allocatable,dimension(:) :: states_shortcut
    character*30,allocatable,dimension(:) :: states_name
    character*15,dimension(4)             :: state_titel
    !variable for counties
    integer,allocatable,dimension(:)      :: counties_dist_id
    character*50,allocatable,dimension(:) :: counties_name
    real,allocatable,dimension(:)         :: counties_area
    integer,allocatable,dimension(:)      :: counties_inhabitants
end type

!variable declaration for pspace
type ps_scalar
    real                                  :: param
    character*10                          :: var_type
    character*10                          :: name
end type

type ps_vector
    real,allocatable                      :: param(:,:)
    character*20,allocatable              :: param_char(:,:)                        
    character*10                          :: var_type
end type

type pspaces
!   type(ps_scalar)                       :: sam_size,R0,icu_dur,mod_surv_ill,lockdown_effect,w_int,w_obs_by_state
    type(ps_scalar),dimension(8)          :: Ps_scalar_list
    type(ps_vector)                       :: ROeffect_ps
end type
    
!variables for control panel
!type(iols)                    :: iol
character*15                  :: exec_procedure
logical                       :: restrict
character*35,allocatable      :: counties(:)
! integer,allocatable           :: counties_integer(:)
integer                       :: time_n
character*10                  :: seed_date
logical                       :: import_R0_matrix
integer                       :: lhc_samples
character*100                 :: export_name

end module
