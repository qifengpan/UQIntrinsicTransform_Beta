module list_variable
    use MOD_GLOBAL_VAR
    implicit none

    
    type,public :: seeds
        integer,allocatable             :: dist_id(:)
        integer,allocatable             :: cases(:)
        character*10,allocatable        :: date(:)

    end type seeds
    
    interface   set_value
        module procedure  set_value_seed
        module procedure  set_value_iol
    end interface
contains
    function init_seed(num)
        implicit none
        type(seeds) :: init_seed   
        integer     :: num
        allocate(init_seed%dist_id(num))
        allocate(init_seed%cases(num))
        allocate(init_seed%date(num))
    end function
    
    function set_value_seed(seed,index_list)
        implicit none
        type(seeds)  :: set_value_seed
        type(seeds)  :: seed
        integer      :: index_list(:)
!         integer,dimension(:)      :: value_id
!         integer,dimension(:)      :: value_cases
!         character*10,dimension(:) :: value_date
        
        set_value_seed%dist_id = seed%dist_id(index_list)
        set_value_seed%cases   = seed%cases(index_list)
        set_value_seed%date    = seed%date(index_list)
    end function
    
    function set_value_iol(iol,index_list)
        implicit none
        type(iols)   :: iol
        type(seeds)  :: set_value_iol
        integer      :: index_list(:)
        set_value_iol%dist_id  = iol%seed_distid(index_list)
        set_value_iol%date     = iol%seed_date(index_list)
        set_value_iol%cases    = iol%seed_cases(index_list)
    end function
    
end module list_variable
