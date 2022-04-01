module MOD_data_prepro

contains
subroutine data_prepro(iol,Counties_type,counties_integer)
use MOD_GLOBAL_VAR
use support_fun
implicit none
!
type(iols)              :: iol
integer,allocatable     :: counties_integer(:)
integer,allocatable     :: all_counties(:)
integer,allocatable     :: state_code_index(:)
integer,allocatable     :: counties_index(:)
integer,allocatable     :: temp_index(:)
real,allocatable        :: connect_total(:,:)
real,allocatable        :: connect_work(:,:)
character(len=*)        :: Counties_type

integer                 :: i
if (import_R0_matrix) then
    !read file here needs to be implemented
end if
!list of all counties codes
all_counties = get_unique(iol%pop_distid)

if (.not.(restrict)) then  
        !allocate(counties(size(all_counties)))
        !counties = all_counties
end if

if (restrict) then
        !if(all(counties == "none"))
        ! don't know how to implement it wisely
        ! just leave blank here, since it wouldn't
        ! affect the code
        if (Counties_type == "Character")then
            state_code_index = get_index_mul_char(iol%states_name,counties)
!             print *,state_code_index
            counties_index   = get_index_mul_integer(all_counties/1000,iol%states_code(state_code_index))
!             allocate(counties_integer(size(counties_index)))
            counties_integer = all_counties(counties_index)
        else
            !not relavent for the current task,can be filled later
        endif

        temp_index = get_index(iol%pop_distid,counties_integer)
        iol%pop_distid = iol%pop_distid(temp_index)
        iol%pop_date   = iol%pop_date(temp_index)
        iol%pop_sex    = iol%pop_sex(temp_index)
        iol%pop_age    = iol%pop_age(temp_index)
        iol%pop_total  = iol%pop_total(temp_index)

end if


! connectitvity data
if (restrict) then
    temp_index = get_index_mul_integer(iol%connect_total_distid,counties_integer)
!     allocate(connect_total(size(temp_index),size(temp_index)))
    connect_total = iol%connect_total(temp_index,temp_index)
    
!     deallocate(temp_index)
    temp_index = get_index_mul_integer(iol%connect_total_distid,counties_integer)
!     allocate(connect_work(size(temp_index),size(temp_index)))
    connect_work = iol%connect_total(temp_index,temp_index)
    
    ! rescale: rows sum to 1
    if (size(connect_total,dim=1) == size(connect_work,dim=1)) then
        do i = 1, size(connect_total,dim=1)
            connect_total(:,i)  =  connect_total(:,i)/sum(connect_total(:,i))
            connect_work(:,i)   =  connect_work(:,i)/sum(connect_work(:,i))
        end do
    else 
        do i = 1, size(connect_total,dim=1)
            connect_total(:,i)  =  connect_total(:,i)/sum(connect_total(:,i))
        end do
        do i = 1, size(connect_work,dim=1)
            connect_work(:,i)   =  connect_work(:,i)/sum(connect_work(:,i))
        end do
    end if

    ! should i tranpose the matrix also in fortran?
    ! data processing for iol is done
    
end if
end subroutine

end module
