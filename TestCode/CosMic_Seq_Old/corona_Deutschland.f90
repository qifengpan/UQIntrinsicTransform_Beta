!COVID-19 spatial microsimulation for Germany

!Translating the R code provided by Christian Dudel etc.
program main
USE MOD_GLOBAL_VAR
use MOD_data_prepro
use mod_kernel
implicit none
type(iols)                    :: iol
type(pspaces)                 :: pspace
!integer,allocatable      :: counties(:)
character*15                  :: Counties_type
integer                       :: iter
integer,allocatable           :: counties_integer(:)
real                          :: time_start,time_end
call cpu_time(time_start)
iter = 1
call loaddata(iol,pspace,Counties_type)
call data_prepro(iol,Counties_type,counties_integer)

call COVID19_Spatial_Microsimulation_for_Germany(iol,pspace,iter,counties_integer)
call cpu_time(time_end)
print *,"time_used ",time_end-time_start,"s"
end program



subroutine loaddata(iol,pspace,Counties_type)

USE MOD_GLOBAL_VAR
USE support_fun, only: get_file_N
implicit none
integer                    :: i,j,k,it_ss
integer                    :: index
character(len=*)           :: Counties_type
type(iols)                 :: iol
type(pspaces)              :: pspace



exec_procedure = "Basic-Param"
restrict       = .TRUE.
! allocate(counties(2))
!  counties = (/"Nordrhein-Westfalen","Rheinland-Pfalz    "/)
 Counties_type = "Character"


allocate(counties(16))
 counties = (/"Schleswig-Holstein    ","Hamburg               ",&
             "Niedersachsen         ","Bremen                ",&
             "Nordrhein-Westfalen   ","Hessen                ",&
             "Rheinland-Pfalz       ","Baden-Württemberg    ",&
             "Bayern                ","Saarland              ",&
             "Berlin                ","Brandenburg           ",&
             "Mecklenburg-Vorpommern","Sachsen               ",&
             "Sachsen-Anhalt        ","Thüringen            "/)

seed_date = "2020-03-16"
time_n    = 165
import_R0_matrix = .FALSE.
lhc_samples = 100
export_name = "v10.1-Germany-w.obs-0.5_by_country_by_state"
!!
!first version, set the array length manually
!!
! read data from file to tran_pr
open(12,file="./data/trans_probs_1.6death_3.0_icu.csv",access='sequential',form="formatted",iostat=k)
index = get_file_N(12)
! allocation for the readin variables
allocate(iol%transpr_age_gr(index-1))
allocate(iol%transpr_sex(index-1))
allocate(iol%transpr_surv_ill(index-1))
allocate(iol%transpr_icu_risk(index-1))
allocate(iol%transpr_surv_icu(index-1))
! read the first line(character)
read(12,*,iostat= k) iol%titel

do i = 1, index-1
  read(12,*,iostat=k) iol%transpr_age_gr(i),iol%transpr_sex(i),iol%transpr_surv_ill(i),&
                      iol%transpr_icu_risk(i),iol%transpr_surv_icu(i)
enddo
close(12)

!read data from file to pop
open(13,file="./data/181231_pop_age_sex_distr_ger.csv",access='sequential',form="formatted",iostat=k)
index = get_file_N(13)
! allocation for the readin variables
allocate(iol%pop_distid(index-1))
allocate(iol%pop_date(index-1))
allocate(iol%pop_sex(index-1))
allocate(iol%pop_age(index-1))
allocate(iol%pop_total(index-1))
! read the first line(character)
read(13,*,iostat= k) iol%titel

do i = 1,index-1
  read(13,*,iostat=k) iol%pop_distid(i),iol%pop_date(i),iol%pop_sex(i),&
                      iol%pop_age(i),iol%pop_total(i)
enddo
close(13)


!read data from file to seed
open(14,file="./data/cases.csv",access='sequential',form="formatted",iostat=k)
index = get_file_N(14)
! allocation for the readin variables
allocate(iol%seed_distid(index-1))
allocate(iol%seed_date(index-1))
allocate(iol%seed_cases(index-1))
! read the first line(character)
read(14,*,iostat= k) iol%seed_titel

do i = 1,index-1
  read(14,*,iostat=k) iol%seed_distid(i),iol%seed_date(i),iol%seed_cases(i)
enddo
close(14)

!read data from file to seeddeath

open(15,file="./data/deaths.csv",access='sequential',form="formatted",iostat=k)
index = get_file_N(15)
! allocation for the readin variables
allocate(iol%death_distid(index-1))
allocate(iol%death_date(index-1))
allocate(iol%death_cases(index-1))
! read the first line(character)
read(15,*,iostat= k) iol%seed_titel

do i = 1,size(iol%death_distid)
  read(15,*,iostat=k) iol%death_distid(i),iol%death_date(i),iol%death_cases(i)
enddo
close(15)


!read data from file to connect_total

open(16,file="./data/2019_comm_mat_tot_pop.csv",access='sequential',form="formatted",iostat=k)

read(16,*,iostat= k) iol%connect_titel,iol%connect_total_name(:)
do i = 1,size(iol%connect_total_distid)
  read(16,*,iostat=k) iol%connect_total_distid(i),iol%connect_total(:,i)
enddo
close(16)

!read data from file to connect_total

open(17,file="./data/2019_comm_mat_work_pop.csv",access='sequential',form="formatted",iostat=k)

read(17,*,iostat= k) iol%connect_titel,iol%connect_work_name(:)
do i = 1,size(iol%connect_work_distid)
  read(17,*,iostat=k) iol%connect_work_distid(i),iol%connect_work(:,i)
enddo
close(17)
!read data from file to connect_total
open(18,file="./data/states.csv",access='sequential',form="formatted",iostat=k)
index = get_file_N(18)
! allocation for the readin variables
allocate(iol%states_code(index-1))
allocate(iol%states_inhabitant(index-1))
allocate(iol%states_shortcut(index-1))
allocate(iol%states_name(index-1))
read(18,*,iostat= k) iol%state_titel(:)
do i = 1,index-1 
  read(18,*,iostat=k) iol%states_code(i),iol%states_inhabitant(i),iol%states_shortcut(i),iol%states_name(i)
enddo
close(18)

open(19,file="./data/counties.csv",access='sequential',form="formatted",iostat=k)
index = get_file_N(19)
! allocation for the readin variables
allocate(iol%counties_dist_id(index-1))
allocate(iol%counties_name(index-1))
allocate(iol%counties_area(index-1))
allocate(iol%counties_inhabitants(index-1))
read(19,*,iostat= k) iol%state_titel(:)
do i = 1,index-1
  read(19,*,iostat=k) iol%counties_dist_id(i),iol%counties_name(i),iol%counties_area(i),iol%counties_inhabitants(i)
enddo
close(19)


!!!!----initalizing pspace
!sam_size
pspace%Ps_scalar_list(1)%param                 = 1000000
pspace%Ps_scalar_list(1)%var_type              = "direct"
pspace%Ps_scalar_list(1)%name                  = "same_size"
!R0
pspace%Ps_scalar_list(2)%param                 = 2.5
pspace%Ps_scalar_list(2)%var_type              = "direct"
pspace%Ps_scalar_list(2)%name                  = "R0"
!icu_dur
pspace%Ps_scalar_list(3)%param                 = 14
pspace%Ps_scalar_list(3)%var_type              = "direct"
pspace%Ps_scalar_list(3)%name                  = "icu_dur"
!mod_surv_ill
pspace%Ps_scalar_list(4)%param                 = 1
pspace%Ps_scalar_list(4)%var_type              = "direct"
pspace%Ps_scalar_list(4)%name                  = "mod_surv_ill"
!lcokdown_effect
pspace%Ps_scalar_list(5)%param                 = 0.39
pspace%Ps_scalar_list(5)%var_type              = "direct"
pspace%Ps_scalar_list(5)%name                  = "lcokdown_effect"
!w_int
pspace%Ps_scalar_list(6)%param                 = 0.9
pspace%Ps_scalar_list(6)%var_type              = "direct"
pspace%Ps_scalar_list(6)%name                  = "w_int"
!w_obs
pspace%Ps_scalar_list(7)%param                 = 0.5
pspace%Ps_scalar_list(7)%var_type              = "direct"
pspace%Ps_scalar_list(7)%name                  = "w_obs"
!w_obs_by_state
pspace%Ps_scalar_list(8)%param                 = 1
pspace%Ps_scalar_list(8)%var_type              = "direct"
pspace%Ps_scalar_list(8)%name                  = "w_obs_by_state"
!ROeffect_ps
open(20,file="./data/200816_1.6death_3.0_icu_R0effect.ps1.w_int-0.9-18-24-0.6.csv",access='sequential',form="formatted",iostat=k)
index = get_file_N(20)

allocate(pspace%ROeffect_ps%param_char(17,index))
allocate(pspace%ROeffect_ps%param(16,index-1))
read(20,*,iostat = k)
do i = 1,index-1
    read(20,*,iostat = k) pspace%ROeffect_ps%param_char(:,i)
end do

do i = 1,index-1
    do j= 1,16
        read(pspace%ROeffect_ps%param_char(j+1,i),"(f20.4)") pspace%ROeffect_ps%param(j,i)
    end do
end do


 pspace%ROeffect_ps%var_type = "directl"
! convert character_param to numeric
! some code here, should be here

end subroutine


