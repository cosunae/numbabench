program rhop
 
  use omp_lib

  real(kind=8), allocatable, dimension (:,:,:) :: t, p, qv, qc, qi, rho
  integer:: isize, jsize, ksize
  integer :: i,j,k, cnt

  real :: pc_r_d, pc_rvd_o
  real(kind=8) :: startt, endt
  real(kind=8) :: mean

  pc_r_d = 287.05
  pc_rvd_o = 461.51 / pc_r_d - 1.0


  isize = 1024
  jsize = 1024
  ksize = 80
  allocate(t(isize, jsize, ksize))
  allocate(p(isize, jsize, ksize))
  allocate(qv(isize, jsize, ksize))
  allocate(qc(isize, jsize, ksize))
  allocate(qi(isize, jsize, ksize))
  allocate(rho(isize, jsize, ksize))

  do k=1, ksize
    do i=1, isize
      do j=1, jsize
        p(i,j,k) = 1+i
        t(i,j,k) = 0.1*i
        qv(i,j,k) = 0.02*j
        qc(i,j,k) = p(i,j,k)*1.1
        qi(i,j,k) = 0.3*i
        rho(i,j,k) = 1.1*k
      enddo
    enddo
  enddo
  mean = 0
  do cnt=1, 100
    startt = omp_get_wtime()
!$omp parallel do
    do k=1, ksize
      do i=2, isize-1
        do j=2, jsize-1
          rho(i,j,k) = p(i,j,k) * (t(i+1,j,k) + t(i-1,j,k) + t(i,j-1,k) + t(i,j+1,k))*0.25 
        enddo
      enddo
    enddo
!$omp end parallel do
    endt = omp_get_wtime()
    write(*,*) "KK" , endt-startt, rho(cnt,cnt,4)
    mean = mean + (endt-startt)
  enddo  

  write(*,*) "Mean ", mean/100.0
end program

!def rho(t, p, qv, qc, qi):
!  return p / ( pc_r_d * t * (1.0 + pc_rvd_o * qv - qc - qi) )


